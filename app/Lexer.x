{
module Lexer
  ( Alex
  , AlexPosn (..)
  , Token (..)
  , alexGetInput
  , alexError
  , alexMonadScan
  , runAlex
  ) where

import Data.ByteString.Lazy.Char8 as BS
import Control.Monad (when)
}

%wrapper "monadUserState-bytestring"

@id = [a-z_] [0-9a-zA-Z_]*

@integer = [0-9]+

tokens :-

<0> $white+ ;

-- Comments
-- single line comments
<0>       "--" .*$ ;
-- multi-line comments
<0>       "{-" { increaseCommentDepth `andBegin` comment }
<0>       "-}" { \_ _ -> alexError "Error: end comment token outside comment" }
<comment> "{-" { increaseCommentDepth }
<comment> "-}" { decreaseCommentDepth }
<comment> \n   ;
<comment> .    ;

-- Strings
<0>      \"   { startString `andBegin` string }
<string> \"   { endString `andBegin` 0 }
<string> \\\\ { stringEscapeChar '\\' }
<string> \\\" { stringEscapeChar '"' }
<string> \\n  { stringEscapeChar '\n' }
<string> \\t  { stringEscapeChar '\t' }
<string> .    { stringNormalChar }

-- Literals
<0> @integer { tokInt }
<0> "true"   { tok TTrue }
<0> "false"  { tok FFalse }

-- Keywords
<0> let   { tok Let }
<0> in    { tok In }
<0> if    { tok If }
<0> then  { tok Then }
<0> else  { tok Else }

-- Parens
<0> "(" { tok LParen }
<0> ")" { tok RParen }

-- Operators
--   Arithmetic
<0> "+" { tok Plus }
<0> "-" { tok Minus }
<0> "*" { tok Times }
<0> "/" { tok Divide }
--   Comparison
<0> "<"  { tok Lt }
<0> "<=" { tok Le }
<0> "==" { tok Eq }
<0> "/=" { tok Neq }
<0> ">=" { tok Ge }
<0> ">"  { tok Gt }
--   Logical
<0> "&&" { tok And }
<0> "||" { tok Or }
<0> "!"  { tok Not }

-- Other tokens
<0> \\   { tok Lambda }
<0> "->" { tok Arrow }
<0> "="  { tok Defn}

<0> @id { tokId }

{

data Token
  -- Keywords
  = Let
  | In
  | If
  | Then
  | Else
  -- Parens
  | LParen
  | RParen
  -- Operators
  --   Arithmetic
  | Plus
  | Minus
  | Times
  | Divide
  --   Comparison
  | Lt
  | Le
  | Eq
  | Neq
  | Ge
  | Gt
  --   Logical
  | And
  | Or
  | Not
  -- Literals
  | Integer Integer
  | String ByteString
  | TTrue
  | FFalse
  -- Variables
  | Identifier ByteString
  -- Other tokens
  | Lambda
  | Arrow
  | Defn
  | EOF

data AlexUserState = AlexUserState
  { commentDepth :: Int
  , stringBuffer :: ByteString
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { commentDepth = 0
  , stringBuffer = BS.empty
  }

-- The get and set functions are only defined by alex for the String version
-- of the monad wrapper, so they are reproduced here
alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ust = Alex $ \s -> Right (s{alex_ust=ust}, ())

increaseCommentDepth :: AlexAction Token
increaseCommentDepth i l = do
  ust <- alexGetUserState
  alexSetUserState ust { commentDepth = (commentDepth ust) + 1 }
  skip i l

decreaseCommentDepth :: AlexAction Token
decreaseCommentDepth i l = do
  ust <- alexGetUserState
  let depth = commentDepth ust - 1
  alexSetUserState ust { commentDepth = depth }
  when (depth == 0) $ alexSetStartCode 0
  skip i l

startString :: AlexAction Token
startString i l = do
  ust <- alexGetUserState
  alexSetUserState ust { stringBuffer = BS.empty }
  skip i l

endString :: AlexAction Token
endString _ _ = do
  ust <- alexGetUserState
  pure $ String $ BS.reverse (stringBuffer ust)

stringEscapeChar :: Char -> AlexAction Token
stringEscapeChar c i l = do
  ust <- alexGetUserState
  let str = stringBuffer ust
  alexSetUserState ust { stringBuffer = BS.cons' c str }
  skip i l

stringNormalChar :: AlexAction Token
stringNormalChar i@(_, _, s, _) l = do
  ust <- alexGetUserState
  let str = stringBuffer ust
  let c = BS.index s (l - 1)
  alexSetUserState ust { stringBuffer = BS.cons' c str }
  skip i l

alexEOF :: Alex Token
alexEOF = do
  code <- alexGetStartCode
  when (code == comment) $ alexError "Error: unclosed comment at end of file"
  when (code == string) $ alexError "Error: EOF while scanning string"
  pure EOF

tok :: Token -> AlexAction Token
tok t _ _ = pure t

tokId :: AlexAction Token
tokId (_, _, s, _) l = pure $ Identifier $ BS.take l s

tokInt :: AlexAction Token
tokInt (_, _, s, _) l = pure $ Integer $ read $ BS.unpack $ BS.take l s

}
