{

module Parser
  ( parseMain
  , Info(..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (intercalate)

import qualified Lexer as Lex
import Source

}

%expect 0

%name parseMain start
%tokentype { Lex.Token }
%error { parseError }
%errorhandlertype explist
%monad { Lex.Alex } { >>= } { pure }
%lexer { lexer } { Lex.EOF }

%left '||'
%left '&&'
%nonassoc '<' '<=' '==' '/=' '>=' '>'
%left '+' '-'
%left '*' '/'

%token
  -- Keywords
  let   { Lex.Let }
  in    { Lex.In }
  if    { Lex.If }
  then  { Lex.Then }
  else  { Lex.Else }
  case  { Lex.Case }
  of    { Lex.Of }
  -- Parens
  '(' { Lex.LParen }
  ')' { Lex.RParen }
  -- Operators
  --  Arithmetic
  '+' { Lex.Plus }
  '-' { Lex.Minus }
  '*' { Lex.Times }
  '/' { Lex.Divide }
  --   Comparison
  '<'  { Lex.Lt }
  '<=' { Lex.Le }
  '==' { Lex.Eq }
  '/=' { Lex.Neq }
  '>=' { Lex.Ge }
  '>'  { Lex.Gt }
  --   Logical
  '&&' { Lex.And }
  '||' { Lex.Or }
  '!'  { Lex.Not }
  -- Data-carrying tokens
  id     { Lex.Identifier _ }
  constr { Lex.Constructor _ }
  int    { Lex.Integer _ }
  string { Lex.String _ }
  -- Other tokens
  '\\' { Lex.Lambda }
  '->' { Lex.Arrow }
  '='  { Lex.Defn }
  ';'  { Lex.Semi }

%%

start :: { [Binding Info] }
  : bindings         { reverse $1 }
  | bindings ';' ';' { reverse $1 }

bindings :: { [Binding Info] }
  : binding                  { [$1] }
  | bindings ';' ';' binding { $4 : $1 }

binding :: { Binding Info }
  : vars '=' expr { let v = reverse $1 in Binding () (head v) (tail v) $3 }

vars :: { [ByteString] }
  : id      { [getId $1] }
  | vars id { getId $2 : $1 }

-- NOTE: Expression parsing is heavily influenced by GHC. See
-- github.com/ghc/ghc/compiler/GHC/Parser.y
-- Splitting up the expression rules this way resolves a lot of ambiguity in
-- the grammar

-- Top-level expressions may have optional, left-associative infix operators
expr :: { Expr Info }
  : expr '+' expr  { EBinop () $1 (OPlus ()) $3 }
  | expr '-' expr  { EBinop () $1 (OMinus ()) $3 }
  | expr '*' expr  { EBinop () $1 (OTimes ()) $3 }
  | expr '/' expr  { EBinop () $1 (ODivide ()) $3 }
  | expr '<' expr  { EBinop () $1 (OLt ()) $3 }
  | expr '<=' expr { EBinop () $1 (OLe ()) $3 }
  | expr '==' expr { EBinop () $1 (OEq ()) $3 }
  | expr '/=' expr { EBinop () $1 (ONeq ()) $3 }
  | expr '>=' expr { EBinop () $1 (OGe ()) $3 }
  | expr '>' expr  { EBinop () $1 (OGt ()) $3 }
  | expr '&&' expr { EBinop () $1 (OAnd ()) $3 }
  | expr '||' expr { EBinop () $1 (OOr ()) $3 }
  | prefix_expr    { $1 }

-- Expressions with optional prefix operators
-- All of these constructors shift because they should bind less tightly than
-- function application.
prefix_expr :: { Expr Info }
  : '!' fexpr %shift { EUnop () (UNot ()) $2 }
  | '-' fexpr %shift { EUnop () (UNegate ()) $2 }
  | fexpr %shift     { $1 }

-- Expressions with optional function application
fexpr :: { Expr Info }
  : fexpr '(' expr ')' { EApp () $1 $3 }
  | fexpr base_expr    { EApp () $1 $2 }
  | aexpr              { $1 }

aexpr :: { Expr Info }
  : if expr then expr else expr %shift { EIf () $2 $4 $6 }
  | case expr of alts %shift           { ECase () $2 (reverse $4) }
  | let id '=' expr in expr %shift     { ELet () (getId $2) $4 $6 }
  | '\\' id '->' expr %shift           { ELambda () (getId $2) $4 }
  | base_expr                          { $1 }

base_expr :: { Expr Info }
  : id       { EVar () (getId $1) }
  | constr   { EConstr () (getId $1) }
  | constant { EConst () $1 }

constant :: { Constant Info }
  : int    { CInt () (getInt $1) }
  | string { CString () (getString $1) }

alts :: { [Alternative Info] }
  : alt      { [$1] }
  | alts alt { $2 : $1 }

alt :: { Alternative Info }
  : pattern '->' expr ';' { Alternative () $1 $3 }

pattern :: { Pattern Info }
  : constr patterns %shift { PConstr () (getId $1) (reverse $2) }
  | '(' pattern ')'        { $2 }
  | id                     { PVar () (getId $1) }
  | constant               { PConst () $1 }

patterns :: { [Pattern Info] }
  : {- empty -}      { [] }
  | patterns pattern { $2 : $1 }

{

type Info = ()

parseError :: (Lex.Token, [String]) -> Lex.Alex a
parseError (_, exp) = do
  (Lex.AlexPn _ line col, _, _, _) <- Lex.alexGetInput
  Lex.alexError $ "Parse error at " <> show line <> ":" <> show col <> ". Expected tokens: " <> intercalate ", " exp

lexer :: (Lex.Token -> Lex.Alex a) -> Lex.Alex a
lexer = (Lex.alexMonadScan >>=)

getId :: Lex.Token -> ByteString
getId (Lex.Identifier id) = id
getId (Lex.Constructor id) = id
getId _ = error "Impossible error: Tried to extract an id from a non-id token"

getInt :: Lex.Token -> Int
getInt (Lex.Integer i) = i
getInt _ = error "Impossible error: Tried to extract an int from a non-int token"

getString :: Lex.Token -> ByteString
getString (Lex.String bs) = bs
getString _ = error "Impossible error: Tried to extract a string from a non-string token"

}
