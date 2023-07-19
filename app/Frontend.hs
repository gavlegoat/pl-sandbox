{-|
Module      : Frontend
Description : Collection of frontend functionality for the PL sandbox language.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : grega@reed.edu

This module collects all of the functionality from the frontend of the PL
sandbox compiler in order to present an easy interface to other parts of the
compiler. Specifically, this module is responsible for converting source code
to the core intermediate representation, including parsing, basic semantic
checks, and typechecking.
-}
module Frontend (toCore) where

import Data.ByteString.Lazy.Char8 (ByteString)

import Frontend.Lexer (runAlex)
import Frontend.Parser (parseMain)
import Frontend.Checks
import Frontend.Typecheck
import Frontend.Desugar
import qualified Core as C

-- | Represents any error which can be raised in the compiler front end.
data Error
  = ParseError String
  | SemanticError [ByteString]
  | TypeError [Constraint]
  deriving (Show)

-- | Convert source code to a core program.
toCore :: ByteString -> Either Error C.Program
toCore src =
  case runAlex src parseMain of
    Left err -> Left $ ParseError err
    Right prog -> case runChecks prog of
      [] -> case inferTop prog of
        Left errs -> Left $ TypeError errs
        Right typed -> Right $ desugar typed
      errs -> Left $ SemanticError errs
