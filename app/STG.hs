{-|
Module      : STG
Description : STG language definitions
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : ganderso@cs.utexas.edu

This module contains definitions for the language used by the spinless, tagless
G-machine.
-}
module STG
  where

import Data.ByteString.Lazy.Char8 (ByteString)

-- This grammar is taken directly from the STG paper v2.5 (Payton Jones, 1992)

data Binding = Binding Var Lf

data Lf = Lf [Var] Upd [Var] Expr

data Upd = U | N

data Expr = ELet [Binding] Expr
          | ELetRec [Binding] Expr
          | ECase Expr Alternatives
          | EVar Var [Atom]
          | EConstr Constr [Atom]
          | EPrimitive PrimitiveOp [Atom]
          | ELiteral Literal

data Alternatives = Algebraic [AAlt] Default
                  | Primitive [PAlt] Default

data AAlt = AAlt Constr [Var] Expr

data PAlt = PAlt Literal Expr

data Default = BoundDefault Var Expr
             | UnboundDefault Expr

data Literal = LInt Int
             | LString String

data PrimitiveOp = Plus
                 | Minus
                 | Times
                 | Divide
                 | Lt
                 | Le
                 | Eq
                 | Neq
                 | Ge
                 | Gt
                 | And
                 | Or
                 | Not
                 | Negate

newtype Var = Var ByteString

data Atom = AVar Var
          | ALit Literal

newtype Constr = Constr ByteString
