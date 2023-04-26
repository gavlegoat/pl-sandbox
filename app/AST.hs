{-# LANGUAGE DeriveFunctor #-}

module AST
  ( Constant (..)
  , Binop (..)
  , Unop (..)
  , Expr (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

-- | Constants
data Constant a
  = CInt a Integer       -- ^ Integer constants
  | CString a ByteString -- ^ String constants
  | CBool a Bool         -- ^ Boolean constants
  deriving (Show, Eq, Functor)

-- | Binary operators
data Binop a
  = OPlus a   -- ^ Addition
  | OMinus a  -- ^ Subtraction
  | OTimes a  -- ^ Multiplication
  | ODivide a -- ^ Division
  | OLt a     -- ^ Less than
  | OLe a     -- ^ Less or equal
  | OEq a     -- ^ Equal
  | ONeq a    -- ^ Not equal
  | OGe a     -- ^ Greater or equal
  | OGt a     -- ^ Greater than
  | OAnd a    -- ^ Boolean and
  | OOr a     -- ^ Boolean or
  deriving (Show, Eq, Functor)

-- | Unary operators
data Unop a
  = UNot a    -- ^ Boolean negation
  | UNegate a -- ^ Arithmetic negation
  deriving (Show, Eq, Functor)

-- | Expressions
data Expr a
  = EVar a ByteString                                 -- ^ Variable
  | EConst a (Constant a)                             -- ^ Constant
  | EApp a (Expr a) (Expr a)                          -- ^ Function application
  | EBinop a (Expr a) (Binop a) (Expr a)              -- ^ Binary operator
  | EUnop a (Unop a) (Expr a)                         -- ^ Unary operator
  | EIf a (Expr a) (Expr a) (Expr a)                  -- ^ Conditional
  | ELambda a ByteString (Expr a)                     -- ^ Anonymous function
  | ELet a ByteString (Expr a) (Expr a)               -- ^ Let binding
  deriving (Show, Eq, Functor)

