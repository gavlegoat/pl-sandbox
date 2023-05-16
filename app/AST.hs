{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : AST
Description : Abstract syntax trees
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : ganderso@cs.utexas.edu

This module contains basic type definitions for abstract syntax trees in the
PL sandbox language.
-}
module AST
  ( Constant (..)
  , Binop (..)
  , Unop (..)
  , Expr (..)
  , exprInfo
  , Alternative (..)
  , Pattern (..)
  , patternInfo
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

-- | Constants
data Constant a
  = CInt a Integer       -- ^ Integer constants
  | CString a ByteString -- ^ String constants
  deriving (Show, Eq, Functor)

instance Foldable Constant where
  foldMap f (CInt i _) = f i
  foldMap f (CString i _) = f i

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
  | EConstr a ByteString                              -- ^ Constructors
  | EApp a (Expr a) (Expr a)                          -- ^ Function application
  | EBinop a (Expr a) (Binop a) (Expr a)              -- ^ Binary operator
  | EUnop a (Unop a) (Expr a)                         -- ^ Unary operator
  | EIf a (Expr a) (Expr a) (Expr a)                  -- ^ Conditional
  | ECase a (Expr a) [Alternative a]                  -- ^ Pattern match
  | ELambda a ByteString (Expr a)                     -- ^ Anonymous function
  | ELet a ByteString (Expr a) (Expr a)               -- ^ Let binding
  deriving (Show, Eq, Functor)

instance Foldable Expr where
  foldMap f expr = case expr of
    EVar i _ -> f i
    EConst i _ -> f i
    EConstr i _ -> f i
    EApp i e a -> f i <> foldMap f e <> foldMap f a
    EBinop i l _ r -> f i <> foldMap f l <> foldMap f r
    EUnop i _ a -> f i <> foldMap f a
    EIf i c t e -> f i <> foldMap f c <> foldMap f t <> foldMap f e
    ECase i e as -> f i <> foldMap f e <> foldMap (foldMap f) as
    ELambda i _ b -> f i <> foldMap f b
    ELet i _ e b -> f i <> foldMap f e <> foldMap f b

-- | Extract info from the top level of this expression
exprInfo :: Expr a -> a
exprInfo (EVar i _) = i
exprInfo (EConst i _) = i
exprInfo (EConstr i _) = i
exprInfo (EApp i _ _) = i
exprInfo (EBinop i _ _ _) = i
exprInfo (EUnop i _ _) = i
exprInfo (EIf i _ _ _) = i
exprInfo (ECase i _ _) = i
exprInfo (ELambda i _ _) = i
exprInfo (ELet i _ _ _) = i

data Alternative a
  = Alternative a (Pattern a) (Expr a)
  deriving (Show, Eq, Functor)

instance Foldable Alternative where
  foldMap f (Alternative i pat expr) = f i <> foldMap f pat <> foldMap f expr

data Pattern a
  = PConstr a ByteString [Pattern a]
  | PVar a ByteString
  | PConst a (Constant a)
  deriving (Show, Eq, Functor)

instance Foldable Pattern where
  foldMap f pat = case pat of
    PConstr i _ pats -> f i <> foldMap (foldMap f) pats
    PVar i _ -> f i
    PConst i c -> f i <> foldMap f c

patternInfo :: Pattern a -> a
patternInfo (PConstr i _ _) = i
patternInfo (PVar i _) = i
patternInfo (PConst i _) = i
