{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Source
Description : Abstract syntax trees
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : ganderso@cs.utexas.edu

This module contains basic type definition for abstract syntax tress in the
PL sandbox language.

#info#
The types used to define the PL sandbox source language are all parameterized
over a type variable @a@. Each constructor includes a field of type @a@ which
is used to carry different information at different stages of compilation. For
example, the information may initially keep track of locations in the source
file, allowing the compiler to generate more useful error messages. Later on,
this type parameter may carry type information during typechecking and
desugaring.

#foldable#
Many of the types in this module also declare a `Foldable` instance. The fold
definitions here combine the information carried in the @a@ field (using a
preorder traversal). The foldable instances are designed to allow easily
checking some kind of condition over a syntax tree. As a concrete example, in
some places we need to repeatedly apply a transformation until we reach a fixed
point. In this case, we have the operation use the @a@ field in each
constructor to mark modified nodes, and then we know we can stop as soon as
@or tree@ becomes false where @tree@ is the modified-and-marked tree.
-}
module Source
  ( Binding (..)
  , bindingName
  , Constant (..)
  , Binop (..)
  , Unop (..)
  , Expr (..)
  , exprInfo
  , Alternative (..)
  , Pattern (..)
  , patternInfo
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

-- | A binding associates a name and list of arguments with a function body.
--
-- Note that the list of arguments may be empty in case any top-level bindings
-- are nullary values.
--
-- See the module description of "Source#info" for a discussion of the extra
-- type parameter.
data Binding a = Binding a ByteString [ByteString] (Expr a)
  deriving (Show, Eq, Functor)

-- | A `Foldable` instance for bindings.
--
-- See the module description of "Source#foldable" for a discussion of the
-- `Foldable` instance..
instance Foldable Binding where
  foldMap f (Binding i _ _ e) = f i <> foldMap f e

-- | Get the name bound by a binding.
bindingName :: Binding a -> ByteString
bindingName (Binding _ name _ _) = name

-- | Constants in the source language.
--
-- See the module description of "Source#info" for a discussion of the extra
-- type parameter.
data Constant a
  = CInt a Int           -- ^ Integer constants
  | CString a ByteString -- ^ String constants
  deriving (Show, Eq, Functor)

-- | A `Foldable` instance for constants.
--
-- See the module description of "Source#foldable" for a discussion of the
-- `Foldable` instance..
instance Foldable Constant where
  foldMap f (CInt i _) = f i
  foldMap f (CString i _) = f i

-- | Binary operators.
--
-- See the module description of "Source#info" for a discussion of the extra
-- type parameter.
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

-- | Unary operators.
--
-- See the module description of "Source#info" for a discussion of the extra
-- type parameter.
data Unop a
  = UNot a    -- ^ Boolean negation
  | UNegate a -- ^ Arithmetic negation
  deriving (Show, Eq, Functor)

-- | Expressions.
--
-- See the module description of "Source#info" for a discussion of the extra
-- type parameter.
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

-- | A `Foldable` instance for expressions.
--
-- See the module description of "Source#foldable" for a discussion of the
-- `Foldable` instance..
instance Foldable Expr where
  foldMap f expr = case expr of
    EVar i _ -> f i
    EConst i c -> f i <> foldMap f c
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

-- | Alternatives for case expressions.
--
-- Each alternative consists of a pattern to match and an expression to use
-- if the associated pattern matches.
--
-- See the module description of "Source#info" for a discussion of the extra
-- type parameter.
data Alternative a
  = Alternative a (Pattern a) (Expr a)
  deriving (Show, Eq, Functor)

-- | A `Foldable` instance for alternatives.
--
-- See the module description of "Source#foldable" for a discussion of the
-- `Foldable` instance..
instance Foldable Alternative where
  foldMap f (Alternative i pat expr) = f i <> foldMap f pat <> foldMap f expr

-- | Patterns for pattern matching.
--
-- See the module description of "Source#info" for a discussion of the extra
-- type parameter.
data Pattern a
  = PConstr a ByteString [Pattern a]
  | PVar a ByteString
  | PConst a (Constant a)
  deriving (Show, Eq, Functor)

-- | A `Foldable` instance for patterns.
--
-- See the module description of "Source#foldable" for a discussion of the
-- `Foldable` instance..
instance Foldable Pattern where
  foldMap f pat = case pat of
    PConstr i _ pats -> f i <> foldMap (foldMap f) pats
    PVar i _ -> f i
    PConst i c -> f i <> foldMap f c

-- | Extract information from the top level of a pattern.
patternInfo :: Pattern a -> a
patternInfo (PConstr i _ _) = i
patternInfo (PVar i _) = i
patternInfo (PConst i _) = i
