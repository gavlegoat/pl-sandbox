{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Source
Description : Abstract syntax trees
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : grega@reed.edu

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
module Frontend.Source
  ( Program (..)
  , Binding (..)
  , bindingName
  , bindingInfo
  , TypeDef (..)
  , Constructor (..)
  , Constant (..)
  , Binop (..)
  , Unop (..)
  , Expr (..)
  , exprInfo
  , Alternative (..)
  , Pattern (..)
  , patternInfo
  , occurs
  , groupAndSort
  , alphaEquiv
  , alphaSub
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Graph as Graph
import Data.Maybe (isJust)
import Control.Monad (zipWithM)

import Type

-- | A program consists of a set of type definitions and a set of value
-- bindings.
data Program a = Program { pValues :: [Binding a], pTypes :: [TypeDef a] }
  deriving (Show, Eq)

-- | A binding associates a name and list of arguments with a function body.
--
-- Note that the list of arguments may be empty in case any top-level bindings
-- are nullary values.
--
-- See the module description of "Source#info" for a discussion of the extra
-- type parameter.
data Binding a
  = Binding a ByteString [ByteString] (Expr a)
  deriving (Show, Eq, Functor)

-- | A `Foldable` instance for bindings.
--
-- See the module description of "Source#foldable" for a discussion of the
-- `Foldable` instance.
instance Foldable Binding where
  foldMap f (Binding i _ _ e) = f i <> foldMap f e

-- | Get the name bound by a binding.
bindingName :: Binding a -> ByteString
bindingName (Binding _ name _ _) = name

-- | Get the info associated with a binding.
bindingInfo :: Binding a -> a
bindingInfo (Binding i _ _ _) = i

-- | A type is defined by a set of constructors.
data TypeDef a = TypeDef a ByteString [Constructor a]
  deriving (Show, Eq)

-- | A `Foldable` instance for type definitions.
--
-- See the module description of "Source#foldable" for a discussion of the
-- `Foldable` instance.
instance Foldable TypeDef where
  foldMap f (TypeDef i _ cs) = f i <> foldMap (foldMap f) cs

-- | A constructor is one option for building a data type.
data Constructor a = Constructor a ByteString [Type]
  deriving (Show, Eq)

-- | A `Foldable` instance for constructors.
--
-- See the module description of "Source#foldable" for a discussion of the
-- `Foldable` instance.
instance Foldable Constructor where
  foldMap f (Constructor i _ _) = f i

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
-- `Foldable` instance.
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

-- | Determine whether there exists a susbstitution of variables which makes
-- two patterns equal.
alphaEquiv :: Eq a => Pattern a -> Pattern a -> Bool
alphaEquiv a b = isJust $ alphaSub a b

-- | Find a substitution which converts the first pattern to the second
-- pattern, if such a substitution exists.
alphaSub :: Eq a => Pattern a -> Pattern a -> Maybe [(ByteString, ByteString)]
alphaSub a b =
  case (a, b) of
    (PVar _ n, PVar _ m) -> Just [(n, m)]
    (PConstr _ n ps1, PConstr _ m ps2) ->
      if n == m
         then concat <$> zipWithM alphaSub ps1 ps2
         else Nothing
    (PConst _ c1, PConst _ c2) -> if c1 == c2 then Just [] else Nothing
    _ -> Nothing

-- | Determine whether a name occurs in an expression.
--
-- This decides whether bindings need to be recursive, so it also considers
-- shadowing. That is, if @name@ appears in @expr@, but only inside of a scope
-- where @name@ is rebound, then we don't consider that an occurance of @name@.
occurs :: ByteString -> Expr a -> Bool
occurs name expr = case expr of
  EVar _ n -> n == name
  EConst _ _ -> False
  EConstr _ _ -> False
  EApp _ f a -> occurs name f || occurs name a
  EBinop _ l _ r -> occurs name l || occurs name r
  EUnop _ _ a -> occurs name a
  EIf _ c t f -> occurs name c || occurs name t || occurs name f
  ECase _ e as -> occurs name e || any (occursAlt name) as
  -- Function definition and let introduce new bindings, so we check for
  -- shadowing in these cases.
  ELambda _ n b -> n /= name && occurs name b
  ELet _ n d b -> occurs name d || (n /= name && occurs name b)

-- | Determine whether a name occurs in an alternative in a case expression.
occursAlt :: ByteString -> Alternative a -> Bool
occursAlt name (Alternative _ pat expr) =
  -- If `name` appears in `pat` then it will be shadowed in `expr`
  not (occursPattern name pat) && occurs name expr

-- | Determine whether a name occurs in a pattern.
--
-- This is used to check for shadowing since if a name occurs in a pattern, the
-- same name from the outer scope can't be referenced in the corresponding case
-- alternative.
occursPattern :: ByteString -> Pattern a -> Bool
occursPattern name pat = case pat of
  PConstr _ _ pats -> any (occursPattern name) pats
  PVar _ n -> n == name
  PConst _ _ -> False

-- | Group bindings into mutually recursive sets then sort those sets by usage.
--
-- Each binding may refer to other bindings. In order to infer general types,
-- we must typecheck bindings before they are used. However, in the case of
-- mutually recursive bindings this is impossible. This function finds minimal
-- sets of mutually recurisve bindings which must be typechecked together,
-- then sorts these sets so that definitions are typed before they are used.
-- In graph terms, this is just the strongly connected components of the
-- dependency graph, sorted reverse topologically.
groupAndSort :: [Binding a] -> [[Binding a]]
groupAndSort binds =
  map Graph.flattenSCC $ Graph.stronglyConnComp dependencyGraph
 where
   dependencyGraph = map getEdges binds
   getEdges b@(Binding _ name args expr) =
     (b, name, filter (\n -> notElem n args && occurs n expr) bindingNames)
   bindingNames = map bindingName binds
