{-|
Module      : Core
Description : The core intermediate representation for PL sandbox.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : grega@reed.edu

This module contains type definitions for the small core language used by
the PL sandbox language. This language is taken directly from GHC, though
the types here are simpler and omit a lot of the useful extra information
carried by the GHC version for simplicity.
-}
module Core
  ( Program (..)
  , TypeDef (..)
  , Constructor (..)
  , Binding (..)
  , Expr (..)
  , typeof
  , Atom (..)
  , Literal (..)
  , Alts (..)
  , CAlt (..)
  , LAlt (..)
  , Default (..)
  , Var (..)
  , TyVar
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

import Type

-- | A Program contains a set of type definitions and a set of value bindings.
data Program = Program { pTypes :: [TypeDef], pValues :: [Binding] }

instance Show Program where
  show (Program {pTypes = ts, pValues = vs}) =
    unlines (map show ts) ++ "\n" ++ unlines (map show vs)

-- | A type definition is a set of constructors.
data TypeDef = TypeDef ByteString [Constructor]
  deriving (Eq)

instance Show TypeDef where
  show (TypeDef n cs) = "data " ++ show n ++ " =\n" ++
    unlines (map (("  | " ++) . show) cs)

-- | A constructor builds a user-defined type from a set of other types.
data Constructor = Constructor ByteString [Type]
  deriving (Eq)

instance Show Constructor where
  show (Constructor n ts) =
    show n ++ " " ++ unwords (map (\t -> "(" ++ show t ++ ")") ts)

-- | Associate a variable with some value.
data Binding
  = BNonRec Var Expr   -- ^ Non-recursive binding.
  | BRec [(Var, Expr)] -- ^ A set of mutually recursive bindings.
  deriving (Eq)

instance Show Binding where
  show (BNonRec v e) = show v ++ " = " ++ show e
  show (BRec bs) =
    let showOne (v, e) = show v ++ " = " ++ show e
     in "{" ++ unlines (map ((++ ";") . showOne) bs) ++ "}"

-- | Type of expressions in the core language.
data Expr
  = EApp Type Expr Expr     -- ^ Value application.
  | ETyApp Type Expr Type   -- ^ Type application.
  | ELambda Type Var Expr   -- ^ Function definition.
  | EBigLam Type TyVar Expr -- ^ Type function definition (polymorphism).
  | ECase Type Expr Alts    -- ^ Case statements.
  | ELet Type Binding Expr  -- ^ Local bindings.
  | EConstr Type ByteString -- ^ Constructors.
  | EAtom Type Atom         -- ^ Base expressions, variables and literals.
  deriving (Eq)

instance Show Expr where
  show (EApp _ f a) = "(" ++ show f ++ ") (" ++ show a ++ ")"
  show (ETyApp _ f t) = "(" ++ show f ++ ") (" ++ show t ++ ")"
  show (ELambda _ n b) = "\\" ++ show n ++ " -> (" ++ show b ++ ")"
  show (EBigLam _ n b) = "/\\" ++ show n ++ " => (" ++ show b ++ ")"
  show (ECase _ e alts) = "case " ++ show e ++ " of\n" ++ show alts
  show (ELet _ b e) = "let " ++ show b ++ "\n in (" ++ show e ++ ")"
  show (EConstr _ n) = show n
  show (EAtom _ a) = show a

-- | Get the type of a core expression.
typeof :: Expr -> Type
typeof (EApp ty _ _) = ty
typeof (ETyApp ty _ _) = ty
typeof (ELambda ty _ _) = ty
typeof (EBigLam ty _ _) = ty
typeof (ECase ty _ _) = ty
typeof (ELet ty _ _) = ty
typeof (EConstr ty _) = ty
typeof (EAtom ty _) = ty

-- | Base-level expressions, variables and literals.
data Atom = AVar Var | ALit Literal
  deriving (Eq)

instance Show Atom where
  show (AVar v) = show v
  show (ALit l) = show l

-- | Concrete values of a base type.
data Literal = LInt Int | LString ByteString
  deriving (Eq)

instance Show Literal where
  show (LInt i) = show i
  show (LString s) = "\"" ++ show s ++ "\""

-- | Alternatives for a case statement.
--
-- Alternatives are split into either a set of alternatives which match
-- constructor patterns, or a set of alternatives matching literal patterns.
-- Each set of alternatives also comes with an optional default branch.
data Alts = CAlts [CAlt] Default
          | LAlts [LAlt] Default
          deriving (Eq)

instance Show Alts where
  show (CAlts as d) = unlines (map (("| " ++) . show) as) ++ "\n| " ++ show d
  show (LAlts as d) = unlines (map (("| " ++) . show) as) ++ "\n| " ++ show d

-- | Alternatives matching constructor patterns.
--
-- Note that the arguments to the constructor are only matched by patterns. Any
-- nested patterns will be flattened into nested case statements in the
-- desugaring pass.
data CAlt = CAlt ByteString [Var] Expr
  deriving (Eq)

instance Show CAlt where
  show (CAlt c vs e) = unwords (show c : map show vs) ++ " -> " ++ show e

-- | Alternatives matching literals.
data LAlt = LAlt Literal Expr
  deriving (Eq)

instance Show LAlt where
  show (LAlt l e) = show l ++ " -> " ++ show e

-- | Default values for case statements.
data Default = NoDefault | Default Var Expr
  deriving (Eq)

instance Show Default where
  show NoDefault = ""
  show (Default v e) = show v ++ " -> " ++ show e

-- | Variables.
--
-- Variables in the core language are always typed.
data Var = Var ByteString Type
  deriving (Eq)

instance Show Var where
  show (Var n t) = show n ++ " : " ++ show t

-- | Type variables.
type TyVar = Int
