{-|
Module      : Type
Description : Types in the PL sandbox language
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : ganderso@cs.utexas.edu

This module defines the types which can be used in PL sandbox languages.
-}
module Type
  ( Type (..)
  , isLiteralType
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

-- | Types
data Type = TInt                    -- ^ Integer type
          | TString                 -- ^ String type
          | TBool                   -- ^ Boolean type
          | TVar Int                -- ^ Type variable
          | TArrow Type Type        -- ^ Function type
          | TForall Int Type        -- ^ Polymorphism
          | TConstr ByteString Type -- ^ Type constructor
          deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show TString = "String"
  show TBool = "Bool"
  show (TVar i) = "t" ++ show i
  show (TArrow t1 t2) = "(" ++ show t1 ++ ") -> (" ++ show t2 ++ ")"
  show (TForall i t) = "forall t" ++ show i ++ ". (" ++ show t ++ ")"
  show (TConstr name t) = "(" ++ show t ++ ") " ++ show name

-- | Determine whether a particular type is a literal.
--
-- Note that Bools are not considered literals because we imagine that
-- they are defined as data Bool = True | False
isLiteralType :: Type -> Bool
isLiteralType TInt = True
isLiteralType TString = True
isLiteralType _ = False
