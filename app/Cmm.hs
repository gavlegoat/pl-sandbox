{-|
Module      : Cmm
Description : The Cmm intermediate representation for PL sandbox.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : grega@reed.edu

This module contains type definitions for C--, a low-level intermediate
representation used by the PL sandbox language. C-- is a small subset of C,
encompassing only the elements needed to represent PL sandbox structures.
-}
module Cmm
  ( Program (..)
  , Declaration (..)
  , Arg (..)
  , Body (..)
  ) where

-- | A C-- program.
newtype Program = Program [Declaration]

-- | A declaration in a C-- program
data Declaration
  = Function [Arg] Body
  | Arg Body
  | Block [Declaration]

data Arg

data Body
