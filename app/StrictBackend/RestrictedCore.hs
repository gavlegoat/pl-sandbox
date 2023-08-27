{-|
Module      : StrictBackend.RestrictedCore
Description : A restricted form of Core used by the strict backend.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : grega@reed.edu

This restricted form is based on
https://www.cse.chalmers.se/edu/year/2011/course/CompFun/lecture2.pdf
-}

module StrictBackend.RestrictedCore
  ( Program (..)
  , Binding (..)
  , Var (..)
  , Expr (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

newtype Program = Program [Binding]

data Binding
  = Function [Arg] Body
  | Object Constructor [Expr]
  | Value Arg Expr

type Arg = ByteString

data Body
  = Let [Binding] Body
  | Case Arg [Alternative]
  | Expr

type Constructor = ByteString

data Expr
  = undefined

data Alternative
  = NullaryC Constructor Body
  | ArgC Constructor [Arg] Body
  | LitC Literal Body

data Literal
  = undefined
