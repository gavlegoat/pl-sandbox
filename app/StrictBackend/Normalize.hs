{-|
Module      : StrictBackend.Normalize
Description : Convert a Core program to restricted form.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : grega@reed.edu

This module normalizes a Core program in order to obtain the restricted form
defined in StrictBackend.RestrictedCore.
-}

module StrictBackend.Normalize
  ( normalize
  ) where

import qualified Core as C
import qualified StrictBackend.RestrictedCore as R

normalize :: C.Program -> R.Program
normalize = undefined
