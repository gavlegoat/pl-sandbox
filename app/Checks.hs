{-
Module      : Checks
Description : Random semantic checks for the PL sandbox language.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : grega@reed.edu
-}
module Checks
  ( runChecks
  ) where

import Source

runChecks :: [Binding a] -> [String]
runChecks prog = [patternRepeats] <*> return prog

-- | Check for repeated variables in pattern matches.
patternRepeats :: [Binding a] -> [String]
patternRepeats = undefined
