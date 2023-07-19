{-# LANGUAGE OverloadedStrings #-}

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

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Source

runChecks :: Program a -> [ByteString]
runChecks prog = concatMap ($ prog) [patternRepeats]

-- | Check for repeated variables in pattern matches.
patternRepeats :: Program a -> [ByteString]
patternRepeats = patternRepeats' . pValues where
  patternRepeats' [] = []
  patternRepeats' (Binding _ _ _ ex : bs) = expr ex ++ patternRepeats' bs
  expr (EApp _ e1 e2) = expr e1 ++ expr e2
  expr (EBinop _ e1 _ e2) = expr e1 ++ expr e2
  expr (EUnop _ _ e) = expr e
  expr (EIf _ ce te fe) = expr ce ++ expr te ++ expr fe
  expr (ECase _ e as) = expr e ++ concatMap alternative as
  expr (ELambda _ _ e) = expr e
  expr (ELet _ _ v b) = expr v ++ expr b
  expr _ = []
  alternative (Alternative _ p e) = snd (pattern [] p) ++ expr e
  pattern vs (PConstr _ _ ps) =
    foldr (\p (vars, errs) -> let (nvs, nes) = pattern vars p
                               in (nvs, errs ++ nes))
          (vs, []) ps
  pattern vs (PVar _ v) =
    if elem v vs
       then (vs, [BS.concat ["Variable ", v, " repeated in pattern"]])
       else (v : vs, [])
  pattern vs PConst{} = (vs, [])
