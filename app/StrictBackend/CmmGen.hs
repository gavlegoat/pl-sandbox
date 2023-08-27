{-|
Module      : StrictBackend.CmmGen
Description : Generate Cmm code from core.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : grega@reed.edu
-}
module StrictBackend.CmmGen
  ( genCmm
  ) where

import qualified StrictBackend.RestrictedCore as R
import qualified Cmm

-- | Generate C-- code from a Core program.
genCmm :: R.Program -> Cmm.Program
genCmm prog = Cmm.Program $ map (binding (pTypes prog)) (pValues prog)

-- | Generate C-- code for a single binding.
binding :: R.Binding -> Cmm.Declaration
binding ts (R.BNonRec v e) = getDec v e
binding ts (R.BRec bs) = Cmm.Block $ zipWith getDec bs

getDec :: R.Var -> R.Expr -> Cmm.Declaration
getDec = undefined
