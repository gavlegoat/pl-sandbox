{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Interpreter
Description : An interpreter for the PL sandbox language.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : ganderso@cs.utexas.edu
-}
module Interpreter
  ( interpret
  , emptyContext
  , Value(..)
  ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)

import AST

-- | A value context for interpretation, mapping names to values.
type Context = Map ByteString Value

-- | An empty value context.
emptyContext :: Context
emptyContext = M.empty

-- | The values an expression can have.
data Value = VInt Integer
           | VString ByteString
           | VConstr ByteString [Value]
           | VClosure Context (Expr ())

instance Show Value where
  show (VInt i) = show i
  show (VString s) = show s
  show (VConstr s as) = show s ++ " " ++ unwords (map show as)
  show (VClosure _ _) = "<closure>"

-- | Intepret an expression to produce a value.
interpret :: Context -> Expr a -> Value
interpret ctx expr = case expr of
  EVar _ name -> fromMaybe (error $ "unknown variable " ++ unpack name)
                           (M.lookup name ctx)
  EConst _ constant -> case constant of
    CInt _ i -> VInt i
    CString _ s -> VString s
  EConstr _ name -> VConstr name []
  EApp _ func arg -> case interpret ctx func of
    VConstr name vs -> let argval = interpret ctx arg
                        in VConstr name (vs ++ [argval])
    VClosure ctx' (ELambda _ name body) ->
      let ctx'' = M.union ctx' ctx
          argval = interpret ctx arg
       in interpret (M.insert name argval ctx'') body
    _ -> error "Trying to apply non-function object"
  EBinop _ left op right ->
    let lval = interpret ctx left
        rval = interpret ctx right
     in interpretBinop lval op rval
  EUnop _ op arg ->
    let aval = interpret ctx arg
     in interpretUnop op aval
  EIf _ cond true false -> case interpret ctx cond of
    VConstr "True" [] -> interpret ctx true
    VConstr "False" [] -> interpret ctx false
    _ -> error "Non-boolean condition in if-statement"
  ECase _ e alts ->
    let eval = interpret ctx e
     in interpretAlts ctx alts eval
  e@ELambda{} -> VClosure ctx $ void e
  ELet _ name value body ->
    let ctx' = M.insert name (interpret ctx value) ctx
     in interpret ctx' body

-- | Helper function for interpreting binary operations
interpretBinop :: Value -> Binop a -> Value -> Value
interpretBinop left op right = case op of
  OPlus _ -> case (left, right) of
    (VInt l, VInt r) -> VInt $ l + r
    _ -> error "Non-number arguments to +"
  OMinus _ -> case (left, right) of
    (VInt l, VInt r) -> VInt $ l - r
    _ -> error "Non-number arguments to -"
  OTimes _ -> case (left, right) of
    (VInt l, VInt r) -> VInt $ l * r
    _ -> error "Non-number arguments to *"
  ODivide _ -> case (left, right) of
    (VInt l, VInt r) -> VInt $ l `div` r
    _ -> error "Non-number arguments to /"
  OLt _ -> case (left, right) of
    (VInt l, VInt r) -> vBool $ l < r
    _ -> error "Non-number arguments to <"
  OLe _ -> case (left, right) of
    (VInt l, VInt r) -> vBool $ l <= r
    _ -> error "Non-number arguments to <="
  OEq _ -> case (left, right) of
    (VInt l, VInt r) -> vBool $ l == r
    _ -> error "Non-number arguments to =="
  ONeq _ -> case (left, right) of
    (VInt l, VInt r) -> vBool $ l /= r
    _ -> error "Non-number arguments to /="
  OGe _ -> case (left, right) of
    (VInt l, VInt r) -> vBool $ l >= r
    _ -> error "Non-number arguments to >="
  OGt _ -> case (left, right) of
    (VInt l, VInt r) -> vBool $ l > r
    _ -> error "Non-number arguments to >"
  OAnd _ -> case (left, right) of
    (VConstr "True"  [], VConstr "True"  []) -> VConstr "True"  []
    (VConstr "False" [], VConstr "True"  []) -> VConstr "False" []
    (VConstr "True"  [], VConstr "False" []) -> VConstr "False" []
    (VConstr "False" [], VConstr "False" []) -> VConstr "False" []
    _ -> error "Non-bool arguments to &&"
  OOr _ -> case (left, right) of
    (VConstr "True"  [], VConstr "True"  []) -> VConstr "True"  []
    (VConstr "False" [], VConstr "True"  []) -> VConstr "True"  []
    (VConstr "True"  [], VConstr "False" []) -> VConstr "True"  []
    (VConstr "False" [], VConstr "False" []) -> VConstr "False" []
    _ -> error "Non-bool arguments to ||"

vBool :: Bool -> Value
vBool True = VConstr "True" []
vBool False = VConstr "False" []

-- | Helper function for interpreting unary operations
interpretUnop :: Unop a -> Value -> Value
interpretUnop op arg = case op of
  UNot _ -> case arg of
    VConstr "True" [] -> VConstr "False" []
    VConstr "False" [] -> VConstr "True" []
    _ -> error "Non-bool argument to !"
  UNegate _ -> case arg of
    VInt a -> VInt $ - a
    _ -> error "Non-number argument to -"

interpretAlts :: Context -> [Alternative a] -> Value -> Value
interpretAlts _ [] _ = error "No alternative matched in case statement"
interpretAlts ctx (Alternative _ pat expr : as) val = case match pat val of
  Nothing -> interpretAlts ctx as val
  Just mp -> interpret (M.union mp ctx) expr
 where
    match :: Pattern a -> Value -> Maybe (Map ByteString Value)
    match (PConstr _ n1 args) v = case v of
      VConstr n2 vs ->
        if n1 == n2
           then let subs = zipWith match args vs
                 in if all isJust subs
                       then Just $ foldr (M.union . fromJust) M.empty subs
                       else Nothing
           else Nothing
      _ -> Nothing
    match (PVar _ var) v = Just $ M.singleton var v
    match (PConst _ c) v = case (v, c) of
      (VInt i1, CInt _ i2) | i1 == i2 -> Just M.empty
      (VString s1, CString _ s2) | s1 == s2 -> Just M.empty
      _ -> Nothing

