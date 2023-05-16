{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Typecheck
Description : Type checking and inference for the PL sandbox language.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : ganderso@cs.utexas.edu
-}
module Typecheck
  ( inferTop
  ) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (ByteString)

import AST

-- | Types
data Type = TInt                    -- ^ Integer type
          | TString                 -- ^ String type
          | TBool                   -- ^ Boolean type
          | TVar Int                -- ^ Type variable
          | TArrow Type Type        -- ^ Function type
          | TConstr ByteString Type -- ^ Type constructor
          deriving (Show, Eq)

-- | Type constraint.
--
-- Each constraint imposes equality between two types.
type Constraint = (Type, Type)

-- | A state for typechecking
data Context = Context
  { constraints :: [Constraint]  -- ^ A set of type constraints
  , nextVar :: Int               -- ^ The next free variable index
  }

builtinTypes :: Map ByteString Type
builtinTypes = M.fromList [("True", TBool), ("False", TBool)]

inferTop :: [Binding a] -> Either [Constraint] [Binding Type]
inferTop bs =
  let l = length bs
      -- Give each binding a fresh type
      types = M.union builtinTypes $
                      M.fromList (zipWith (\b i -> (b, TVar i))
                                          (map bindingName bs) [0..l-1])
      initContext = Context { constraints = [], nextVar = l }
      -- typecheck each binding in a context which includes all other bindings
      (tbs, st) = runState (mapM (annotateBinding types) bs) initContext
      (subs, errs) = resolveConstraints $ constraints st
   in if null errs
         then Right $ map (substitute subs) tbs
         else Left errs

annotateBinding :: Map ByteString Type -> Binding a -> State Context (Binding Type)
annotateBinding types (Binding _ name vars expr) = do
  fts <- replicateM (length vars) freshVar
  et <- annotate (M.union (M.fromList $ zip vars fts) types) expr
  let retTy = foldr TArrow (exprInfo et) fts
  unify (fromJust $ M.lookup name types) retTy
  return $ Binding retTy name vars et

-- | Add type annotations to each subexpression.
--
-- The annotations added by this function include many tyep variables which
-- are not resolved until later.
annotate :: Map ByteString Type -> Expr a -> State Context (Expr Type)
annotate types expr = case expr of
  EVar _ name -> case M.lookup name types of
    Nothing -> flip EVar name <$> freshVar
    Just t -> return $ EVar t name
  EConst _ (CInt _ int) -> return $ EConst TInt (CInt TInt int)
  EConst _ (CString _ str) -> return $ EConst TString (CString TString str)
  EConstr _ name -> case M.lookup name types of
    Nothing -> flip EConstr name <$> freshVar
    Just t -> return $ EConstr t name
  EApp _ func arg -> do
    f <- annotate types func
    a <- annotate types arg
    b <- freshVar
    unify (exprInfo f) (TArrow (exprInfo a) b)
    return $ EApp b f a
  EBinop _ left op right -> do
    l <- annotate types left
    r <- annotate types right
    let lt = leftType op
    let rt = rightType op
    unify (exprInfo l) lt
    unify (exprInfo r) rt
    let rest = resultTypeBinary op
    return $ EBinop rest l (typeBinop op) r
  EUnop _ op arg -> do
    a <- annotate types arg
    let at = argType op
    unify (exprInfo a) at
    let rt = resultTypeUnary op
    return $ EUnop rt (typeUnop op) a
  EIf _ cond true false -> do
    c <- annotate types cond
    t <- annotate types true
    f <- annotate types false
    unify (exprInfo c) TBool
    unify (exprInfo t) (exprInfo f)
    return $ EIf (exprInfo t) c t f
  ECase _ e alts -> do
    (pets, aets) <- mapAndUnzipM (annotateAlt types) alts
    unifyAll $ map patternInfo pets
    unifyAll $ map exprInfo aets
    et <- annotate types e
    unify (exprInfo et) (patternInfo $ head pets)
    return $ ECase (exprInfo (head aets)) et (zipWith buildAlt pets aets)
  ELambda _ name body -> do
    ft <- freshVar
    b <- annotate (M.insert name ft types) body
    return $ ELambda (TArrow ft $ exprInfo b) name b
  ELet _ name value body -> do
    -- Let bindings may be recursive, so we give the name a type while typing
    -- the value expression.
    ft <- freshVar
    v <- annotate (M.insert name ft types) value
    -- Then unify the type with whatever we ended up with
    unify ft (exprInfo v)
    b <- annotate (M.insert name (exprInfo v) types) body
    return $ ELet (exprInfo b) name v b

annotateAlt :: Map ByteString Type -> Alternative a
            -> State Context (Pattern Type, Expr Type)
annotateAlt types (Alternative _ pat expr) = do
  (pt, mp) <- annotatePattern types pat
  et <- annotate (M.union mp types) expr
  return (pt, et)

annotatePattern :: Map ByteString Type -> Pattern a
                -> State Context (Pattern Type, Map ByteString Type)
annotatePattern types pat = case pat of
  PConstr _ name pats -> do
    (pts, mps) <- mapAndUnzipM (annotatePattern types) pats
    ct <- case M.lookup name types of
            Nothing -> freshVar
            Just tm -> return tm
    unifyArgs ct $ map patternInfo pts
    return (PConstr ct name pts, foldr M.union M.empty mps)
  PVar _ name -> freshVar >>= \ft -> return (PVar ft name, M.singleton name ft)
  PConst _ (CInt _ i) -> return (PConst TInt (CInt TInt i), M.empty)
  PConst _ (CString _ s) ->
    return (PConst TString (CString TString s), M.empty)

buildAlt :: Pattern Type -> Expr Type -> Alternative Type
buildAlt p o = Alternative (exprInfo o) p o

-- | Create a fresh type variable.
freshVar :: State Context Type
freshVar = do
  varNum <- gets nextVar
  modify (\st -> st { nextVar = varNum + 1 })
  return $ TVar varNum

-- | Assert that two types should be the same.
unify :: Type -> Type -> State Context ()
unify t1 t2 = modify (\st -> st { constraints = (t1, t2) : constraints st })

unifyAll :: [Type] -> State Context ()
unifyAll (t : ts) = mapM_ (unify t) ts
unifyAll [] = return ()

unifyArgs :: Type -> [Type] -> State Context ()
unifyArgs _    [] = return ()
unifyArgs func as = unify func $ foldr1 TArrow as

-- | Get the type of the left argument of a binary operator.
leftType :: Binop a -> Type
leftType op = case op of
  OPlus _ -> TInt
  OMinus _ -> TInt
  OTimes _ -> TInt
  ODivide _ -> TInt
  OLt _ -> TInt
  OLe _ -> TInt
  OEq _ -> TInt
  ONeq _ -> TInt
  OGe _ -> TInt
  OGt _ -> TInt
  OAnd _ -> TBool
  OOr _ -> TBool

-- | Get the type of the right argument of a binary operator.
rightType :: Binop a -> Type
rightType = leftType  -- All operators so far have the same type on both sides

-- | Get the type of the result of a binary operator.
resultTypeBinary :: Binop a -> Type
resultTypeBinary op = case op of
  OPlus _ -> TInt
  OMinus _ -> TInt
  OTimes _ -> TInt
  ODivide _ -> TInt
  OLt _ -> TBool
  OLe _ -> TBool
  OEq _ -> TBool
  ONeq _ -> TBool
  OGe _ -> TBool
  OGt _ -> TBool
  OAnd _ -> TBool
  OOr _ -> TBool

-- | Annotate a binary operator with its result type.
typeBinop :: Binop a -> Binop Type
typeBinop op = fmap (const $ resultTypeBinary op) op

-- | Get the type of the argument to a unary operator.
argType :: Unop a -> Type
argType op = case op of
  UNot _ -> TBool
  UNegate _ -> TInt

-- | Get the result type of a unary operator.
resultTypeUnary :: Unop a -> Type
resultTypeUnary op = case op of
  UNot _ -> TBool
  UNegate _ -> TInt

-- | Annotate a unary operator with its result type.
typeUnop :: Unop a -> Unop Type
typeUnop op = fmap (const $ resultTypeUnary op) op

-- | Create a substitution map which resolve all type constraints.
--
-- The return value is a substitution along with a set of constraints which
-- could not be satisfied.
resolveConstraints :: [Constraint] -> (Map Int Type, [Constraint])
resolveConstraints = resolveConstraints' M.empty [] where
  resolveConstraints' types errs [] = (types, errs)
  resolveConstraints' types errs ((t1, t2) : cs) = case t1 of
    TInt -> case t2 of
      TInt -> resolveConstraints' types errs cs
      TVar m -> resolveConstraints' (M.insert m t1 types) errs cs
      _ -> resolveConstraints' types ((t1, t2) : errs) cs
    TString -> case t2 of
      TString -> resolveConstraints' types errs cs
      TVar m -> resolveConstraints' (M.insert m t1 types) errs cs
      _ -> resolveConstraints' types ((t1, t2) : errs) cs
    TBool -> case t2 of
      TBool -> resolveConstraints' types errs cs
      TVar m -> resolveConstraints' (M.insert m t1 types) errs cs
      _ -> resolveConstraints' types ((t1, t2) : errs) cs
    TVar n -> case t2 of
      -- We impose an arbitrary ordering on resolutions involving two variables
      -- to avoid cycles in the substitution graph
      TVar m | n < m -> resolveConstraints' (M.insert m t1 types) errs cs
      TVar m | n > m -> resolveConstraints' (M.insert n t2 types) errs cs
      TVar m | m == n -> resolveConstraints' types errs cs
      _ -> resolveConstraints' (M.insert n t2 types) errs cs
    TArrow arg body -> case t2 of
      TVar m -> resolveConstraints' (M.insert m t1 types) errs cs
      TArrow a b -> resolveConstraints' types errs ((a, arg) : (b, body) : cs)
      _ -> resolveConstraints' types ((t1, t2) : errs) cs
    TConstr name arg -> case t2 of
      TVar m -> resolveConstraints' (M.insert m t1 types) errs cs
      TConstr n2 a2 ->
        if name == n2
           then resolveConstraints' types errs ((arg, a2) : cs)
           else resolveConstraints' types ((t1, t2) : errs) cs
      _ -> resolveConstraints' types ((t1, t2) : errs) cs

-- | Replace type variables in an expression with their substitutions.
--
-- Note that this function iterates to a fixed point.
substitute :: (Functor f, Foldable f) => Map Int Type -> f Type -> f Type
substitute subs expr = let s = fmap substituteType expr
                           ts = fmap fst s
                        in if or (fmap snd s)
                              then substitute subs ts
                              else ts where
  -- substituteType returns both the modified type and a boolean indicating
  -- whether the type is changed. This helps us determine when we can stop
  -- repeating calls to substitute.
  substituteType :: Type -> (Type, Bool)
  substituteType t = case t of
    TVar n -> case M.lookup n subs of
                Nothing -> (t, False)
                Just t' -> (t', True)
    TArrow a b -> let (ta, ca) = substituteType a
                      (tb, cb) = substituteType b
                   in (TArrow ta tb, ca || cb)
    TConstr n a -> let (ta, ca) = substituteType a
                    in (TConstr n ta, ca)
    _ -> (t, False)
