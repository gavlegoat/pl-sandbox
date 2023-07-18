{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Desugar
Description : Translate the PL sandbox source language to the core language.
Copyright   : (c) Greg Anderson, 2023
License     : BSD3
Maintainer  : ganderso@cs.utexas.edu

This module converts a typed program in the source language into a program in
the core language. This does a number of relatively straightforward things,
like:

* converting if statements to case statements destructuring the boolean,
* replacing operators with normal function application,
* categorizing bindings as recursinge or non-recursive, and
* other minor syntactic transformations.

Additionally, this module is responsible for converting nested patterns in case
expressions to nested case expressions, because the core language only allows
matching on distinct constructors or literals.

Finally, desugaring adds type functions to make all polymorphism explicit so
that there are no free type variables. That is, given a polymorphic function
with a type like

@
    compose :: (b -> c) -> (a -> b) -> a -> c
    compose = \f \g \x. f (g x)
@

we introduce type-level lambdas to produce and quantification in the types to
produce

@
    compose' :: forall b c. (b -> c) -> forall a. (a -> b) -> a -> c
    compose' = /\b. /\c. \f. /\a. \g. \x. f (g x)
@

(Note that the name of the function doesn't actually change.) Type abstraction
is interleaved with value abstraction so that each type is bound as late as
possible. For example, in compose we need to bind b and c before any arguments
because the type of the first argument refers to both b and c. However, a is
bound after f because f does not depend on a.

All applications of polymorphic functions need to be augmented with type
applications as well, so that for example

@
    compose f g x
@

is replaced by

@
    compose' b c f a g x
@
-}

-- Special identifiers introduced by the desugarer:
--   p$0, p$1, ...: pattern matching variables

module Desugar
  ( desugar
  ) where

import Control.Arrow (second)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (nub, nubBy)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Source as S
import qualified Core as C
import Type

-- | Desugar a top-level program.
--
-- NOTE: All core bindings are currently recursive.
desugar :: [S.Binding Type] -> [C.Binding]
desugar binds = --[C.BRec $ map desugarBinding binds]
  let bss = S.groupAndSort binds
      desugared = map (map desugarBinding) bss
   in map C.BRec desugared

-- | Desugar a single binding.
desugarBinding :: S.Binding Type -> (C.Var, C.Expr)
desugarBinding (S.Binding ty name args expr) =
  let e = expandLambda ty args expr
      de = desugarExpr e
      pe = pruneBigLambdas Set.empty de
      ae = addTyApps pe
   in (C.Var name $ C.typeof ae, ae)
 where
   -- Convert a binding with arguments to a zero-argument binding to a lambda.
   -- Note that the type of a binding is already the type of the lambda.
   expandLambda _ [] e = e
   expandLambda t@(TArrow _ t2) (a : as) e =
     S.ELambda t a (expandLambda t2 as e)
   -- Free variables are all quantified in top level bindings, and we re-add
   -- big lambdas later, so we can just forget quantification here
   expandLambda (TForall _ t) as e = expandLambda t as e
   expandLambda _ _ _ = error "Internal error: ill-typed function in expand"

-- | Desugar an expression.
desugarExpr :: S.Expr Type -> C.Expr
desugarExpr expr = case expr of
  S.EVar ty name -> C.EAtom ty (C.AVar $ C.Var name ty)
  S.EConst ty constant ->
    C.EAtom ty (C.ALit $ case constant of
                           S.CInt _ i -> C.LInt i
                           S.CString _ s -> C.LString s)
  S.EConstr ty name -> C.EConstr ty name
  S.EApp _ func arg ->
    let f = desugarExpr func
     in C.EApp (case C.typeof f of
                  TArrow _ t2 -> t2
                  _ -> error "Internal error: ill-typed function")
               f (desugarExpr arg)
  S.EBinop ty left op right ->
    C.EApp ty (C.EApp (halfType op) (convertBinop op) $ desugarExpr left) $
           desugarExpr right
  S.EUnop ty op arg -> C.EApp ty (convertUnop op) $ desugarExpr arg
  -- "if cond then true else false" becomes
  -- "case cond of { True -> true; False -> false }"
  S.EIf ty cond true false ->
    C.ECase ty (desugarExpr cond) $
      C.CAlts [ C.CAlt "True"  [] (desugarExpr true)
              , C.CAlt "False" [] (desugarExpr false)
              ] C.NoDefault
  -- Case statements are more complicated, so they are handled separately
  S.ECase ty e alts -> desugarCase ty e alts
  -- Lambdas can introduce polymorphism, so we need to add type bindings
  -- here as well. Type bindings may be redundant and will be prund later.
  -- See pruneBigLambdas.
  S.ELambda t@(TArrow t1 _) var body ->
    addBigLambdas t1 $ C.ELambda t (C.Var var t1) (desugarExpr body)
  S.ELambda{} -> error "Internal error: ill-typed lambda after typecheck"
  -- For let expressions, we require an occurs check to decide whether the
  -- introduced binding should be recursive. At the moment there are no
  -- mutually recursive bindings, although the syntax of the core language
  -- does allow this.
  S.ELet ty name defn body ->
    let v = C.Var name $ S.exprInfo body
        d = desugarExpr defn
     in C.ELet ty (if S.occurs name defn
                      then C.BRec [(v, d)]
                      else C.BNonRec v d)
                  (desugarExpr body)

addBigLambdas :: Type -> C.Expr -> C.Expr
addBigLambdas (TVar n) e = C.EBigLam (TForall n $ C.typeof e) n e
addBigLambdas (TArrow t1 t2) e = addBigLambdas t1 $ addBigLambdas t2 e
addBigLambdas (TConstr _ t) e = addBigLambdas t e
addBigLambdas _ e = e

-- | Convert a binary operator to the core language.
--
-- This function simply assigns a name and a type to each built-in binary
-- operator.
convertBinop :: S.Binop a -> C.Expr
convertBinop op =
  let (name, ty) = case op of
                     S.OPlus   _ -> ("+" , TArrow TInt  (TArrow TInt  TInt ))
                     S.OMinus  _ -> ("-" , TArrow TInt  (TArrow TInt  TInt ))
                     S.OTimes  _ -> ("*" , TArrow TInt  (TArrow TInt  TInt ))
                     S.ODivide _ -> ("/" , TArrow TInt  (TArrow TInt  TInt ))
                     S.OLt     _ -> ("<" , TArrow TInt  (TArrow TInt  TBool))
                     S.OLe     _ -> ("<=", TArrow TInt  (TArrow TInt  TBool))
                     S.OEq     _ -> ("==", TArrow TInt  (TArrow TInt  TBool))
                     S.ONeq    _ -> ("/=", TArrow TInt  (TArrow TInt  TBool))
                     S.OGe     _ -> (">=", TArrow TInt  (TArrow TInt  TBool))
                     S.OGt     _ -> (">" , TArrow TInt  (TArrow TInt  TBool))
                     S.OAnd    _ -> ("&&", TArrow TBool (TArrow TBool TBool))
                     S.OOr     _ -> ("||", TArrow TBool (TArrow TBool TBool))
   in C.EAtom ty $ C.AVar (C.Var name ty)

-- | Get the type of a half-applied binary operator.
halfType :: S.Binop a -> Type
halfType op = case convertBinop op of
  C.EAtom _ (C.AVar (C.Var _ (TArrow _ t))) -> t
  _ -> error "Internal error: problem in halfType"

-- | Convert a unary operator to the core language.
--
-- This function simply assigns a name and a type to each built-in unary
-- operator.
convertUnop :: S.Unop a -> C.Expr
convertUnop op =
  let (name, ty) = case op of
                     S.UNot    _ -> ("!" , TArrow TBool TBool)
                     S.UNegate _ -> ("u-", TArrow TInt  TInt )
   -- We give negation the odd-looking name "u-" in order to keep it distinct
   -- from the binary "-"
   in C.EAtom ty $ C.AVar (C.Var name ty)

-- | Desugar a case statement.
--
-- Case statements are treated separately from other kinds of expressions
-- because they are a little more complicated. Specifically, any nested
-- patterns must be flattened and replaced with nested case statements. For
-- example:
--
-- @
--     case e of
--       Left True -> e1;
--       Left False -> e2;
--       Right Nil -> e3;
--       Right (Cons h t) -> e4;
-- @
--
-- is converted to
--
-- @
--     case e of
--       Left a -> case a of
--                   True -> e1;
--                   False -> e2;;
--       Right b -> case b of
--                   Nil -> e3;
--                   Cons h t -> e4;;
-- @
desugarCase :: Type -> S.Expr Type -> [S.Alternative Type] -> C.Expr
desugarCase _ _ [] = error "Internal error: empty case statement"
desugarCase ty expr alts@(S.Alternative _ p _ : _) =
  let def = getDefault alts
   in C.ECase ty (desugarExpr expr) $
     if isLiteralType $ S.patternInfo p
         then C.LAlts (mapMaybe getLAlt alts) def
         else C.CAlts (getCAlts alts) def
 where
   -- getDefault searches for the first alternative which matches anything.
   getDefault [] = C.NoDefault
   getDefault (S.Alternative _ (S.PVar t v) e : _) =
     C.Default (C.Var v t) (desugarExpr e)
   getDefault (_ : as) = getDefault as
   -- getLAlt converts a simple literal alternative. Since the matching pattern
   -- is just a literal, there's no need to worry about nesting.
   getLAlt (S.Alternative _ (S.PConst _ c) e) = Just $
     C.LAlt (case c of
               S.CInt _ i -> C.LInt i
               S.CString _ s -> C.LString s) (desugarExpr e)
   getLAlt (S.Alternative _ S.PConstr{} _) =
     error "Internal error: constructor pattern in literal case expression"
   getLAlt _ = Nothing  -- defaults are handled separately, so can be ignored.

-- | Convert alternatives with constructor patterns to core alternatives.
getCAlts :: [S.Alternative Type] -> [C.CAlt]
-- getCAlts is spun out into it's own function because it handles the most
-- complex case and I felt it would be too crowded to dump everything into the
-- where clause of desugarCase.
-- The basic strategy is:
-- 1) Gather the set of constructors in order
-- 2) Gather each instance of each constructor
-- 3) For each constructor, recurse if there are any literal or deeper
--    patterns inside the arguments.
getCAlts alts =
  let cs = gatherConstructors [] alts
      insts = map (gatherInstances [] alts) cs
   in translate [] (map processConstructor insts)
 where
   -- gatherConstructors produces an in-order list of unique constructors
   -- encountered in the pattern list.
   gatherConstructors :: [ByteString] -> [S.Alternative Type] -> [ByteString]
   gatherConstructors acc [] = nub $ reverse acc
   gatherConstructors acc (S.Alternative _ p _ : as) = case p of
     -- This is a default constructor which hides all following patterns
     S.PVar{} -> nub $ reverse acc
     S.PConstr _ name _ -> gatherConstructors (name : acc) as
     S.PConst{} ->
       error "Internal error: literal pattern in constructor case expression"
   -- gatherInstances takes a constructor name and finds all non-shadowed
   -- instances of that constructor.
   gatherInstances :: [S.Alternative Type] -> [S.Alternative Type] -> ByteString
                   -> [S.Alternative Type]
   gatherInstances acc [] _ = reverse acc
   gatherInstances acc (a@(S.Alternative _ p _) : as) name = case p of
     S.PVar{} -> reverse acc
     S.PConstr _ n _ ->
       gatherInstances (if name == n then a : acc else acc) as name
     S.PConst{} ->
       error "Internal error: literal pattern in constructor case expression"
   -- Once processConstructor is called, all elements of alts will have the
   -- same constructor. Then processConstructor will replace a set of
   -- alternatives with a single alternative for that constructor with an
   -- inner case expression analyzing constructor arguments
   -- Operationally:
   -- 1) Generate new variable names (p1, ..., pn) for each constructor (C)
   --    argument, and build the pattern "C p1 ... pn"
   -- 2) Build up the internal case expression:
   --    a) Generate "case p1 of P1 -> e1; P2 -> e2; ... where Pi are the
   --       patterns appearing in the p1 position and each e2 is generated
   --       recursively.
   processConstructor :: [S.Alternative Type] -> S.Alternative Type
   processConstructor as@(S.Alternative at (S.PConstr ty n ps) _ : _) =
     let pats = zipWith (\p i -> S.PVar (S.patternInfo p)
                                        (pack $ "p$" ++ show i))
                        ps [1 .. length ps]
      in S.Alternative at (S.PConstr ty n pats) $ buildExpr pats 0 as
   processConstructor _ =
     error "Internal error: Unexpected pattern in processConstructor"
   buildExpr :: [S.Pattern Type] -> Int -> [S.Alternative Type] -> S.Expr Type
   buildExpr [] _ (S.Alternative _ _ e : _) = e
   buildExpr [] _ _ = error "Internal error: no alternatives in buildExpr"
   buildExpr (p : ps) i as =
     let pats = getOptions i as
         subexprs = map (buildExpr ps (i + 1) . filterAlts as i) pats
         ty = S.exprInfo (head subexprs)
      in S.ECase ty
                 (getVarExpr p)
                 (zipWith (S.Alternative ty) pats subexprs)
   -- Get the subset of alts for which the i'th argument to a constructor
   -- matches the given pattern
   filterAlts :: [S.Alternative Type] -> Int -> S.Pattern Type
              -> [S.Alternative Type]
   filterAlts as i p = mapMaybe (checkAndModifyAlt i p) as
   checkAndModifyAlt :: Int -> S.Pattern Type -> S.Alternative Type
                     -> Maybe (S.Alternative Type)
   checkAndModifyAlt i p a = applySub a <$> S.alphaSub (getOption i a) p
   applySub :: S.Alternative Type -> [(ByteString, ByteString)]
            -> S.Alternative Type
   applySub (S.Alternative t p e) s =
     S.Alternative t (subPattern s p) (subExpr s e)
   subPattern :: [(ByteString, ByteString)] -> S.Pattern Type -> S.Pattern Type
   subPattern s (S.PVar t n) = S.PVar t (fromMaybe n $ lookup n s)
   subPattern s (S.PConstr t n ps) = S.PConstr t n $ map (subPattern s) ps
   subPattern _ p@S.PConst{} = p
   subExpr :: [(ByteString, ByteString)] -> S.Expr Type -> S.Expr Type
   subExpr s (S.EVar t n) = S.EVar t (fromMaybe n $ lookup n s)
   subExpr _ e@S.EConst{} = e
   subExpr _ e@S.EConstr{} = e
   subExpr s (S.EApp t e1 e2) = S.EApp t (subExpr s e1) (subExpr s e2)
   subExpr s (S.EBinop t l o r) = S.EBinop t (subExpr s l) o (subExpr s r)
   subExpr s (S.EUnop t o a) = S.EUnop t o (subExpr s a)
   subExpr s (S.EIf t ce te fe) =
     S.EIf t (subExpr s ce) (subExpr s te) (subExpr s fe)
   subExpr s (S.ECase t e as) =
     S.ECase t (subExpr s e) (map (flip applySub s) as)
   subExpr s (S.ELambda t n e) = S.ELambda t n (subExpr (remove n s) e)
   subExpr s (S.ELet t n v b) =
     let s' = remove n s in S.ELet t n (subExpr s' v) (subExpr s' b)
   remove :: ByteString -> [(ByteString, ByteString)]
          -> [(ByteString, ByteString)]
   remove n = filter (\(m, _) -> m == n)
   -- Get a list of patterns appearing in the i'th argument to a constructor
   getOptions :: Int -> [S.Alternative Type] -> [S.Pattern Type]
   getOptions i = nubBy S.alphaEquiv . map (getOption i)
   getOption i (S.Alternative _ (S.PConstr _ _ ps) _) = ps !! i
   getOption _ _ = error "Internal error: Unexpected pattern in getOption"
   -- At this point, all alternatives have only a single constructor then
   -- variable patterns.
   translate :: [C.CAlt] -> [S.Alternative Type] -> [C.CAlt]
   translate acc [] = reverse acc
   translate acc (S.Alternative _ (S.PConstr _ n ps) e : as) =
     translate (C.CAlt n (map getVar ps) (desugarExpr e) : acc) as
   translate _ _ = error "Internal error: Unexpected pattern in translate"
   -- Get a variable from a variable pattern
   getVar :: S.Pattern Type -> C.Var
   getVar (S.PVar t n) = C.Var n t
   getVar _ = error "Internal error: Unexpected pattern in getVar"
   getVarExpr :: S.Pattern Type -> S.Expr Type
   getVarExpr (S.PVar t n) = S.EVar t n
   getVarExpr _ = error "Internal error: Unexpected pattern in getVarExpr"


{-
e : C a1 b1 -> e1
    C a1 b2 -> e2
    C a2 b1 -> e3
    C a2 b2 -> e4

e : C p1 p2 -> { p1 : a1 -> { p2 : b1 -> e1
                                   b2 -> e2 }
                      a2 -> { p2 : b1 -> e3
                                   b2 -> e4 } }

e : C p1 b1 -> e1
    C p1 b2 -> e2
    C p1 b1 -> e3
    C p1 b2 -> e4
-}

{-
For illustration, here is a stack trace of a call to pruneBigLambda starting
from a function `compose f g x = f (g x)`
original:
\f. \g. \x. app f (app g x)
after adding big lambdas we get:
/\t1 /\t2. \f. /\t2 /\t3. \g. /\t3. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
Tracing the pruning:
[] /\t1 /\t2. \f. /\t2 /\t3. \g. /\t3. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
[t1] /\t2. \f. /\t2 /\t3. \g. /\t3. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
[t1, t2] \f. /\t2 /\t3. \g. /\t3. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
[t1, t2] /\t2 /\t3. \g. /\t3. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
[t1, t2] /\t3. \g. /\t3. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
[t1, t2, t3] \g. /\t3. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
... (skipping some frames, then moving back up the stack...)
[t1, t2, t3] \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
(the previous /\t3 is eliminated because t3 is in the context
[t1, t2, t3] \g. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
(we're now at the point in the stack where t3 was introduced, so the /\ is kept)
[t1, t2] /\t3. \g. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
(t2 is skipped)
[t1, t2] \f. /\t3. \g. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
(now t2 and t1 are introduced)
[t1] /\t2. \f. /\t3. \g. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
[] /\t1. /\t2. \f. /\t3. \g. \x. app (f : t2 -> t1) (app (g : t3 -> t2) (x : t3))
and this is the type we wanted for compose
-}
-- | Remove excess type bindings.
pruneBigLambdas :: Set Int -> C.Expr -> C.Expr
pruneBigLambdas bound expr = case expr of
  -- The only interesting case is type bindings
  C.EBigLam ty v body ->
    if Set.member v bound
       then pruneBigLambdas bound body
       else C.EBigLam ty v $ pruneBigLambdas (Set.insert v bound) body
  -- Everything else just recurses in a straightforward way.
  C.EApp ty e1 e2 -> C.EApp ty
                            (pruneBigLambdas bound e1)
                            (pruneBigLambdas bound e2)
  C.ETyApp ty e t -> C.ETyApp ty (pruneBigLambdas bound e) t
  C.ELambda ty v e -> C.ELambda ty v (pruneBigLambdas bound e)
  C.ECase ty e alts -> C.ECase ty (pruneBigLambdas bound e) (pruneAlts alts)
  C.ELet ty bind e -> C.ELet ty (pruneBinding bind) (pruneBigLambdas bound e)
  e@C.EConstr{} -> e
  e@C.EAtom{} -> e
 where
   pruneAlts (C.CAlts as d) = C.CAlts (map pruneCAlt as) (pruneDefault d)
   pruneAlts (C.LAlts as d) = C.LAlts (map pruneLAlt as) (pruneDefault d)
   pruneCAlt (C.CAlt n vs e) = C.CAlt n vs (pruneBigLambdas bound e)
   pruneLAlt (C.LAlt l e) = C.LAlt l (pruneBigLambdas bound e)
   pruneDefault C.NoDefault = C.NoDefault
   pruneDefault (C.Default v e) = C.Default v (pruneBigLambdas bound e)
   pruneBinding (C.BNonRec v e) = C.BNonRec v (pruneBigLambdas bound e)
   pruneBinding (C.BRec bs) = C.BRec $ map (second $ pruneBigLambdas bound) bs

-- | Add necessary type applications.
--
-- In the preceding transformation, we added BigLams to the expression but we
-- did not add matching type applications when those BigLams are called. This
-- function resolves that by inserting type applications every time a big
-- lambda is applied to an argument.
addTyApps :: C.Expr -> C.Expr
addTyApps expr = case expr of
  C.EApp ty e1 e2 -> case e1 of
    C.EBigLam bt@(TForall n t) _ b ->
      -- Insert a TyApp before the App, i.e.,
      -- (/\ tv. e1) e2 -> ((/\ tv. e1) (typeof e2)) e2
      C.EApp ty
             (C.ETyApp (subst n (C.typeof e2) t)
                       (C.EBigLam bt n (addTyApps b))
                       (C.typeof e2))
             (addTyApps e2)
    C.EBigLam{} -> error "Internal error: ill-typed big lambda"
    _ -> C.EApp ty (addTyApps e1) (addTyApps e2)
  C.ETyApp ty e t -> C.ETyApp ty (addTyApps e) t
  C.ELambda ty v e -> C.ELambda ty v (addTyApps e)
  C.EBigLam ty v e -> C.EBigLam ty v (addTyApps e)
  C.ECase ty e as -> C.ECase ty (addTyApps e) (addAppsAlts as)
  C.ELet ty b e -> C.ELet ty (addAppsBind b) (addTyApps e)
  e@C.EConstr{} -> e
  e@C.EAtom{} -> e
 where
   addAppsAlts (C.CAlts as d) = C.CAlts (map addAppCAlt as) (addAppDef d)
   addAppsAlts (C.LAlts as d) = C.LAlts (map addAppLAlt as) (addAppDef d)
   addAppCAlt (C.CAlt n vs e) = C.CAlt n vs (addTyApps e)
   addAppLAlt (C.LAlt l e) = C.LAlt l (addTyApps e)
   addAppsBind (C.BNonRec v e) = C.BNonRec v (addTyApps e)
   addAppsBind (C.BRec bs) = C.BRec $ map (second addTyApps) bs
   addAppDef C.NoDefault = C.NoDefault
   addAppDef (C.Default var e) = C.Default var (addTyApps e)
   -- Substitute a type for a type variable
   subst v t (TVar n) = if v == n then t else TVar n
   subst v t (TArrow t1 t2) = TArrow (subst v t t1) (subst v t t2)
   subst v t (TForall n t') =
     if n == v then TForall n t' else TForall n (subst v t t')
   subst v t (TConstr n t') = TConstr n (subst v t t')
   subst _ _ t' = t'
