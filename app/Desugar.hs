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

module Desugar
  ( desugar
  ) where

-- TODO: Need to insert a type applications. Type applications may preceed any
-- value application.

import Control.Arrow (second)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (mapMaybe)
import Data.List (nub)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Source as S
import qualified Core as C
import Type

-- | Desugar a top-level program.
--
-- For the moment, we simply assume all top level bindings are all mutually
-- recursive with each other.
desugar :: [S.Binding Type] -> [C.Binding]
desugar binds = [C.BRec $ map desugarBinding binds]

-- | Desugar a single binding.
desugarBinding :: S.Binding Type -> (C.Var, C.Expr)
desugarBinding (S.Binding ty name args expr) =
  let e = undefined ty args expr  -- expand lambda
      de = desugarExpr e
      pe = pruneBigLambdas Set.empty de
      ae = addTyApps pe
   in (C.Var name $ C.typeof ae, ae)

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
     in C.ELet ty (if occurs name defn then C.BRec [(v, d)] else C.BNonRec v d)
                  (desugarExpr body)

addBigLambdas :: Type -> C.Expr -> C.Expr
addBigLambdas (TVar n) e = C.EBigLam (TForall n $ C.typeof e) n e
addBigLambdas (TArrow t1 t2) e = (addBigLambdas t1 . addBigLambdas t2) e
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
   in map processConstructor insts
   -- instst :: [[S.Alternative Type]]
 where
   -- gatherConstructors produces an in-order list of unique constructors
   -- encountered in the pattern list.
   gatherConstructors acc [] = nub $ reverse acc
   gatherConstructors acc (S.Alternative _ p _ : as) = case p of
     -- This is a default constructor which hides all following patterns
     S.PVar _ _ -> nub $ reverse acc
     S.PConstr _ name _ -> gatherConstructors (name : acc) as
     S.PConst _ _ ->
       error "Internal error: literal pattern in constructor case expression"
   -- gatherInstances takes a constructor name and finds all non-shadows
   -- instances of that constructor.
   gatherInstances acc [] _ = reverse acc
   gatherInstances acc (a@(S.Alternative _ p _) : as) name = case p of
     S.PVar _ _ -> reverse acc
     S.PConstr _ n _ ->
       gatherInstances (if name == n then a : acc else acc) as name
     S.PConst _ _ ->
       error "Internal error: literal pattern in constructor case expression"
   -- Once processConstructor is called, all elements of alts will have the
   -- same constructor. Then processConstructor will look for any patterns
   -- which are not only a single PVar, and if any exist it will generate a
   -- new case expression recursively.
   processConstructor [] =
     error "Internal error: empty alternatives in processConstructor"
   processConstructor (a@(S.Alternative _ (S.PConstr _ name pats) e) : _) =
     if notPVar a
        then undefined
        -- recursive case: create a CAlt with new variables and then generate
        -- nested cases to examine each non-var pattern
        else C.CAlt name (map getVar pats) (desugarExpr e)
   processConstructor _ =
     error "Internal error: non-constructor pattern in processConstructors"
   -- notPVar determins whether any pattern inside a constructor pattern is not
   -- just a variable.
   notPVar (S.Alternative _ (S.PConstr _ _ pats) _) =
     any (\case { S.PVar{} -> False; _ -> True }) pats
   notPVar _ = error "Internal error: constructor pattern was expected"
   -- Convert a variable pattern from the source language to a variable
   -- in the core language.
   getVar (S.PVar t v) = C.Var v t
   getVar _ = error "Internal error: trying to get var from non-var pattern"

-- | Determine whether a name occurs in an expression.
--
-- This decides whether bindings need to be recursive, so it also considers
-- shadowing. That is, if @name@ appears in @expr@, but only inside of a scope
-- where @name@ is rebound, then we don't consider that an occurance of @name@.
occurs :: ByteString -> S.Expr a -> Bool
occurs name expr = case expr of
  S.EVar _ n -> n == name
  S.EConst _ _ -> False
  S.EConstr _ _ -> False
  S.EApp _ f a -> occurs name f || occurs name a
  S.EBinop _ l _ r -> occurs name l || occurs name r
  S.EUnop _ _ a -> occurs name a
  S.EIf _ c t f -> occurs name c || occurs name t || occurs name f
  S.ECase _ e as -> occurs name e || any (occursAlt name) as
  -- Function definition and let introduce new bindings, so we check for
  -- shadowing in these cases.
  S.ELambda _ n b -> n /= name && occurs name b
  S.ELet _ n d b -> occurs name d || (n /= name && occurs name b)

-- | Determine whether a name occurs in an alternative in a case expression.
occursAlt :: ByteString -> S.Alternative a -> Bool
occursAlt name (S.Alternative _ pat expr) =
  -- If `name` appears in `pat` then it will be shadowed in `expr`
  not (occursPattern name pat) && occurs name expr

-- | Determine whether a name occurs in a pattern.
--
-- This is used to check for shadowing since if a name occurs in a pattern, the
-- same name from the outer scope can't be referenced in the corresponding case
-- alternative.
occursPattern :: ByteString -> S.Pattern a -> Bool
occursPattern name pat = case pat of
  S.PConstr _ _ pats -> any (occursPattern name) pats
  S.PVar _ n -> n == name
  S.PConst _ _ -> False

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
  e -> e
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
-- function resovles that by inserting type applications every time an
-- application has a type which is a free variable.
addTyApps :: C.Expr -> C.Expr
addTyApps = undefined
