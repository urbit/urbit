{-- OPTIONS_GHC -Wall -Werror #-}

{- |
    # The Problem

    Uses `seq` to prevent overly-eager evaluation.

    Without this, `λx.fg` would compile to `K(fg)` which will cause
    the evaluation of `(fg)` at definition time, instead of waiting until
    `x` is passed.

    We can prevent this by transforming `λx.fg` into `λx.SKxfg`,
    which will delay the evaluation of `fg` until the right time.

    This transformation is especially important in recursive code, which
    will almost always contain an if expressions. If both branches of
    the if expression are always evaluated, then the loop will never
    terminate.

    # The Algorithm

    The goal of this algorithm is to make sure that no significant
    evaluation takes place before the most recent lambda binding has
    been made available.

      An example of insignificant evaluation, is applying `K` to `S`,
      which trivially becomes `KS`.

      Another example, is applying `3` to `ADD`, which trivial becomes
      `(ADD 3)`. This is because the `ADD` jet has arity two.

      In general, applying a value to a function of arity one is
      significant, and all other application is insignificant.

    The basic idea is to recurse through expressions, starting at the
    leaves, determine their arity, and delay any significant applications
    by transforming `(fg)` into `(SKxfg)`. This will prevent `f` from
    being applied to `g` until `x` is supplied.

    We treat the most recently bound variable as having arity 0, which is
    treated differently. Expressions of arity 0 do not need to be changed,
    since their evaluation already depends on the most-recently-bound
    variable.

    The arity of combinators:

      *S    -> 3
      *K    -> 2
      *J    -> 2
      *Jⁿ   -> 2
      *Jⁿtb -> n
      *D    -> 1

    The arity of variables:

      *x    -> 0 (most recently bound, or all from jet arguments)
      *f    -> 1 (free variable, arity statically unknowable)

    The arity of lambdas:

      *(λv.B) -> 0       (if B references `x`)
      *(λv.B) -> (*B)+1

    The arity of applications:

      *(Keₙ)  -> n+1
      *(e₀eₙ) -> 0
      *(eₙe₀) -> 0
      *(eₙeₘ) -> n-1

    The transformation of applications:

      *(e₁eₙ) -> (SEQ x e e)
      *(eₙeₙ) -> ee

    ## Better Output for Jets

    - Let's look at an example:

      - `λx.λy.λz.xyz`

    - Without this trasnformation, this compiles to:

      - `SKK`

    - But that doesn't preserve evaluation order.

      - `(λx.λy.λz.xyz) ded %too-early` evaluates to:

         `(λz. ded %too-early z)`

      - But `SKK ded %too-early` evaluates to:

        `(ded %too-early)`

    - The transformation above soves the problem, by instead producing:

      ```
      (S (S (K S)
         (S (K (S (K S)))
            (S (K (S S (K K))) (S (K (S (K K) S)) (S (K (S (S K))) K)))))
      (K (S K)))
      ```

    - However, this is large. To solve this problem, we can take advantage
      of the jet system. Let's look at the same example, jetted:

       ```
       J J J K (λx.λy.λz.xyz)
       ```

    - Because the jet system will delay evaluation until all three
      arguments have been passed, the transformation is not necessary for
      those arguments.

    - So, we can recover our nice output:

      ```
      J J J K (S K K)
      ```

    ## Better Output with `yet`

    In some expressions, we can produce slightly smaller code by using
    `In` instead of `seq`. `In` is the bulk combinator for the identity
    function. For example, `I5` is `J J J J J %In I`.

    For example, `λx.(fgx)` becomes λx.(seq x f g x), which produces
    significantly larger output.

    Instead, we can produce: `λx.(I3 f g x). This also delays the
    application `(fg)` until x is provided, but does so without
    introducing another variable reference, so the resulting code size
    is significantly smaller.

    When does this rule apply?

      (pqx) -> (I3 p q x)
      (pqrx) -> (I4 p q r x)
      (pqrsx) -> (I5 p q r s x)
      ...

    Informally, any application that would need to be delayed, but the
    result is eventually applied to `x`.

    What's the algorithm for this?

    It doesn't quite fit into the current model.

    When we process `fgx`, we first process `fg`. So, by the time we
    see the `x` we have already transformed `fg` into `SKxfg`.

    Let's just try some stuff.

    If we convert an expression to a tree first, then

      `(p q r s x)` turns into `p[q,r,s,x]`

    If the head and the first argument would need to be delayed, then
    we can apply this transformation.

    This is too agressive, though.  For example:

      `(K p q r x)` would become `(I5 K p q r x)`

    But, `Kpq` doesn't need to be delayed.

    Instead, it would be better to produce:

      `(I3 (Kpq) r x)`

    So, really, we want to find things of the shape:

      `AB(C‥)x` where `AB` would usually need to be delayed.

    And transform that into

      `In A B (C‥) x`

    I guess, in `Ex`, we can see that the RHS has arity 0, and remember
    that. Later, when we go to delay `AB`, we can use `In` instead of
    `Qx`. We will also need to know how far down the list we have gone.

    Let's go through an example:

      `ABCDEx`

    We see `(ABCDE)x` and x has arity 0.

      So, we begin processing `(ABCDE)` with the knowledge that there is an
      forced expression 1 steps behind.

        This is an application, with a RHS of arity (>0), so we process the
        LHS `(ABCD)`. with the knowledge that there is an forced expression 2
        steps behind.

          This is an application, with a RHS of arity (>0), so we process the
          LHS `(ABC)`. with the knowledge that there is an forced expression 3
          steps behind.

            This is an application, with a RHS of arity (>0), so we process the
            LHS `(AB)`. with the knowledge that there is an forced expression 4
            steps behind.

            This is an application, with a RHS of arity (>0), so we process the
            LHS `A`. with the knowledge that there is an forced expression 5
            steps behind.

            A is not an application, so we return `A` with arity 1.

          `(AB)` is an application that needs to be delayed, but we know
          that there is a forced expression 4 steps behind.

          So we produce `(I6 A B)` with arity 4

        `(I5 A B C)` has arity 3

        `(I5 A B C D)` has arity 2

      `(I5 A B C D E)` has arity 1

    Then `((I5 A B C D E) x)` is an application of an expression of
    arity 1 against an expression of arity 0, which is safe.

    This approach seems to work.

  Thinking out loud:

    What does MakeStrict operate on?

    Lambda expressions whose free variables are uruk values.

    What is an uruk value?

      An application of two uruk values

      S, K, J, D, or a jet.

    In the `pak` example:

      In the `pak` example:

      ```
      ++  (pak n)    (J J K (n sksucc skzero))
      ```

    We have this body: `(J J K (n sksucc skzero))`

    Here we have an expression that contains three uruk values: `J`,
    `J`, and `K`. Would combining them be the right anwser?

    I guess no.

      What is the arity of this?

        `(J J x)`

      Well, we can't know because we don't know if `x` is a `J`
      or not.

      However, `J` has arity 2, and `(J J)` also has arity two. The
      given value-arity machinery knows this. So, actually, yes: I think
      combining applications of values into values will give correct
      arity information.

  ## TODO Automatic Recognition of Jet Literals

  ```
  J [^J] (λx.B)
  J J [^J] (λx.λy.B)
  J J J [^J] (λx.λy.λz.B)
  ...
  ```

  Any expression of the above shape can use the optimized jet rules
  automatically. This avoids the need for a second entry-point.
-}

module Urbit.Moon.MakeStrict (makeStrict, Prim(..)) where

import ClassyPrelude hiding (try, Prim)

import Bound.Scope        (fromScope, toScope)
import Bound.Var          (Var(..), unvar)
import Data.List          (nub)
import Numeric.Natural    (Natural)
import Urbit.Moon.Arity   (Arity(..), appArity, arityPos)
import Urbit.Moon.Bracket (Exp(..))
import Urbit.Moon.Smoosh  (smoosh, unSmoosh)
import Urbit.Pos          (Pos)


-- Types -----------------------------------------------------------------------

type Nat = Natural

type ExpV p v a = Exp p () (Var v a)

data Prim p = Prim
  { pSeq :: p
  , pYet :: Int -> p
  , pKay :: p
  , pApp :: p -> p -> p
  , pArg :: p -> Maybe Arity
  }

data RecSt p v = RecSt
  { rsPri :: Prim p
  , rsMRB :: v
  , rsRit :: Maybe Int
  }

data RecRes p v a = RecRes
  { rrArg :: Maybe Arity
  , rrFre :: [a]
  , rrExp :: ExpV p v a
  }

rsArg :: RecSt p v -> p -> Maybe Arity
rsArg = pArg . rsPri


-- Calculate Application Arity and Delay if necessary --------------------------

{-
    [e₀e₀]     -> (e e)₀
    [eₙe₀]     -> (e e)₀
    [e₁eₙ]     -> (SEQ x e e)₀
    [e₁eₙ]e₀   -> (I3 e e)₁
    [e₁eₙ]eₙe₀ -> (I4 e e)₂
    [eₙeₙ]     -> [ee]{n-1}
-}
safeApp
  :: RecSt p v
  -> (Maybe Arity, ExpV p v a)
  -> (Maybe Arity, ExpV p v a)
  -> (Maybe Arity, ExpV p v a)
safeApp _ (Nothing, x) (_      , y) = (Nothing, x :@ y)
safeApp _ (_      , x) (Nothing, y) = (Nothing, x :@ y)
safeApp RecSt { rsRit, rsPri, rsMRB } (Just arX, x) (Just arY, y) =
  case (appArity arX arY, rsRit) of
    (Just ar, _      ) -> (Just ar, x :@ y)
    (Nothing, Nothing) -> (Nothing, seqHed :@ x :@ y)
    (Nothing, Just n ) -> (Just (yetAri n), yetHed (n + 2) :@ x :@ y)
 where
  seqHed = Pri (pSeq rsPri) :@ Var (B rsMRB)
  yetHed n = Pri (pYet rsPri $ n)
  yetAri n = AriOth (fromIntegral n)


-- Core Loop -------------------------------------------------------------------

onlyFree :: [Var v x] -> [x]
onlyFree = mapMaybe (unvar (const Nothing) Just)

incArity :: Maybe Arity -> Maybe Arity
incArity Nothing  = Just (AriOth 1)
incArity (Just a) = Just (AriOth $ succ $ arityPos a)

loop
  :: forall p v a
   . (Eq p, Eq v, Eq a)
  => RecSt p v
  -> ExpV p v a
  -> RecRes p v a
loop st@RecSt {..} = \case
  Pri p -> RecRes (rsArg st p) [] (Pri p)
  Var v@(B _) -> RecRes (varArity v) [] (Var v)
  Var v@(F f) -> RecRes (varArity v) [f] (Var v)
  Lam () b ->
    let boSt = st { rsMRB = (), rsRit = Nothing }
        body = loop boSt (fromScope b)
        args = do
          guard (not $ any isBoundVar $ rrFre body)
          incArity (rrArg body)
    in  RecRes args (onlyFree $ rrFre body) (Lam () $ toScope $ rrExp body)

  x :@ y ->
    let rit = loop (st { rsRit = Nothing }) y

        dis = case (rrArg rit, rsRit) of
          (Nothing, _      ) -> Just 1
          (_,       Just n ) -> Just (n + 1)
          (_,       Nothing) -> Nothing

        lef = loop (st { rsRit = dis }) x

        (rArg, rExp) =
          case rrArg lef of
            Just (AriHdr n) -> doJetLit n y
            _ -> safeApp st (rrArg lef, rrExp lef) (rrArg rit, rrExp rit)

    in  RecRes
          { rrArg = rArg
          , rrExp = rExp
          , rrFre = nub (rrFre lef <> rrFre rit)
          }

 where
  doJetLit :: Pos -> ExpV p v a -> (Maybe Arity, ExpV p v a)
  doJetLit p expr = (ari, res)
   where
    ari = if any isBoundVar fre then Nothing else Just (AriOth p)
    (res, _, fre) = makeJetStrict rsPri (fromIntegral p) expr

  varArity :: Var v a -> Maybe Arity
  varArity (B _) = Nothing
  varArity (F _) = Just (AriOth 1)

  isBoundVar :: Var x y -> Bool
  isBoundVar = \case
    B _ -> True
    F _ -> False


-- Loop Entry Point ------------------------------------------------------------

enter
  :: (Eq p, Eq v, Eq a)
  => v
  -> Prim p
  -> ExpV p v a
  -> (ExpV p v a, Maybe Arity, [a])
enter v0 pri = (\RecRes{..} -> (rrExp, rrArg, rrFre)) . loop st
 where
  st = RecSt { rsMRB = v0
             , rsPri = pri
             , rsRit = Nothing
             }


-- Unjetted Entry Point --------------------------------------------------------

makeStrict' :: (Eq p, Eq a) => Prim p -> Exp p () a -> (Exp p () a, Maybe Arity, [a])
makeStrict' p = go
 where
  go = \case
    Pri x -> (Pri x, pArg p x, [])
    Var v -> (Var v, Just (AriOth 1), [v])
    x :@ y ->
      let (xv, xa, xf) = go x
      in
        case xa of
          Just (AriHdr n) ->
            let (yv, ya, yf) = makeJetStrict p (fromIntegral n) y
                ra = join (appArity <$> xa <*> ya)
            in (xv :@ yv, ra, nub (xf <> yf))
          _ ->
            let (yv, ya, yf) = go y
                ra = join (appArity <$> xa <*> ya)
            in (xv :@ yv, ra, nub (xf <> yf))
    Lam bi b ->
      let bo           = fromScope b
          (bv, ba, bf) = enter () p bo
      in  (Lam bi (toScope bv), incArity ba, bf)

makeJetStrict
  :: (Eq p, Eq a)
  => Prim p
  -> Int
  -> Exp p () a
  -> (Exp p () a, Maybe Arity, [a])
makeJetStrict pri n expr = fromMaybe (makeStrict' pri expr) $ go expr
 where
  go e = do
    let depth = fromIntegral n :: Nat
    e' <- smoosh depth e
    let (re, _, rf) = enter 0 pri e'
    pure (unSmoosh depth re, Just (AriOth (fromIntegral n)), rf)

resExp :: (e, a, b) -> e
resExp (x, _, _) = x

makeStrict :: (Eq p, Eq a) => Prim p -> Exp p () a -> Exp p () a
makeStrict p = resExp . makeStrict' p
