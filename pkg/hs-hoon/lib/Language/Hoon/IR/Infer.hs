module Language.Hoon.IR.Infer where

import ClassyPrelude hiding (union, intersect, subtract, negate)
import Control.Monad.Fix
import Data.Void
import Language.Hoon.IR.Ty

import Data.List.NonEmpty
import Language.Hoon.LL.Types hiding (L, R, Ctx)
import Control.Category ((>>>))
import Control.Lens
import Data.Function ((&))
import Data.Maybe (fromJust)

import qualified Language.Hoon.LL.Types as LL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Hoon.IR.Wing as Wing
import qualified Prelude


-- Code Inference --------------------------------------------------------------

infer :: Ty -> Hoon -> Either String Ty
infer sut h = view llTy <$> down sut h

splitPattern :: Ty -> Ty -> (Ty, Ty)
splitPattern pat sut = (pat `intersect` sut, pat `diff` sut)

traversePair :: Applicative f => (f a, f b) -> f (a, b)
traversePair (mx, my) = (,) <$> mx <*> my

refine :: Wing -> Ty -> Ty -> Either String (Ty, Ty)
refine w pat sut = do
    (_,oldTy) <- Wing.resolve w sut

    let matchBr = pat `intersect` oldTy
        elseBr  = pat `diff` oldTy
        update  = \t -> snd <$> Wing.edit w t sut

    traversePair (update matchBr, update elseBr)

extractRefinement :: Ty -> Hoon -> Either String (Ty, Ty)
extractRefinement sut (HNest (Pat p) h@(HRef w)) = refine w p sut
extractRefinement sut _                          = pure (sut, sut)


-- Resolve Names and Infer Types -----------------------------------------------

splitHoonPath :: HoonPath -> Either HoonPath (HoonPath, Sym, Core Ty)
splitHoonPath pp = pp ^? _Snoc & \case
  Just (xs, Arm n c) -> Right (xs, n, c)
  _                  -> Left pp

fromHoonPath :: HoonPath -> LLPath
fromHoonPath = catMaybes . fmap \case
  Dot     -> Nothing
  Arm _ _ -> Nothing
  L       -> pure LL.L
  R       -> pure LL.R
  Ctx     -> pure LL.Ctx

resolve :: Ty -> Wing -> Either String LLTy
resolve sut w = do
    (hoonPath, resTy) <- Wing.resolve w sut
    pure $ splitHoonPath hoonPath & \case
      Left p               -> LAxis resTy (fromHoonPath p)
      Right (p, arm, core) -> lFire resTy (fromHoonPath p) arm core

  where
    lFire :: Ty -> LLPath -> Sym -> Core Ty -> LLTy
    lFire ty []   arm (Core bat ctx) = LFire ty arm bat
    lFire ty axis arm (Core bat ctx) = LWith ty (LFire ty arm bat)
                                                (LAxis (tyCore bat ctx) axis)

-- TODO Should we produce an LFire if we edit an arm?
mkEdit :: Ty -> Wing -> LLTy -> LLTy -> Either String LLTy
mkEdit sut w v x = do
  (p,ty) <- Wing.edit w (x ^. llTy) (v ^. llTy)
  p      <- pure (fromHoonPath p)
  pure (LEdit ty p v x)

disjoin, conjoin :: LLTy -> LLTy -> LLTy
disjoin x y = LTest tyBool x llYes y
conjoin x y = LTest tyBool x y llNo

negate :: LLTy -> LLTy
negate x = LTest tyBool x llNo llYes

reduce1 :: (a -> a -> a) -> a -> [a] -> a
reduce1 f z []     = z
reduce1 f z (x:xs) = foldl' f x xs

disjoinAll, conjoinAll :: [LLTy] -> LLTy
disjoinAll = reduce1 disjoin llYes
conjoinAll = reduce1 conjoin llNo

fishAtom :: (LLPath, Ty) -> Nat -> Either String LLTy
fishAtom (p, t) n = pure (LEqlQ tyBool (LAxis t p) (LAtom (tyConst n) n))

-- TODO maybe this is wrong?
fishCell :: (LLPath, Ty) -> Cell Ty -> Either String LLTy
fishCell (p, sut) (Cell l r) =
  let cellSut = bot { tCell = tCell sut }
  in if cellSut == bot then pure llNo else do
       lef   <- Wing.getPath [L] cellSut
       rit   <- Wing.getPath [R] cellSut
       left  <- fishTy (p ++ [LL.L], lef) l
       right <- fishTy (p ++ [LL.R], rit) r
       pure (conjoin left right)

fishCore :: (LLPath, Ty) -> Core Ty -> Either String LLTy
fishCore _ _ = Left "Can't fish using core type."

fishHappy :: Ord a
          => ((LLPath, Ty) -> a -> Either String LLTy)
          -> (LLPath, Ty)
          -> Happy a
          -> Either String LLTy
fishHappy f p (Fork (setToList -> xs)) = disjoinAll <$> traverse (f p) xs
fishHappy f p (Isnt (setToList -> xs)) = conjoinAll <$> traverse (fmap negate . (f p)) xs

fishFork :: Ord a
         => ((LLPath, Ty) -> a -> Either String LLTy)
         -> (LLPath, Ty)
         -> Fork a
         -> Either String LLTy
fishFork f _ Top                       = pure llYes
fishFork f p (FFork (setToList -> xs)) = disjoinAll <$> traverse (f p) xs

fishTy :: (LLPath, Ty) -> Ty -> Either String LLTy
fishTy (p, sut) t@Ty{..} = do
  atomic   <- fishHappy fishAtom (p, sut) tAtom
  core     <- fishHappy fishCore (p, sut) tCore
  cellular <- fishFork  fishCell (p, sut) tCell
  let atomic'   = conjoin (negate (LCelQ tyBool (LAxis sut p))) atomic
      cellular' = conjoin (LCelQ tyBool (LAxis sut p)) cellular
  pure (disjoinAll [atomic, core, cellular])

doesNest :: LLTy -> Pat -> Either String LLTy
doesNest (LAxis t p) (Pat ref) = fishTy (p, t) ref
doesNest h (Pat ref) = do
  j <- fishTy ([], (h ^. llTy)) ref
  pure (LWith tyBool h (simplify j))

simplify :: LLTy -> LLTy
simplify = \case
  LTest t c x y -> simplify c & \case
                     LAtom _ 0 -> simplify x
                     LAtom _ 1 -> simplify y
                     c'        -> LTest t c' (simplify x) (simplify y)
  LWith t x y   -> LWith t (simplify x) (simplify y)
  LEdit t p x y -> LEdit t p (simplify x) (simplify y)
  LPair t x y   -> LPair t (simplify x) (simplify y)
  LCore t b     -> LCore t (simplify <$> b)
  LSucc t x     -> LSucc t (simplify x)
  LCelQ t x     -> LCelQ t (simplify x)
  LEqlQ t x y   -> LEqlQ t (simplify x) (simplify y)
  ll            -> ll

mbErr :: String -> Maybe a -> Either String a
mbErr err = \case Nothing -> Left err
                  Just a  -> pure a

down :: Ty -> Hoon -> Either String LLTy
down sut = \case
  HRef w      -> resolve sut w
  HCast t h   -> do h <- down sut h
                    let nestFail = mconcat [ show (h ^. llTy)
                                           , " does not nest in "
                                           , show t
                                           ]
                    mbErr nestFail $ guard (view llTy h `nest` t)
                    pure (h & set llTy t)
  HFace f h   -> over llTy (addFace f) <$> down sut h
  HLike x y   -> do x <- down sut x
                    down sut (HCast (view llTy x) y)
  HEdit w v x -> do v <- down sut v
                    x <- down sut x
                    mkEdit sut w v x
  HEq h j     -> do h <- down sut h
                    j <- down sut j
                    let (ht, jt) = (h ^. llTy, j ^. llTy)
                    let nestFail = mconcat [ "type mismatch: "
                                           , show ht
                                           , " vs "
                                           , show jt
                                           ]
                    mbErr nestFail $ guard (nest ht jt || nest jt ht)
                    pure (LEqlQ tyBool h j)
  HAtom a     -> pure (LAtom (tyConst a) a)
  HCons h j   -> do h <- down sut h
                    j <- down sut j
                    let ty = tyCell (h ^. llTy) (j ^. llTy)
                    pure (LPair ty h j)
  HSucc h     -> LSucc tyAnyAtom <$> down sut (HCast tyAnyAtom h)
  HIf c l r   -> do (lSut, rSut) <- extractRefinement sut c
                    c <- down sut c
                    l <- down lSut l
                    r <- down rSut r
                    let nestFail = show c <> " is not a boolean value"
                    mbErr nestFail $ guard (nest (c ^. llTy) tyBool)
                    let res = view llTy l `union` view llTy r
                    pure (LTest res c l r)
  HWith h j   -> do h <- down sut h
                    j <- down (h ^. llTy) j
                    pure (LWith (j ^. llTy) h j)
  HNest p h   -> do h <- down sut h
                    doesNest h p
  HCore arms  -> do let coreTy      = tyCore (fst <$> arms) sut
                    let go decl arm = down coreTy (HCast decl arm)
                    arms <- traverse (uncurry go) arms
                    pure (LCore coreTy arms)
