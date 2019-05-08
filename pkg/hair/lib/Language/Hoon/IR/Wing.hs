module Language.Hoon.IR.Wing where

import ClassyPrelude hiding (union)
import Control.Lens hiding (union)
import Language.Hoon.IR.Ty

import Control.Category ((>>>))
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Foldable (foldrM)


-- Search Forks ----------------------------------------------------------------

{-
  Search all the branches of a fork, and merge the results.

  If the fork is `Top`, return `top`, otherwise `search` all the branches
  of the fork and `merge` the results. This succeeds if all of the
  searches succeed and all of the merges succeed.
-}
searchFork :: Ord a
            => Either String b
            -> (b -> b -> Either String b)
            -> (a -> Either String b)
            -> Fork a
            -> Either String b
searchFork top merge search = \case
    Top        -> top
    FFork alts -> do
      results <- traverse search (setToList alts)
      case results of
        []   -> Left "searchFork: no matches"
        r:rs -> foldrM merge r rs



-- Traversals ------------------------------------------------------------------

atDir :: HoonDir -> Traversal' Ty Ty
atDir dd f t = dd & \case
    Dot     -> f t
    Ctx     -> walk _Core topCtx     ctx
    Arm s c -> walk _Core (topArm s) (arm s)
    L       -> walk _Cell topLeft    left
    R       -> walk _Cell topRight   right
  where
    sing :: Ord a => a -> Fork a
    sing = review _Singleton

    topRight = sing <$> right (Cell top top)
    topLeft  = sing <$> left  (Cell top top)
    topCtx   = sing <$> ctx   (Core mempty top)
    topArm s = sing <$> ctx   (Core mempty top)

    right   (Cell l r) = Cell <$> pure l <*> f r
    left    (Cell l r) = Cell <$> f l <*> pure r
    ctx     (Core a c) = Core a <$> f c

    arm s k@(Core a c) =
      Map.lookup s a & \case
        Nothing -> pure k
        Just at -> do at' <- f at
                      pure (Core (Map.insert s at' a) c)

    walk :: (Ord a, Applicative f)
         => Prism' Ty (Fork a) -> f (Fork a) -> (a -> f a) -> f Ty
    walk p top get =
      (t ^? p) & \case
        Nothing -> pure t
        Just fk -> review p <$> traverseFork top get fk

atPath :: HoonPath -> Traversal' Ty Ty
atPath [] f t     = f t
atPath (d:ds) f t = (atDir d . atPath ds) f t

getPath :: HoonPath -> Ty -> Either String Ty
getPath d t = t ^.. atPath d & \case
  []     -> Left ("No values found at path " <> show d)
  (d:ds) -> pure (foldr union d ds)


-- Name Resolution -------------------------------------------------------------

mbErr :: String -> Maybe a -> Either String a
mbErr err = \case Nothing -> Left err
                  Just a  -> pure a

getName :: Sym -> Ty -> Either String (HoonPath, Ty)
getName nm = fmap (over _1 reverse) . go []
  where
    go :: HoonPath -> Ty -> Either String (HoonPath, Ty)
    go acc t | member nm (tFace t) = pure (acc, t)
    go acc t                       =
      mbErr "getName: Can't discriminate type" (t ^? _Discrim) >>= \case
        DCore k          -> searchFork top merge (goCore acc) k
        DCell p          -> searchFork top merge (goCell acc) p
        DCoreAndCell _ _ -> Left "getName: Might be core or cell"
        DAtom _          -> Left "getName: Search ended at atom"

    top :: Either String (HoonPath, Ty)
    top = Left "Trying to search through top type"

    merge :: (HoonPath, Ty) -> (HoonPath, Ty) -> Either String (HoonPath, Ty)
    merge (i,x) (j,y) = do
      mbErr "face matches at differing locations" $ guard (i == j)
      pure (i, union x y)

    goCore :: HoonPath -> Core Ty -> Either String (HoonPath, Ty)
    goCore acc c@(Core arms ctx) = isArm <|> inCtx
      where
        isArm = ((Arm nm c:acc),) <$>
                  mbErr "no such arm" (Map.lookup nm arms)
        inCtx = go (Ctx:acc) ctx

    goCell :: HoonPath -> Cell Ty -> Either String (HoonPath, Ty)
    goCell acc (Cell l r) = go (L:acc) l <|> go (R:acc) r

-- Simple Getters and Setters --------------------------------------------------

previewErr :: String -> Prism' a b -> a -> Either String b
previewErr err p a = mbErr err (a ^? p)

getDir' :: HoonDir -> Ty -> Either String Ty
getDir' = \case
    Dot     -> pure
    L       -> previewErr notCell _Cell >=>
                 searchFork ptop merge (\(Cell l _) -> pure l)
    R       -> previewErr notCell _Cell >=>
                 searchFork ptop merge (\(Cell _ r) -> pure r)
    Ctx     -> previewErr notCore _Core >=>
                 searchFork ptop merge (\(Core _ c) -> pure c)
    Arm s c -> \t -> do c' <- mbErr "getDir': can't discern core type" $
                                t ^? _Core . _Singleton
                        mbErr "arm signature doesn't match core type" $
                          guard (c == c')
                        pure (_Core . _Singleton # c)
  where
    notCell = "Trying to use cell-indexing into non-cell value"
    notCore = "Trying to get context of non-core value"
    merge = \x y -> pure (union x y)
    ptop  = pure top

getPath' :: HoonPath -> Ty -> Either String Ty
getPath' []     t = pure t
getPath' (d:ds) t = getDir' d t >>= getPath' ds

setDir' :: HoonDir -> Ty -> Ty -> Either String Ty
setDir' dd newTy t = dd & \case
    Dot     -> pure newTy
    Ctx     -> Left "Can't edit context type"
    Arm s c -> Left "Can't edit arms"
    L       -> review _Cell . mapFork topLeft  setLeft  <$> asCell
    R       -> review _Cell . mapFork topRight setRight <$> asCell
  where
    asCell = mbErr "Can't get axis of non-cell value" (t ^? _Cell)
    topRight            = _Singleton # Cell top newTy
    setRight (Cell l _) = Cell l newTy
    topLeft             = _Singleton # Cell newTy top
    setLeft  (Cell _ r) = Cell newTy r


-- Get and Edit Wings ----------------------------------------------------------

getAxis :: TreePath -> Ty -> Either String (HoonPath, Ty)
getAxis axis = fmap (path,) . getPath path
  where
    path = axis <&> \case { False -> L; True -> R }

getLimb :: Limb -> Ty -> Either String (HoonPath, Ty)
getLimb (WAxis a) = getAxis a
getLimb (WName n) = getName n
getLimb WDot      = \t -> pure ([Dot], t)

{-
  In Hoon, only the last arm name actually resolves to an arm,
  everything else just refers to the core of that arm. `muck` handles
  this transformation.
-}
resolve :: Wing -> Ty -> Either String (HoonPath, Ty)
resolve (reverse -> ww) tt = over _1 muck <$> go ww tt
  where
    go []     t = pure ([], t)
    go (l:ls) t = do (p,t') <- getLimb l t
                     over _1 (p <>) <$> go ls t'

    muck = reverse . r . reverse
      where
        r []     = []
        r (d:ds) = d : filter (not . isWeird) ds
        isWeird (Arm _ _) = True
        isWeird Dot       = True
        isWeird _         = False

edit :: Wing -> Ty -> Ty -> Either String (HoonPath, Ty)
edit w newTy ty = do
    (path, oldTy) <- resolve w ty

    guard (all (isn't _Arm) path)

    result <- any (has _Ctx) path & \case
                True  -> ty <$ guard (nest newTy oldTy)
                False -> pure (set (atPath path) newTy ty)

    pure (path, result)
