module Language.Hoon.IR.Ty where

import ClassyPrelude hiding (union, intersect)
import Control.Lens
import Control.Lens.TH
import Control.Monad.Fix
import Data.Void

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Maybe (fromJust)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck


-- Happy and Fork --------------------------------------------------------------

data Fork a = Top | FFork (Set a)
  deriving (Eq, Ord)

showHappy :: (Show a, Ord a) => String -> Set a -> String
showHappy i xs = intercalate i (show <$> setToList xs)

instance (Ord a, Show a) => Show (Fork a)
  where
    show Top                                      = "Top"
    show (FFork alts)               | null alts   = "Bot"
    show (FFork (setToList -> [x]))               = show x
    show (FFork alts)                             =
      "(" <> showHappy " ∪ " alts <> ")"

data Happy a = Fork (Set a) | Isnt (Set a)
  deriving (Eq, Ord)

instance (Ord a, Show a) => Show (Happy a)
  where
    show (Isnt alts) | null alts   = "Top"
    show (Fork alts) | null alts   = "Bot"
    show (Fork (setToList -> [x])) = show x
    show (Isnt (setToList -> [x])) = "not:" <> show x
    show (Fork alts)               = showHappy " ∪ " alts
    show (Isnt alts)               = "not:" <> showHappy " ∩ " alts

_HappyFork :: Ord a => Prism' (Happy a) (Fork a)
_HappyFork = prism' mk get
  where
    mk Top       = Isnt mempty
    mk (FFork f) = Fork f
    get (Isnt i) = if null i then pure Top else Nothing
    get (Fork h) = pure (FFork h)

_Singleton :: Ord a => Prism' (Fork a) a
_Singleton = prism' mk get
  where
    mk a = FFork (Set.singleton a)
    get = \case FFork (setToList -> [x]) -> Just x
                _                        -> Nothing

nullHappy :: Ord a => Happy a -> Bool
nullHappy (Fork f) = null f
nullHappy (Isnt i) = False

nullFork :: Ord a => Fork a -> Bool
nullFork = nullHappy . review _HappyFork

traverseSet :: (Applicative f, Ord a, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f = fmap setFromList . traverse f . setToList

mapFork :: Ord b => Fork b -> (a -> b) -> Fork a -> Fork b
mapFork top _ Top       = top
mapFork _   f (FFork k) = FFork (Set.map f k)

traverseFork :: (Applicative f, Ord a, Ord b)
             => f (Fork b) -> (a -> f b) -> Fork a -> f (Fork b)
traverseFork top f = \case
  Top     -> top
  FFork k -> FFork <$> traverseSet f k

traverseHappyFork :: (Applicative f, Ord a)
                  => (a -> f a) -> Happy a -> f (Happy a)
traverseHappyFork f = \case
  Isnt k -> pure (Isnt k)
  Fork k -> Fork <$> traverseSet f k

-- Type Types ------------------------------------------------------------------

type Sym = String
type Nat = Int

data Cell t = Cell t t
  deriving (Eq, Ord)

instance Show t => Show (Cell t) where
  show (Cell l r) = mconcat [ "["
                            , filter (/= '"') (show l)
                            , " "
                            , filter (/= '"') (show r)
                            , "]"
                            ]

data Core t = Core (Map Sym t) t
  deriving (Eq, Ord)

instance Show t => Show (Core t) where
  show (Core batt ctx) = mconcat [ "%:("
                               , filter (/= '"') (show ctx)
                               , " → "
                               , filter (/= '"') (show $ mapToList batt)
                               , ")"
                               ]

data Func t = Func t t
  deriving (Eq, Ord, Show)

data Ty = Ty { tFace :: Set Sym
             , tAtom :: Happy Nat
             , tCore :: Happy (Core Ty)
             , tCell :: Fork (Cell Ty)
            -- , tFunc :: Func Ty
             }
  deriving (Eq, Ord)

instance Show Ty
  where
    show t = "\"" <> showTy t <> "\""

showTy :: Ty -> String
showTy t@Ty{..} = faces <> niceTy
  where
    niceTy = let tyNoFace = t { tFace = mempty } in
             if top    == tyNoFace then "*" else
             if tyBool == tyNoFace then "?" else
               t ^? _Discrim & \case
                 Just d  -> show d
                 Nothing -> let alts = catMaybes [ atoms, cores, cells ]
                            in "(" <> intercalate " ∪ " alts <> ")"
    faces = if null tFace then "" else unpack (intercalate "," tFace <> "=")
    atoms = if      tAtom == bot then Nothing
            else if tAtom == top then Just "@"
                                 else Just (show tAtom)
    cores = if      tCore == bot then Nothing
            else if tCore == top then Just "%"
                                 else Just (show tCore)
    cells = if      tCell == bot then Nothing
            else if tCell == top then Just "^"
                                 else Just (show tCell)

-- IR Types --------------------------------------------------------------------

type TreePath = [Bool]

data Limb
    = WName Sym
    | WAxis TreePath
    | WDot
  deriving (Eq, Ord, Show)

type Wing = [Limb]

newtype Pat = Pat Ty
  deriving (Eq, Ord, Show)

data Hoon
    = HRef Wing
    | HNest Pat Hoon
    | HSucc Hoon
    | HEq Hoon Hoon
    | HIf Hoon Hoon Hoon
    | HAtom Nat
    | HCons Hoon Hoon
    | HEdit Wing Hoon Hoon
    | HWith Hoon Hoon
    | HFace Sym Hoon
    | HCore (Map Sym (Ty, Hoon))
    | HCast Ty Hoon
    | HLike Hoon Hoon
  deriving (Eq, Ord, Show)


-- Path Types ------------------------------------------------------------------

data HoonDir = Dot | L | R | Ctx | Arm Sym (Core Ty)
  deriving (Eq, Ord, Show)

type HoonPath = [HoonDir]


-- Type Discrimination ---------------------------------------------------------

data Discrim
    = DAtom (Fork Nat)
    | DCore (Fork (Core Ty))  -- TODO: Should this be Core Discrim
    | DCell (Fork (Cell Ty))
    | DCoreAndCell (Fork (Core Ty)) (Fork (Cell Ty))
  deriving (Eq, Ord)

instance Show Discrim where
  show = \case
    DAtom Top                        -> "@"
    DCore Top                        -> "%"
    DCell Top                        -> "^"

    DAtom (FFork (setToList -> []))  -> "!"
    DCell (FFork (setToList -> []))  -> "!"
    DCore (FFork (setToList -> []))  -> "!"

    DAtom (FFork (setToList -> [x])) -> show x
    DCell (FFork (setToList -> [x])) -> show x
    DCore (FFork (setToList -> [x])) -> show x

    DAtom (FFork xs) -> showHappy " ∪ " xs
    DCell (FFork xs) -> showHappy " ∪ " xs
    DCore (FFork xs) -> showHappy " ∪ " xs
    DCoreAndCell k p -> showHappy " ∪ " (setFromList [DCore k, DCell p])


-- Boolean algebras ------------------------------------------------------------

class BoolAlg a where
  top :: a
  bot :: a
  complement :: a -> a
  union :: a -> a -> a
  intersect :: a -> a -> a
  nest :: a -> a -> Bool

  diff :: a -> a -> a
  diff x y = intersect x (complement y)


-- Ur Elements -----------------------------------------------------------------

class Ord a => Ur a

instance Ur Nat
instance (Ord t) => Ur (Core t)


-- Happy BoolAlg ---------------------------------------------------------------

instance Ur a => BoolAlg (Happy a) where
  bot = Fork Set.empty
  top = Isnt Set.empty

  complement (Fork x) = Isnt x
  complement (Isnt x) = Fork x

  union (Fork x) (Fork y) = Fork (Set.union x y)
  union (Isnt x) (Isnt y) = Isnt (Set.intersection x y)
  union (Isnt x) (Fork y) = Isnt (Set.difference x y)
  union (Fork x) (Isnt y) = Isnt (Set.difference y x)

  intersect (Fork x) (Fork y) = Fork (Set.intersection x y)
  intersect (Isnt x) (Isnt y) = Isnt (Set.union x y)
  intersect (Isnt x) (Fork y) = Fork (Set.difference y x)
  intersect (Fork x) (Isnt y) = Fork (Set.difference x y)

  nest (Fork xs) (Fork ys) = xs `Set.isSubsetOf` ys
  nest (Isnt xs) (Isnt ys) = ys `Set.isSubsetOf` xs
  nest (Fork xs) (Isnt ys) = Set.null (ys `Set.intersection` xs)
  nest (Isnt xs) (Fork ys) = False


-- (Fork Cell) BoolAlg ---------------------------------------------------------

instance (Ord t, BoolAlg t) => BoolAlg (Fork (Cell t)) where
  bot = FFork mempty
  top = Top

  complement Top        = bot
  complement (FFork cs) = FFork $ Set.fromList do
    (Cell x y) <- toList cs
    id [ Cell (complement x) y
       , Cell x              (complement y)
       , Cell (complement x) (complement y)
       ]

  union Top        _          = Top
  union _          Top        = Top
  union (FFork xs) (FFork ys) = FFork (Set.union xs ys)

  nest _          Top        = True
  nest Top        _          = False
  nest (FFork xs) (FFork ys) = all (\x -> any (cellNest x) ys) xs
    where
      cellNest (Cell x1 y1) (Cell x2 y2) = nest x1 x2 && nest y1 y2

  intersect Top        f          = f
  intersect f          Top        = f
  intersect (FFork xs) (FFork ys) = FFork $ Set.fromList do
    (Cell x1 x2) <- toList xs
    (Cell y1 y2) <- toList ys
    let z1 = (intersect x1 y1)
        z2 = (intersect x2 y2)
    guard (z1 /= bot && z2 /= bot)
    pure (Cell z1 z2)


-- Func BoolAlg ----------------------------------------------------------------

instance BoolAlg t => BoolAlg (Func t) where
  bot = Func top bot
  top = Func bot top

  complement (Func x y) = Func (complement x) (complement y)

  union (Func x1 y1) (Func x2 y2) = Func (intersect x1 x2) (union y1 y2)
  intersect (Func x1 y1) (Func x2 y2) = Func (union x1 x2) (union y1 y2)

  nest (Func x1 y1) (Func x2 y2) = nest x2 x1 && nest y1 y2

-- Ty BoolAlg ------------------------------------------------------------------

instance BoolAlg Ty where
  bot = Ty mempty bot bot bot
  top = Ty mempty top top top

  complement = \case
    Ty{tFace,tAtom,tCore,tCell} ->
      Ty { tFace = mempty
         , tAtom = complement tAtom
         , tCore = complement tCore
         , tCell = complement tCell
         }

  union p q =
    Ty { tFace = tFace p `Set.intersection` tFace q
       , tAtom = tAtom p `union` tAtom q
       , tCore = tCore p `union` tCore q
       , tCell = tCell p `union` tCell q
       }

  intersect p q =
    Ty { tFace = Set.union (tFace p) (tFace q)
       , tAtom = tAtom p `intersect` tAtom q
       , tCore = tCore p `intersect` tCore q
       , tCell = tCell p `intersect` tCell q
       }

  nest p q =
    and [ nest (tAtom p) (tAtom q)
        , nest (tCell p) (tCell q)
        , nest (tCore p) (tCore q)
        ]

  diff p q =
    Ty { tFace = Set.union (tFace p) (tFace q)
       , tAtom = tAtom p `diff` tAtom q
       , tCore = tCore p `diff` tCore q
       , tCell = tCell p `diff` tCell q
       }


-- Basic Types -----------------------------------------------------------------

tyAnyCell, tyAnyAtom, tyAnyCore :: Ty
tyAnyAtom = bot { tAtom = top }
tyAnyCell = bot { tCell = top }
tyAnyCore = bot { tCore = top }

tyConst :: Nat -> Ty
tyConst x = bot { tAtom = Fork (singleton x) }

tyCell :: Ty -> Ty -> Ty
tyCell x y = bot { tCell = _Singleton # Cell x y }

tyCore :: Map Sym Ty -> Ty -> Ty
tyCore arms ctx = bot { tCore = Fork (singleton (Core arms ctx)) }

tyNull, tyYes, tyNo, tyBool :: Ty
tyNull = tyConst 0
tyYes  = tyConst 0
tyNo   = tyConst 1
tyBool = tyYes `union` tyNo

addFace :: Sym -> Ty -> Ty
addFace fc t = t { tFace = Set.insert fc (tFace t) }


-- Lenses ----------------------------------------------------------------------

{-
  TODO The review case is not quite right. If we do
  `DAtom nullFork # _HappyFork`, we will get `tyNull`. So
  this law is broken:

      Just f == (f # _HappyFork) ^? _HappyFork
-}
_Discrim :: Prism' Ty Discrim
_Discrim = prism' mk get
  where
    mk = \case
      DAtom f -> bot { tAtom = _HappyFork # f }
      DCore f -> bot { tCore = _HappyFork # f }
      DCell f -> bot { tCell = f }

      DCoreAndCell core cell ->
        bot { tCell = cell, tCore = _HappyFork # core }

    get Ty{..} =
      (nullHappy tCore, nullFork tCell, nullHappy tAtom) & \case
        ( False, True,  True  ) -> DCore <$> tCore ^? _HappyFork
        ( True,  False, True  ) -> DCell <$> pure tCell
        ( True,  True,  False ) -> DAtom <$> tAtom ^? _HappyFork
        ( False, False, True  ) -> DCoreAndCell <$> tCore ^? _HappyFork
                                                <*> pure tCell
        _                       -> Nothing

makePrisms ''HoonDir
makePrisms ''Discrim

_Cell :: Prism' Ty (Fork (Cell Ty))
_Cell = _Discrim . _DCell

_Core :: Prism' Ty (Fork (Core Ty))
_Core = _Discrim . _DCore

_Atom :: Prism' Ty (Fork Nat)
_Atom = _Discrim . _DAtom

_CoreAndCell :: Prism' Ty (Fork (Core Ty), Fork (Cell Ty))
_CoreAndCell = _Discrim . _DCoreAndCell


-- Cast cores to cells. --------------------------------------------------------

castCoreToCell :: (BoolAlg t) => Core t -> Cell t
castCoreToCell (Core _ c) = Cell top c

castTyToCell :: Ty -> Maybe Ty
castTyToCell = fmap (review _Cell . cast) . preview _CoreAndCell
  where
    cast (cores, cells) = union cells (mapFork Top castCoreToCell cores)


-- Testing: Generators ---------------------------------------------------------

-- prop_example :: Int -> Int -> Bool
-- prop_example a b = a + b == b + a

-- tests :: TestTree
-- tests = $(testGroupGenerator)


perms :: forall a. Show a => [a] -> [[a]]
perms as = do
  ii <- iis (length as)
  pure (foldr f [] (zip ii as))
  where
    f (True, x)  xs = x:xs
    f (False, x) xs = xs

    iis :: Int -> [[Bool]]
    iis 0 = pure [True, False]
    iis n = do
      x <- iis (n - 1)
      b <- [True, False]
      pure (b:x)

genFace :: Gen (Set Sym)
genFace = do
    xs <- oneof (pure <$> perms ["p", "q"])
    pure (setFromList xs)

instance Arbitrary Ty where
  arbitrary = do
    getSize >>= \case
      0 -> oneof [pure top, pure bot]
      1 -> resize 0 (Ty <$> genFace <*> arbitrary <*> arbitrary <*> arbitrary)
      n -> Ty <$> genFace <*> arbitrary <*> arbitrary <*> resize 1 arbitrary

instance (Arbitrary t, Ord t) => Arbitrary (Happy (Core t)) where
  -- TODO
  arbitrary = oneof [ pure (Fork mempty), pure (Isnt mempty) ]

instance (Arbitrary t, Ord t) => Arbitrary (Fork t) where
  arbitrary = oneof [ pure Top, FFork <$> setFromList <$> listOf arbitrary ]

instance Arbitrary t => Arbitrary (Cell t) where
  arbitrary = Cell <$> arbitrary <*> arbitrary

instance Arbitrary (Happy Nat) where
  arbitrary = do
    xs <- oneof (pure <$> perms [0, 1])
    b  <- arbitrary
    pure case b of
      True -> Fork (setFromList xs)
      False -> Isnt (setFromList xs)

instance Arbitrary t => Arbitrary (Func t) where
  arbitrary = Func <$> arbitrary <*> arbitrary

-- can we make this ==?
equiv :: (BoolAlg a) => a -> a -> Bool
equiv x y = nest x y && nest y x

nestRefl :: (Arbitrary a, BoolAlg a) => a -> Bool
nestRefl x = nest x x

subGivesBot :: (Arbitrary a, Eq a, BoolAlg a) => a -> Bool
subGivesBot x = bot == diff x x

nestTrans :: (Arbitrary a, BoolAlg a) => a -> a -> a -> Property
nestTrans a b c = (nest a b && nest b c) ==> nest a c

nestUnion :: (Arbitrary a, BoolAlg a) => a -> a -> Bool
nestUnion x y = nest x u && nest y u
  where u = x `union` y

nestIntersect :: (Arbitrary a, BoolAlg a) => a -> a -> Bool
nestIntersect x y = nest u x && nest u y
  where u = x `intersect` y

unionId :: (Arbitrary a, BoolAlg a) => a -> Bool
unionId x = union x bot `equiv` x

intersectId :: (Arbitrary a, BoolAlg a) => a -> Bool
intersectId x = intersect x top `equiv` x

unionAbsorbs :: (Arbitrary a, BoolAlg a) => a -> Bool
unionAbsorbs x = union x top `equiv` top

intersectAbsorbs :: (Arbitrary a, BoolAlg a) => a -> Bool
intersectAbsorbs x = intersect x bot `equiv` bot

unionCommutes :: (Arbitrary a, BoolAlg a) => a -> a -> Bool
unionCommutes x y = union x y `equiv` union y x

intersectCommutes :: (Arbitrary a, BoolAlg a) => a -> a -> Bool
intersectCommutes x y = intersect x y `equiv` intersect y x

unionAssociates :: (Arbitrary a, BoolAlg a) => a -> a -> a -> Bool
unionAssociates x y z = union x (union y z) `equiv` union (union x y) z

intersectAssociates :: (Arbitrary a, BoolAlg a) => a -> a -> a -> Bool
intersectAssociates x y z = intersect x (intersect y z)
                    `equiv` intersect (intersect x y) z

unionDistributes :: (Arbitrary a, BoolAlg a) => a -> a -> a -> Bool
unionDistributes x y z = union x (intersect y z)
                 `equiv` intersect (union x y) (union x z)

intersectDistributes :: (Arbitrary a, BoolAlg a) => a -> a -> a -> Bool
intersectDistributes x y z = intersect x (union y z)
                     `equiv` union (intersect x y) (intersect x z)

doubleCompl :: (Arbitrary a, BoolAlg a) => a -> Bool
doubleCompl x = equiv x (complement (complement x))

complSub :: (Arbitrary a, BoolAlg a) => a -> a -> Bool
complSub x y = diff x y `equiv` intersect x (complement y)

deMorganUnion :: (Arbitrary a, BoolAlg a) => a -> a -> Bool
deMorganUnion x y = complement (union x y) `equiv` intersect (complement x) (complement y)

deMorganIntersect :: (Arbitrary a, BoolAlg a) => a -> a -> Bool
deMorganIntersect x y = complement (intersect x y) `equiv` union (complement x) (complement y)


prop_nestReflAtom  = nestRefl  @(Happy Nat)
prop_nestTransAtom = nestTrans @(Happy Nat)
prop_nestUnionAtom = nestUnion @(Happy Nat)
prop_nestIntersectAtom = nestIntersect @(Happy Nat)
prop_unionIdAtom = unionId @(Happy Nat)
prop_intersectIdAtom = intersectId @(Happy Nat)
prop_unionAbsorbsAtom = unionAbsorbs @(Happy Nat)
prop_intersectAbsorbsAtom = intersectAbsorbs @(Happy Nat)
prop_unionCommutesAtom = unionCommutes @(Happy Nat)
prop_intersectCommutesAtom = intersectCommutes @(Happy Nat)
prop_unionAssociatesAtom = unionAssociates @(Happy Nat)
prop_intersectAssociatesAtom = intersectAssociates @(Happy Nat)
prop_unionDistributesAtom = unionDistributes @(Happy Nat)
prop_intersectDistributesAtom = intersectDistributes @(Happy Nat)
prop_doubleComplAtom = doubleCompl @(Happy Nat)
prop_complSubAtom = complSub @(Happy Nat)
prop_deMorganUnionAtom = deMorganUnion @(Happy Nat)
prop_deMorganIntersectAtom = deMorganIntersect @(Happy Nat)

prop_nestReflCore  = nestRefl  @(Happy (Core (Happy Nat)))
prop_nestTransCore = nestTrans @(Happy (Core (Happy Nat)))
prop_nestUnionCore = nestUnion @(Happy (Core (Happy Nat)))
prop_nestIntersectCore = nestIntersect @(Happy (Core (Happy Nat)))
prop_unionIdCore = unionId @(Happy (Core (Happy Nat)))
prop_intersectIdCore = intersectId @(Happy (Core (Happy Nat)))
prop_unionAbsorbsCore = unionAbsorbs @(Happy (Core (Happy Nat)))
prop_intersectAbsorbsCore = intersectAbsorbs @(Happy (Core (Happy Nat)))
prop_unionCommutesCore = unionCommutes @(Happy (Core (Happy Nat)))
prop_intersectCommutesCore = intersectCommutes @(Happy (Core (Happy Nat)))
prop_unionAssociatesCore = unionAssociates @(Happy (Core (Happy Nat)))
prop_intersectAssociatesCore = intersectAssociates @(Happy (Core (Happy Nat)))
prop_unionDistributesCore = unionDistributes @(Happy (Core (Happy Nat)))
prop_intersectDistributesCore = intersectDistributes @(Happy (Core (Happy Nat)))
prop_doubleComplCore = doubleCompl @(Happy (Core (Happy Nat)))
prop_complSubCore = complSub @(Happy (Core (Happy Nat)))
prop_deMorganUnionCore = deMorganUnion @(Happy (Core (Happy Nat)))
prop_deMorganIntersectCore = deMorganIntersect @(Happy (Core (Happy Nat)))

prop_nestReflCell  = nestRefl  @(Fork (Cell (Happy Nat)))
prop_nestTransCell = nestTrans @(Fork (Cell (Happy Nat)))
prop_nestUnionCell = nestUnion @(Fork (Cell (Happy Nat)))
prop_nestIntersectCell = nestIntersect @(Fork (Cell (Happy Nat)))
prop_unionIdCell = unionId @(Fork (Cell (Happy Nat)))
prop_intersectIdCell = intersectId @(Fork (Cell (Happy Nat)))
prop_unionAbsorbsCell = unionAbsorbs @(Fork (Cell (Happy Nat)))
prop_intersectAbsorbsCell = intersectAbsorbs @(Fork (Cell (Happy Nat)))
prop_unionCommutesCell = unionCommutes @(Fork (Cell (Happy Nat)))
prop_intersectCommutesCell = intersectCommutes @(Fork (Cell (Happy Nat)))
prop_unionAssociatesCell = unionAssociates @(Fork (Cell (Happy Nat)))
prop_intersectAssociatesCell = intersectAssociates @(Fork (Cell (Happy Nat)))
prop_unionDistributesCell = unionDistributes @(Fork (Cell (Happy Nat)))
prop_intersectDistributesCell = intersectDistributes @(Fork (Cell (Happy Nat)))
prop_doubleComplCell = doubleCompl @(Fork (Cell (Happy Nat)))
prop_complSubCell = complSub @(Fork (Cell (Happy Nat)))
prop_deMorganUnionCell = deMorganUnion @(Fork (Cell (Happy Nat)))
prop_deMorganIntersectCell = deMorganIntersect @(Fork (Cell (Happy Nat)))

prop_nestReflFunc  = nestRefl  @(Func (Happy Nat))
prop_nestTransFunc = nestTrans @(Func (Happy Nat))
prop_nestUnionFunc = nestUnion @(Func (Happy Nat))
prop_nestIntersectFunc = nestIntersect @(Func (Happy Nat))
prop_unionIdFunc = unionId @(Func (Happy Nat))
prop_intersectIdFunc = intersectId @(Func (Happy Nat))
prop_unionAbsorbsFunc = unionAbsorbs @(Func (Happy Nat))
prop_intersectAbsorbsFunc = intersectAbsorbs @(Func (Happy Nat))
prop_unionCommutesFunc = unionCommutes @(Func (Happy Nat))
prop_intersectCommutesFunc = intersectCommutes @(Func (Happy Nat))
prop_unionAssociatesFunc = unionAssociates @(Func (Happy Nat))
prop_intersectAssociatesFunc = intersectAssociates @(Func (Happy Nat))
prop_unionDistributesFunc = unionDistributes @(Func (Happy Nat))
prop_intersectDistributesFunc = intersectDistributes @(Func (Happy Nat))
prop_doubleComplFunc = doubleCompl @(Func (Happy Nat))
prop_complSubFunc = complSub @(Func (Happy Nat))
prop_deMorganUnionFunc = deMorganUnion @(Func (Happy Nat))
prop_deMorganIntersectFunc = deMorganIntersect @(Func (Happy Nat))

prop_nestReflTy    = nestRefl  @Ty
prop_nestTransTy   = nestTrans @Ty
prop_nestUnionTy   = nestUnion @Ty
prop_nestIntersectTy = nestIntersect @Ty
prop_unionIdTy = unionId @Ty
prop_intersectIdTy = intersectId @Ty
prop_unionAbsorbsTy = unionAbsorbs @Ty
prop_intersectAbsorbsTy = intersectAbsorbs @Ty
prop_unionCommutesTy = unionCommutes @Ty
prop_intersectCommutesTy = intersectCommutes @Ty
prop_unionAssociatesTy = unionAssociates @Ty
prop_intersectAssociatesTy = intersectAssociates @Ty
prop_unionDistributesTy = unionDistributes @Ty
prop_intersectDistributesTy = intersectDistributes @Ty
prop_doubleComplTy = doubleCompl @Ty
prop_complSubTy = complSub @Ty
prop_deMorganUnionTy = deMorganUnion @Ty
prop_deMorganIntersectTy = deMorganIntersect @Ty
