module Practice.DependentHoon4 where

import ClassyPrelude hiding ((/), even, find, head, join, read, tail, catch)
import Prelude (head, tail)

import Control.Arrow (left)
import Control.Exception (ArithException, catch)
import Control.Monad.Except hiding (join)
import Control.Monad.Reader hiding (join)
import Control.Monad.State.Strict hiding (join)
import Data.Maybe (fromJust)
import Data.Set (toAscList)
import qualified Data.Set as Set
import Data.Void
import System.IO.Unsafe (unsafePerformIO)

import Practice.HoonCommon hiding (Bass(..))
import Practice.Nock
import Practice.Render (Rolling)
import {-# SOURCE #-} Practice.RenderDH4Orphans ()
import Urbit.Atom (utf8Atom)

data Soft
  = Wng Wing [(Wing, Soft)]        --  a.b(c.d h, +6 j)  subject lookup and edit
  | Atm Atom Grit Aura             --  1, %foo                      atomic value
  | Cel Soft Soft                  --  [h j], [t u]    cell value, type (nondep)
  | Fac Pelt Soft                  --  s=h              face(s) applied to value
  | Lam Soft Soft                  --  |=  t  h                   function value
  | Cru (Map Term Soft)            --  |%  ++  foo  h  --             core value
  --
  | Plu Soft                       --  +(h)                     atomic increment
  | Sla Soft Soft                  --  (h j)                       function call
  | Equ Soft Soft                  --  =(h j)                      equality test
  | Tes Soft Soft Soft             --  ?:  h  j  k                boolean branch
  | Rhe Soft Soft                  --  ??  h  j                rhetorical branch
  | Fis Pelt Soft                  --  ?=(s h)                      pattern test
--  | Edi Wing [(Wing, Soft)]      --  h(a.b j, +6 k)
  --
  | Aur Aura Tool                  --  @ud,  ?(%foo, %bar)           atomic type
  | Ral Soft Soft                  --  {t u}                          sigma type
  | Gat Soft Soft                  --  $-(t u)                      pi/gate type
  | Cor Soft Soft (Map Term Soft)  --  $|  t  u  ++  foo v  --         core type
-- | God
-- | Led
  | Sin Soft Soft                  --  1|@,  $=  1  @             singleton type
  | Fus Soft Pelt                  --  $>  t  p, t?(p)                 fuse type
  | Non                            --  *                           top/noun type
  | Vod                            --  !                        bottom/void type
  | Typ                            --  $                            type of type
  --
  | Wit Soft Soft
  | Pus Soft Soft
  | Net { sof :: Soft, typ :: Soft }
  | Cat { sof :: Soft, typ :: Soft }
  deriving (Eq, Ord, Show, Generic)

data Pelt
  = Punt            -- ^ _     wildcard
  | Peer Term       -- ^ a     variable
  | Part Atom       -- ^ %foo  constant
  | Pair Pelt Pelt  -- ^ []    cons
  -- | Pons Pelt Pelt  -- ^ a=    as-pattern
  | Pest Pelt Soft  -- ^ /   patern nest
  | Past Pelt Soft  -- ^ ``  pattern cast
  deriving (Eq, Ord, Show, Generic)

-- TODO "Duet" for $@

data Code a
  = Spot Axis
  | Fore a
  --
  | Atom Atom
  | Cell (Code a) (Code a)
  | Lamb (Code a)
  | Crux (Map Term (Code a))
  --
  | Pull Term (Set Term) (Code a)
  | Plus (Code a)
  | Slam (Code a) (Code a)
  | Equl (Code a) (Code a)
  | Test (Code a) (Code a) (Code a)
  | Fish Fish (Code a)
  | Edit (Code a) Axis (Code a)
  --
  | Aura Aura Tool
  | Rail (Code a) (Code a)
  | Gate (Code a) (Code a)
  | Core (Code a) (Map Term (Code a)) (Code a)  -- FIXME user syntax fom/act
  | Sing (Code a) (Code a)
  | Face Face (Code a)
  | Fuse (Code a) Fish
  | Noun
  | Void
  | Type
  --
  | With (Code a) (Code a)
  | Push (Code a) (Code a)
  deriving (Generic, Functor, Foldable, Traversable)

deriving instance Eq   a => Eq   (Code a)
deriving instance Ord  a => Ord  (Code a)
deriving instance Show a => Show (Code a)

data Stub
  = Leg Axis
  | Arm Axis Term (Set Term)
  deriving (Eq, Ord)

instance Show Stub where
  show = \case
    Leg a      -> "+" <> show a
    Arm a ar _ -> unpack ar <> ".+" <> show a

data Tool
  = Fork (Set Atom)
  | Bowl
  deriving (Eq, Ord, Show)

-- | A layer of facial information on a type or annotating a value to change its
-- type.
--
-- A "blocking face" or "mask" is an ordinary hoon face with the poperties you
-- are familiar with. If you are looking for face f and you encounter mask g
-- (g /= f), then your search does not go into the current subtree and instead
-- skips over it, moving sideways. This is what we mean by "blocking."
--
-- A "non-blocking alias" or "link" is an extra name for an axis in the current
-- subtree that doesn't interfere with any names inside. If the face you're
-- looking for matches a link, you're done. On the other hand, if it doesn't,
-- you still proceed deep into the current subtree to look for it anyway. If
-- the link does match, the semantics are that you go to that axis, *strip off
-- any masks present*, then apply the list of faces to the outside.
--
-- Links are based on an idea of Joe for improving tistar, and may in fact be
-- used to implement tistar eventually.
data Face
  = Mask Term                       -- ^ blocking face
  | Link (Map Term (Axis, [Face]))  -- ^ non-blocking alias
  deriving (Eq, Ord, Show, Generic)

-- | Computational content of a refutable pattern. All you need to know to
-- check for match at nock/eval time.
data Fish
  = Tuna            -- ^ definite match  _
  | Sole Atom       -- ^ equality match  %foo
  | Char Fish Fish  -- ^ cellular match  [_ _]
  deriving (Eq, Ord, Show, Generic)

-- | Unlike the Peg instance below which pegs the axis below the stub, this
-- operation pegs above.
pole :: Axis -> Stub -> Stub
pole a = \case
  Leg b -> Leg (peg a b)
  Arm b ar sh -> Arm (peg a b) ar sh

type Level = Nat

type Loc = (Level, Axis)

-- | Frozen wing.
data Rump
  = Leg' Loc

instance Show Rump where
  show = \case
    Leg' (l, a) -> "+" <> show l <> "_" <> show a
--  Arm' (l, a) ar _ -> unpack ar <> ".+" <> show l <> "_" <> show a

data Hop b a
  = New b  -- ^ reference to current subject
  | Old a  -- ^ reference to outer subject
  deriving (Functor, Foldable, Traversable, Generic)

instance (Show a, Show b) => Show (Hop a b) where
  show = \case
    New x -> show x
    Old y -> show y <> "^"

deriving instance (Eq a, Eq b) => Eq (Hop a b)
deriving instance (Ord a, Ord b) => Ord (Hop a b)

-- | Check whether two leveled axes correspond to the same axis. Because I'm too
-- stupid right now, the current encoding of leveled axes has duplicate reprs.
comp :: (Level, Axis) -> (Level, Axis) -> Ordering
comp (l, a) (m, b)
  | l < m     = compare (peg (2 ^ (m + 1 - l) - 1) a) b
  | l > m     = compare a (peg (2 ^ (l + 1 - m) - 1) b)
  | otherwise = compare a b

instance Eq Rump where
  Leg' la == Leg' lb = comp la lb == EQ
  -- XX: correct? Reasoning: It is never valid for two Arm's to the same axis
  -- to have different shape.
--  Arm' la ar _ == Arm' lb br _ = comp la lb == EQ && ar == br
--  Leg'{} == Arm'{} = False
--  Arm'{} == Leg'{} = False

instance Ord Rump where
  compare (Leg' la) (Leg' lb) = comp la lb
--  compare (Arm' la ar _) (Arm' lb br _) = compare (ar, Leg' la) (br, Leg' lb)
--  compare Arm'{} Leg'{} = LT
--  compare Leg'{} Arm'{} = GT

-- | Alternative name useful for pedagogical purposes.
type Type = Semi

-- | Closure
data Jamb a = Jamb { cod :: Code a, clo :: Semi a }
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance Eq   a => Eq   (Jamb a)
deriving instance Ord  a => Ord  (Jamb a)
deriving instance Show a => Show (Jamb a)

data Semi a
  = Spot' Rump
  | Fore' a
  -- | Evaluation hold. Hold' sub cod has the semantics of (eval sub cod),
  -- except in fits where the Amber rule is applied.
  | Hold' (Semi a) (Code a)
  --
  | Atom' Atom
  | Cell' (Semi a) (Semi a)
  | Lamb' (Jamb a)
  | Crux' (Map Term (Code a)) (Semi a)
-- | Crux' or Bulk'
  --
  | Pull' Text (Set Text) (Semi a)
  | Plus' (Semi a)
  | Slam' (Semi a) (Semi a)
  | Equl' (Semi a) (Semi a)
  | Test' (Semi a) (Semi a) (Semi a)
  | Fish' Fish (Semi a)
  | Look' (Semi a) Axis
--  | Edit' (Semi a) Axis (Semi a)
  --
  | Aura' Aura Tool
  | Rail' (Semi a) (Jamb a)
  | Gate' (Semi a) (Jamb a)
  | Core'
    -- | formal payload type (to be thought of as part of the battery type)
    (Semi a)
    -- | map of Jambs to calculate arm types, with shared closure
    (Semi a, Map Term (Code a))
    -- | actual payload type
    (Semi a)
  | Sing' {- | val -} (Semi a) {- | type -} (Semi a)
  | Face' Face (Semi a)
  | Fuse' (Semi a) Fish
  | Noun'
  | Void'
  | Type'
  deriving (Generic, Functor, Foldable, Traversable)

-- | Shorthand for the boolean type, which is commonly used.
pattern Flag' :: Semi a
pattern Flag' <- Aura' "f" (Fork (toAscList -> [0, 1])) where
  Flag' = Aura' "f" (Fork (setFromList [0, 1]))

deriving instance Eq   a => Eq   (Semi a)
deriving instance Ord  a => Ord  (Semi a)
deriving instance Show a => Show (Semi a)


-- Smart constructors to handle Void' properly.

cell' :: Type a -> Type a -> Type a
cell' Void' _ = Void'
cell' _ Void' = Void'
cell' t     u = Cell' t u

rail' :: Type a -> Jamb a -> Type a
rail' Void' _ = Void'
rail' t     j = Rail' t j

gate' :: Type a -> Jamb a -> Type a
gate' Void' _ = Void'
gate' t     j = Gate' t j

core' :: Type a -> (Semi a, Map Term (Code a)) -> Type a -> Type a
core' _ _  Void' = Void'
core' t js     u = Core' t js u

sing' :: Semi a -> Type a -> Type a
sing' _ Void' = Void'
sing' s     t = Sing' s t

face :: [Face] -> Code a -> Code a
face fs b = foldr Face b fs

face' :: [Face] -> Type a -> Type a
face' fs = \case
  Void' -> Void'
  t -> foldr nose t fs
   where
    nose f t = case f of
      Link l | l == mempty -> t
      f -> Face' f t

class Peg a where
  -- | Go down according to the axis.
  (/) :: a -> Axis -> a

class Rise a where
  -- | Ascend one step.
  rise :: a -> a

instance Peg Void where
  _ / _ = error "impossible"

instance Rise Void where
  rise v = absurd v

instance Peg Wing where
 (Axis a : w) / b = Axis (a / b) : w
 w / a = Axis a : w

instance Peg Axis where
  a / b = peg a b

instance Rise Axis where
  rise a = case a of
    1 -> error "rise: too far"
    _ -> a `div` 2

instance Peg Stub where
  Leg a / b = Leg (a / b)
  Arm a ar ars / b = Arm (a / b) ar ars

instance Rise Stub where
  rise (Leg a) = Leg (rise a)
  rise (Arm a ar ars) = Arm (rise a) ar ars

instance Peg (Level, Axis) where
  (l, a) / b = (l, a / b)

instance Rise (Level, Axis) where
  rise (l, a) = (l, rise a)

instance Peg Rump where
  Leg' la / b = Leg' (la / b)
--  Arm' la ar ars / b = Arm' (la / b) ar ars

instance Rise Rump where
  rise (Leg' la) = Leg' (rise la)
--  rise (Arm' la ar ars) = Arm' (rise la) ar ars

instance (Peg a, Peg b) => Peg (Hop b a) where
  Old x / a = Old $ x / a
  New x / a = New $ x / a

instance (Rise a, Rise b) => Rise (Hop b a) where
  rise (Old x) = Old (rise x)
  rise (New x) = New (rise x)

instance (Peg a) => Peg (Semi a) where
  s / a = case (cut a, s) of
    (Nothing,     s)                   -> s
    (_,           Hold' s c)           -> Hold' s $ With c (Spot a)
    (_,           Spot' r)             -> Spot' $ r / a
    (_,           Fore' x)             -> Fore' $ x / a
    (_,           Look' c st)          -> Look' c $ st / a
--  (_,           Face' _ c)           -> walk a c  -- Faces only on types now
    (Just (L, a), Cell' s _)           -> s / a
    (Just (R, a), Cell' _ s)           -> s / a
    (Just (R, a), Crux' _ p)           -> p / a
    (Just _,      _)                   -> Look' s a

-- instance Functor f => SetFunctor f where
--   fmap = smap

type Var a = (Eq a, Ord a, Show a, Peg a)


-- Code generator --------------------------------------------------------------

-- | Compile a Code coming from work/play into Nock. Note that this does not
-- involve types at all; Code is an untyped IR and the type system is over at
-- this point.
mint :: Code Void -> Nock
mint = \case
  Spot a -> N0 a
  Fore _ -> error "mint: impossible Fore" -- XX why does ghc complain?
  --
  Atom a -> N1 (A a)
  Cell c d -> case (mint c, mint d) of
    (N1 n, N1 m) -> N1 (C n m)
    (f, g) -> NC f g
  Lamb c  -> NC (mint c)  $ NC (N1 $ A 0) (N0 1)
  Crux cs -> NC (mine cs) $ NC (N1 $ A 0) (N0 1)
  --
  Pull a as c -> N9 (loot a as) (mint c)
  Plus c -> N4 (mint c)
  Slam c d -> N9 2 $ N10 6 (mint d) $ mint c  -- XX check
  Equl c d -> N5 (mint c) (mint d)
  Test c d e -> N6 (mint c) (mint d) (mint e)
  Fish f (Spot a) -> lure a f
  Fish f c -> N7 (mint c) (lure 1 f)
  Edit bas a mod -> N10 a (mint mod) $ mint bas
  --
  -- Generate code to compute runtime type representations.
  Aura au Bowl -> N1 (C AURA $ C (A $ utf8Atom au) (A 0))
  Aura au (Fork as) -> N1 (C AURA $ C (A $ utf8Atom au) $ C (A 0) as')
    where as' = foldr C (A 0) $ map A $ setToList as
  Rail c d -> NC (N1 RAIL) $ NC (N1 $ mint c) $ NC (N1 $ mint d) (N0 1)
  Gate c d -> NC (N1 GATE) $ NC (N1 $ mint c) $ NC (N1 $ mint d) (N0 1)
  Core{} -> A 424242  -- FIXME finalize Core' layout
  Sing c d -> NC (N1 SING) $ NC (mint c) (mint d)
  Face (Mask m) c -> NC (N1 FACE) $ NC (A $ utf8Atom m) (mint c)
  -- FIXME, also strip on empty link to avoid colliding with empty mask.
  Face (Link l) c -> NC (N1 FACE) $ NC (C (A 999) (A 888)) (mint c)
  Fuse c f -> NC (N1 FUSE) $ NC (mint c) (land f)
  Noun -> N1 NOUN
  Void -> N1 VOID
  Type -> N1 TYPE
  --
  With c d -> N7 (mint c) (mint d)
  Push c d -> N8 (mint c) (mint d)

pattern AURA = A 1_634_891_105
pattern RAIL = A 1_818_845_554
pattern GATE = A 1_702_125_927
pattern CORE = A 1_701_998_435
pattern SING = A 1_735_289_203
pattern FACE = A 1_701_011_814
pattern FUSE = A 1_702_065_510
pattern NOUN = A 1_853_189_998
pattern VOID = A 1_684_631_414
pattern TYPE = A 1_701_869_940

-- | Find the axis of an arm in a core.
loot :: Term -> Set Term -> Axis
loot _ _ = 1338  -- FIXME

-- | Build the formula tree for the battery of a core.
-- FIXME
mine :: Map Term (Code Void) -> Noun
mine bat = foldr step (A 0) $ mapToList bat
 where
  step (_, cod) bod = C (mint cod) bod

-- | Compile pattern tests. FIXME add .? for $@
lure :: Axis -> Fish -> Nock
lure ax = \case
  Tuna -> N1 $ A 0
  Sole a -> N5 (N0 ax) (N1 $ A a)
  Char f g -> case (lure (ax / 2) f, lure (ax / 3) g) of
    (N1 (A 0), n) -> n
    (n, N1 (A 0)) -> n
    (n,        m) -> flan n m

-- | Implement logical and. Old Hoon does some extra stuff for some reason.
flan :: Nock -> Nock -> Nock
flan bos nif = N6 bos nif (N1 $ A 1)

-- | Represent fish in runtime type representation.
land :: Fish -> Noun
land = \case
  Tuna -> A 0
  Sole a -> A (a + 1)
  Char f g -> C (land f) (land g)

-- Evaluator -------------------------------------------------------------------

-- | Read a seminoun representing what we know about the *values* of the subject
-- from the type of the subject. Where we don't know anything, the given
-- seminoun is used to generate unknowns.
read :: Var a => Semi a -> Type a -> Semi a
read nul = \case
  Sing' s _ -> s
  Fuse' t h -> skim (read nul t) h
  Aura' au (Fork ss) | [a] <- setToList ss -> Atom' a
  Face' _ t -> read nul t
  Test' b t f -> Test' b (read nul t) (read nul f)   -- XX correct?
  Cell' t u -> Cell' (read (nul / 2) t) (read (nul / 3) u)
  Rail' t j -> Cell' lef rit
  -- XX Core'?
   where
    lef = read (nul / 2) t
    rit = read (nul / 3) $ jamb j lef
  _ -> nul

-- | Refine the given seminoun according to the given fish.
skim :: Var a => Semi a -> Fish -> Semi a
skim ken = \case
  Tuna -> ken
  Sole a -> Atom' a
  Char f g -> Cell' (skim (ken / 2) f) (skim (ken / 3) g)

--- | A common choice for the fallback value in read, a Spot at the given loc.
rump :: Var a => (Level, Axis) -> Semi a
rump = Spot' . Leg'

jamb :: Var a => Jamb a -> Semi a -> Semi a
jamb Jamb{..} arg = eval (Cell' arg clo) cod

{-
-- | Axially project a value; i.e. implement Nock 0 or 9.
look :: forall a. Var a => Stub -> Semi a -> Semi a
look st b = home $ b / a
 where
  a = case st of
    Leg a -> a
    Arm a _ _ -> a

  home :: Semi a -> Semi a
  home b = case st of
    Leg _ -> b
    Arm _ ar ars -> case b of
      Crux' cs _ | Just c <- lookup ar cs -> Hold' b c
      stuck -> Look' stuck (Arm 1 ar ars)
-}

-- | Change the part of a value found at the given axis.
edit :: Var a => Semi a -> Axis -> Semi a -> Semi a
edit bas a mod = case (cut a, bas) of
  (Nothing, s) -> mod
  (Just (R, a), Crux' bat pay) -> Crux' bat (edit pay a mod)
  (Just (L, a), _) -> Cell' (edit (bas / 2) a mod) (bas / 3)
  (Just (R, a), _) -> Cell' (bas / 2) (edit (bas / 3) a mod)

hook :: Fish -> Semi a -> Maybe Bool
hook h b = case (h, b) of
  (Tuna, _) -> Just True
  (Sole a, Atom' b)
    | a == b    -> Just True
    | otherwise -> Just False
  (Sole a, Cell'{}) -> Just False
  (Sole{}, _) -> Nothing
  (Char{}, Atom'{}) -> Just False
  (Char h j, Cell' b c) -> (&&) <$> hook h b <*> hook j c
  -- I guess I could also let you see that gates are cells, but I don't want
  -- to right now?
  (Char{}, _) -> Nothing

-- | Refine the given type to its subtype of values which match the given
-- pattern. The computed nature of fuses allows for a particularly elegant
-- implementation of the (Rail', Char) case, which you should study. To hide
-- the runtime representation of gates and types, and avoid particular
-- complexity, these are regarded as equivalent to Cell' Noun' Noun' herein.
fuse :: Var a => Type a -> Fish -> Type a
fuse typ fis = case (typ, fis) of
  (_,                     Tuna)      -> typ
  -- arguably Fuse is an eliminator and this should be treated in `held`.
  ((held -> Just t),      _)         -> fuse t fis
  (Aura' au Bowl,         Sole a)    -> Aura' au (Fork $ singleton a)
  (Aura' au (Fork as),    Sole a)
    | a `elem` as                    -> Aura' au (Fork (singleton a))
    | otherwise                      -> Void'
  (Aura' _ _,             Char{})    -> Void'
  (Cell' t u,             Char f j)  -> cell' (fuse t f) (fuse u j)
  (Cell' _ _,             Sole{})    -> Void'
  (Rail' t Jamb{cod,clo}, Char f j)  -> rail' (fuse t f) Jamb { cod = Fuse cod j
                                                              , clo
                                                              }
  (Rail' _ _,             Sole{})    -> Void'
  (Gate' _ _,             _)         -> fuse (Cell' Noun' Noun') fis
  (Core' _ _ act,         _)         -> fuse (Cell' Noun' act) fis
  -- XX think about Joe vs non-Joe below.
  -- Joe wants =/ to behave like |= as far as exhaustiveness/fuse go. E.g.
  -- he wants to be able to do ?-(1 1 ~, 2 ~, 3 ~). I think he also feels
  -- differently for %1 scrutinee.
  --
  -- Joe rule. We must strip off the singleton entirely to avoid refining to
  -- nonsense like [%foo 1 2]|[$bar @].
  (Sing' s t,             _)         -> fuse t fis
  {-
  -- Non-Joe rule.
  (Sing' s t,             _)         -> case hook fis s of
                                          Just True  -> fuse t fis
                                          Just False -> Void'
                                          -- being stuck here makes subject
                                          -- refinement impossible for
                                          -- nontrivial scrutinees
                                          Nothing    -> Fuse' typ fis -}
  (Face' f t,             _)         -> face' [f] (fuse t fis)
  (Fuse' t f,             _)         -> case swim f fis of
                                          Just j  -> Fuse' t j
                                          Nothing -> Void'
  (Noun',                 Sole a)    -> Aura' "" (Fork $ singleton a)
  (Noun',                 Char f j)  -> Cell' (fuse Noun' f) (fuse Noun' j)
  (Void',                 _)         -> Void'
  (Type',                 _)         -> fuse (Cell' Noun' Noun') fis
  -- Stuck types.
  (_,                     _)         -> Fuse' typ fis

eval :: Var a => Semi a -> Code a -> Semi a
eval sub = \case
  Spot s -> sub / s
  Fore x -> Fore' x
  --
  Atom a -> Atom' a
  -- XX cannot use cell' here because consider e.g. [! 1]. This is not !.
  -- A possibly serious drawback of cell/cons unification.
  Cell c d -> Cell' (eval sub c) (eval sub d)
  Lamb a -> Lamb' (Jamb a sub)
  Crux as -> Crux' as sub
  --
  Pull a ar c -> pull a ar (eval sub c)

  Plus c -> plus (eval sub c)
  Slam c d -> slam (eval sub c) (eval sub d)
  Equl c d -> equl (eval sub c) (eval sub d)
  Test c d e -> test (eval sub c) (eval sub d) (eval sub e)  -- Laziness!
  Fish f c -> fisk f (eval sub c)
  Edit c a d -> edit (eval sub c) a (eval sub d)
  --
  Aura au to -> Aura' au to
  Rail c d -> rail' (eval sub c) (Jamb d sub)
  Gate c d -> gate' (eval sub c) (Jamb d sub)
  Core c ds e -> core' (eval sub c) (sub, ds) (eval sub e)
  Sing c d -> sing' (eval sub c) (eval sub d)
  Face f c -> face' [f] (eval sub c)
  Fuse c f -> fuse (eval sub c) f
  Noun -> Noun'
  Void -> Void'
  Type -> Type'
  With c d -> eval (eval sub c) d
  Push c d -> eval (Cell' (eval sub c) sub) d

-- | Implement the Pull eliminator.
pull :: Text -> Set Text -> Semi a -> Semi a
pull ar ax b = case b of
  Crux' cs _ | Just c <- lookup ar cs -> Hold' b c
  x -> Pull' ar ax x

-- | Implement the Plus eliminator.
plus :: Semi a -> Semi a
plus = \case
  Atom' a -> Atom' (a + 1)
  x -> Plus' x

-- | Implement the Slam eliminator.
slam :: Var a => Semi a -> Semi a -> Semi a
slam x y = case x of
  Lamb' j -> jamb j $ y
  x -> Slam' x y

-- | Implement the Equl eliminator.
equl :: Var a => Semi a -> Semi a -> Semi a
equl x y
  | x == y    = Atom' 0
  | otherwise = Equl' x y

-- | Implement the Test eliminator.
test :: Semi a -> Semi a -> Semi a -> Semi a
test x y z = case x of
  Atom' 0 -> y
  Atom' 1 -> z
  x       -> Test' x y z

-- | Implement the Fish eliminator.
fisk :: Fish -> Semi a -> Semi a
fisk h b = case hook h b of
  Just True  -> Atom' 0
  Just False -> Atom' 1
  Nothing    -> Fish' h b

-- | A seminoun is *held* or *advanceable* if it is a Hold' or a bunch of
-- eliminators stacked on a Hold'. Because the evaluator "pauses" at these cases
-- consumers of seminouns need to be aware of them and be prepared to advance
-- them. This function returns the advanced version of the seminoun if it
-- advanceable, nothing otherwise.
--
-- On first blush, it might seem like a valid strategy for solving this problem
-- would be to "push" any eliminator into the hold during evaluation. For
-- example when c ~> Hold s c', one would evaluate Plus c ~> Hold s (Plus c).
-- Then one need only check for the Hold' at the outermost layer of the Semi.
-- The problem with this approach is that some eliminators, like Slam and Test,
-- have additional subexpressions, and these subexpressions, if moved under the
-- hold, will later be evaluated against the *wrong* subject.
held :: Var a => Semi a -> Maybe (Semi a)
held = \case
  Hold' s c     -> Just (eval s c)
  Pull' ar ax s -> pull ar ax <$> held s
  Plus' s       -> plus <$> held s
  Slam' s t     -> slam <$> held s <*> pure t
  Equl' s t     -> equl <$> held s <*> pure t  -- advance one step at a time
               <|> equl <$> pure s <*> held t
  Test' s t u   -> test <$> held s <*> pure t <*> pure u
  Fish' f s     -> fisk f <$> held s
  Look' s a     -> (/ a) <$> held s
  --
  Fuse' s f     -> fuse <$> held s <*> pure f
  --
  _ -> Nothing

loft :: Var a => Level -> Semi a -> Code a
loft lvl = \case
  -- XX we should have some printout here if lvl < l, which is a serious
  -- invariant violation that should be more legible
  Spot' (Leg' (l, a)) -> Spot ax
   where
    ax = if lvl < l then 9001 else peg (2 ^ (lvl + 1 - l) - 1) a
  Fore' x -> Fore x
  -- XX no longer a split morphism. Consider Hold' Term Crux
  Hold' s c -> With (loft lvl s) c
  --
  Atom' a -> Atom a
  Cell' a b -> Cell (loft lvl a) (loft lvl b)
  Lamb' j -> Lamb $ luft lvl j
  -- cf the discussion on luft below. Otoh, I think we do want contexts printed
  -- for cruxen.
  Crux' ars pay -> With (loft lvl pay) (Crux ars)
  --
  Pull' x xr a -> Pull x xr (loft lvl a)
  Plus' a -> Plus (loft lvl a)
  Slam' a b -> Slam (loft lvl a) (loft lvl b)
  Equl' a b -> Equl (loft lvl a) (loft lvl b)
  Test' a b c -> Test (loft lvl a) (loft lvl b) (loft lvl c)
  Fish' h a -> Fish h (loft lvl a)
  Look' a s -> With (loft lvl a) $ Spot s
  --
  Aura' au to -> Aura au to
  Rail' l j -> Rail (loft lvl l) (luft lvl j)
  Gate' a j -> Gate (loft lvl a) (luft lvl j)
  Core' a (s, as) b ->
    Core (loft lvl a) (fmap (luft lvl . (`Jamb` s)) as) (loft lvl b)
  Sing' a b -> Sing (loft lvl a) (loft lvl b)
  Face' f t -> Face f (loft lvl t)
  Fuse' t h -> Fuse (loft lvl t) h
  Noun' -> Noun
  Void' -> Void
  Type' -> Type

-- XX once we have %= this will be wrong. Consider:
--   =+  a=2
--   |=  ...
--   =.  a  3
--   a
-- The old encoding using With was correct, but leads to unpleasant printouts.
-- A possible resolution here is to use a different loft for printouts, which
-- checks for the presence of %= when deciding to inline.
--
-- More broadly, this leads to questions about how %= will interact with
-- alternate seminoun representations. (To be clear, it works fine with the
-- current one. The problem is that in some sense %= *mandates* a closure-based
-- encoding, because it purports to edit the closure.) But many other encodings
-- would sidestep the need to loft on the semantic path entirely.
luft l j = loft (l + 1) $ jamb j $ rump (l + 1, 2)

-- | Given a Code coming straight out of the compiler, read the subject type
-- and evaluate against the resulting seminoun.
evil :: Var a => Con a -> Code Void -> Semi a
evil Con{lvl, sut} cod = eval (read (rump (lvl, 1)) sut) (fmap absurd cod)

-- | Given a semi/type possibly stuck on references to the outer subject, read
-- it back into nockable code (Code Void), failing if there are in fact such
-- references. A type which has no outer references is called "fair"; the others
-- are "unfair."
fair :: (MonadCheck m, Var a) => Con a -> Semi a -> m (Code Void)
fair Con{lvl} ken = loft lvl <$> traverse (bail . FairFore ken) ken


-- The fish calculus -----------------------------------------------------------

-- | One fish can eat another
prey :: Fish -> Fish -> Bool
prey big lit = case (big, lit) of
  (Tuna, _)              -> True
  (Sole a, Sole b)
    | a == b             -> True
    | otherwise          -> False
  (Sole{}, _)            -> False
  (Char h j, Char h' j') -> prey h h' && prey j j'
  (Char{}, _)            -> False

-- | Fish can evolve to become land-dwelling animals
-- XX Not used
lung :: Var a => Fish -> Semi a -> Maybe (Semi a)
lung fis ken = case (fis, ken) of
  (Tuna, _)         -> Just ken
  (Sole a, Atom' b)
    | a == b        -> Just (Atom' b)
    | otherwise     -> Nothing
  (Sole{}, Cell'{}) -> Nothing
  (Sole a, _)       -> Just (Atom' a)
  (Char{}, Atom'{}) -> Nothing
  (Char h j, _)     -> Cell' <$> lung h (ken / 2) <*> lung j (ken / 2)

-- | Intersection of fishes
swim :: Fish -> Fish -> Maybe Fish
swim lef rit = case (lef, rit) of
  (Tuna, h) -> pure h
  (h, Tuna) -> pure h
  (Sole a, Sole b)
    | a == b    -> pure $ Sole a
    | otherwise -> Nothing
  (Sole{}, Char{}) -> Nothing
  (Char{}, Sole{}) -> Nothing
  (Char h j, Char h' j') -> Char <$> swim h h' <*> swim j j'

-- | All pairwise intersections
swam :: Set Fish -> Set Fish -> Set Fish
swam fs gs
  | Tuna `elem` fs = gs
  | Tuna `elem` gs = fs
  | otherwise = setFromList do
      f <- setToList fs
      g <- setToList gs
      case swim f g of
        Nothing -> []
        Just h -> [h]

-- | Subtract those fishes that are compatible with the given fish.
cull :: Fish -> Set Fish -> Set Fish
cull fis = setFromList . filter (not . prey fis) . setToList

-- | Form a map of left patterns to right patterns
-- Does not decompose Tuna so you need to check for that yourself first!
dive :: Set Fish -> Map Fish (Set Fish)
dive fis = mapFromList
  [ ( l
    , setFromList
        [ r
        | Char l' r <- setToList fis
        , prey l l'
        ]
    )
  | Char l  _ <- setToList fis
  ]

-- Get left/right subpatterns. Discard noncells.
slip :: Step -> Set Fish -> Set Fish
slip dir fs = setFromList do
  f <- setToList fs
  case (dir, f) of
    (_, Tuna) -> [Tuna]
    (_, Sole _) -> []
    (L, Char l _) -> [l]
    (R, Char _ r) -> [r]

-- | Enlarge the given fish to cover a bigger noun.
gill :: Axis -> Fish -> Fish
gill a f = case cut a of
  Nothing -> f
  Just (L, a) -> Char (gill a f) Tuna
  Just (R, a) -> Char Tuna (gill a f)

-- To make the below, Tuna must be annotated with all possibilities, or we have
-- an "or" pattern and forbid Tuna, using type checker to enumerate, but also
-- somehow undoing this in code generation? Do we need yet another type?
-- We could parametrize tuna, use Fish (Set (Fish Void)) here and Fish () there?
-- In the presence of a type one can convert from the latter to the former?
-- Want also to avoid explosions but I think this can be solved by nesting tunas

{-
-- | Two fish can swim together
herd :: Fish -> Fish -> Fish
herd = undefined

-- | Fish can be truant
fear :: Fish -> Fish -> [Fish]
fear = undefined
-}


-- The type checking monad -----------------------------------------------------

class (Monad m, MonadIO m, Alternative m) => MonadCheck m where
  -- | Push an error reporting stack frame.
  act :: (Show a, Rolling a) => Act -> m a -> m a

  -- | Report an error.
  bail :: Fail -> m a

  -- | Change the error message of the error, if any, that occurs within the
  -- computation.
  bailSwap :: (Fail -> Fail) -> m a -> m a

  -- | Leave a message to be embedded in the trace. Has no effect in non-tracing
  -- modes.
  note :: (Show a, Rolling a) => Text -> a -> m ()

  -- | Get the success or failure of a given nested check, allowing for
  -- recovery / fallback.
  ogle :: m a -> m (Either Fail a)

-- | Fail with no message.
bailFail :: MonadCheck m => m a
bailFail = bail BailFail

-- | Error reporting context, analogous to stack trace item. As the compiler
-- recurses deeper into its operations, it pushes these descriptions to a stack
-- so they can serve as breadcrumbs in error messages.
data Act
  =                    ActRoot
  | forall a. Var a => ActFits Fit (Type a) (Type a)
  | forall a. Var a => ActSeal (Line a)
  | forall a. Var a => ActFind (Level, Axis) (Type a) Wing
  | forall a. Var a => ActFuse (Level, Axis) (Type a) Fish
  | forall a. Var a => ActCrop (Level, Axis) (Type a) Fish
  | forall a. Var a => ActToil (Con a) (Level, Axis) Pelt (Type a)
  | forall a. Var a => ActTore (Cube a)
  | forall a. Var a => ActTear Claw (Cube a)
  | forall a. Var a => ActTyre (Cube a)
  | forall a. Var a => ActTire Level (Set Fish) (Type a)
  | forall a. Var a => ActWork (Con a) Fit Soft (Type a)
  | forall a. Var a => ActPlay (Con a) Soft
  |                    ActDone

-- | Compiler errors.
data Fail
  = forall a. Var a => FairFore (Semi a) a
  -- | Invariant violation: unknown seminoun on exiting tisgar.
  | forall a. Var a => PareFree Rump (Semi (Hop Rump a))
  -- | Trying to refine the type of an arm.
  |                    SealCore Text
  -- | Cannot locate the given ally in the subject.
  | forall a. Var a => FindFail Limb (Type a)
  -- | Two cores or core types have differing arm sets.
  |                    FarmCore (Set Text) (Set Text)
  -- | The two types do not {nest, cast, equal each other}.
  | forall a. Var a => FitsFail Fit (Type a) (Type a)
  -- | Your fish is not compatible with any fish in the fork
  -- ClamFork (Fish) (Set Fish)
  -- | Your pelt performs a test, which is not permitted in this context.
  | forall a. Var a => ToilFish Pelt (Type a)
  -- | Here is a case you failed to consider in your pattern matching.
  |                    TireFish Axis Fish
  -- | You are trying to edit the return value of an arm pull
  | forall a. Var a => EditPull Wing (Type a)
  -- | You are trying to slam something which is not a gate.
  | forall a. Var a => NeedGate (Type a)
  -- | A rhetorical question had a non-rhetorical answer.
  | forall a. Var a => WorkMiss Soft (Semi a)
  -- | A rhetorical question had a non-rhetorical answer.
  | forall a. Var a => PlayMiss Soft (Semi a)
  | BailNote Text  -- ^ failure with note
  | BailFail  -- ^ unspecified failure

instance Semigroup Fail where
  _ <> f = f  -- report last failure in fallback list

instance Monoid Fail where
  mempty = BailNote "mempty"

deriving instance (Show Act)
deriving instance (Show Fail)

-- | Basic type checking monad.
type Check a = ExceptT ([Act], Fail) (ReaderT [Act] IO) a

-- | Run the computation in basic type checking mode
runCheck :: Check a -> Either ([Act], Fail) a
runCheck chk = unsafePerformIO $ runReaderT (runExceptT chk) []

{-
instance Alternative Check where
  empty = bailFail
  Check (ExceptT (ReaderT c)) <|> Check (ExceptT (ReaderT d)) =
    Check $ ExceptT $ ReaderT \r -> c r <> d r-}

catchCheck :: Exception e => Check a -> (e -> Check a) -> Check a
catchCheck (ExceptT (ReaderT c)) h = ExceptT $ ReaderT \r -> c r `catch` \e ->
  runReaderT (runExceptT $ h e) r

instance MonadCheck (ExceptT ([Act], Fail) (ReaderT [Act] IO)) where
  act a chk = local (a:) $ catchCheck chk
    \(e::ArithException) -> bail (BailNote $ tshow e)
  bail f = ask >>= \as -> throwError (as, f)
  bailSwap f chk = ExceptT $ ReaderT \r ->
    runReaderT (runExceptT chk) r <&> \case
      Left (acts, err) -> Left (acts, f err)
      Right x -> Right x
  note _ _ = pure ()
  ogle chk = ExceptT $ ReaderT \r ->
    runReaderT (Right . left snd <$> runExceptT chk) r

data ActTree
  = ActTree Act [ActTree]  -- ^ most recent act at front
  | forall a. (Show a, Rolling a) => ActExit Act a  -- ^ last part of act
  | forall a. (Show a, Rolling a) => ActNote Text a

deriving instance Show ActTree

type Trace a = ExceptT Fail (StateT [ActTree] IO) a

runTrace :: Trace a -> (ActTree, Either Fail a)
runTrace tac = (tree zipper, res)
 where
  (res, zipper) = unsafePerformIO $
    runStateT (runExceptT tac) [ActTree ActRoot []]

  tree zz = foldl' insertTree (ActTree ActDone []) zz

catchTrace :: Exception e => Trace a -> (e -> Trace a) -> Trace a
catchTrace (ExceptT (StateT st)) h = ExceptT $ StateT \s -> st s `catch` \e ->
  runStateT (runExceptT $ h e) s

insertTree :: ActTree -> ActTree -> ActTree
insertTree inner _outer@(ActTree a cs) = ActTree a (inner : cs)
insertTree _ ActNote{} = error "I can't be bothered to write safe printf code"
insertTree _ ActExit{} = error "I can't be bothered to write safe printf code"

instance MonadCheck (ExceptT Fail (StateT [ActTree] IO)) where
  act a m = do
    modify' (ActTree a [] :)
    res <- m
      `catchTrace` \(e::ArithException) -> bail (BailNote $ tshow e)
    modify' \(curAct:rest) -> insertTree (ActExit a res) curAct : rest
    modify' \(curAct:prevAct:rest) -> insertTree curAct prevAct : rest
    pure res
  bail = throwError
  bailSwap f m = catchError m (\e -> throwError (f e))
  note t n = modify' \(outer:rest) -> insertTree (ActNote t n) outer : rest
  ogle chk = ExceptT $ StateT \s -> runStateT (Right <$> runExceptT chk) s

traceToStack :: ActTree -> [Act]
traceToStack = reverse . go
 where
  go = \case
    ActNote _ _ -> []
    ActExit _ _ -> []
    ActTree ActDone [] -> []
    ActTree a [] -> [a]
    ActTree ActRoot (t:_) -> go t
    ActTree a (t:_) -> a : go t


-- Context management ----------------------------------------------------------

-- | What we know about the subject
data Con a = Con
  { lvl :: Level   -- ^ number of =+s we have passed under since =>
  , sut :: Type a  -- ^ type of the subject
  }

deriving instance (Show a) => Show (Con a)

hide :: Con a -> Type a -> Con a
hide Con{lvl, sut} typ = Con
  { lvl = lvl + 1
  , sut = Cell' typ sut
  }

shew :: Var a => Con a -> Semi a -> Type a -> Con a
shew Con{lvl, sut} ken typ = Con
  { lvl = lvl + 1
  , sut = Cell' (Sing' ken typ) sut
  }

-- | Grow the type because we have passed under a tisgar
grow :: forall a. Var a => Type a -> Type (Hop Rump a)
grow = \case
  Spot' r -> Fore' (New r)
  Fore' x -> Fore' (Old x)
  Hold' s c -> Hold' (grow s) (crow c)
  --
  Atom' a -> Atom' a
  Cell' x y -> Cell' (grow x) (grow y)
  Lamb' j -> Lamb' (jrow j)
  Crux' cs s -> Crux' (fmap crow cs) (grow s)
  --
  Pull' ar ars x -> Pull' ar ars (grow x)
  Plus' x -> Plus' (grow x)
  Slam' x y -> Slam' (grow x) (grow y)
  Equl' x y -> Equl' (grow x) (grow y)
  Test' x y z -> Test' (grow x) (grow y) (grow z)
  Fish' f x -> Fish' f (grow x)
  Look' x st -> Look' (grow x) st
  --
  Aura' au to -> Aura' au to
  Rail' x j -> Rail' (grow x) (jrow j)
  Gate' x j -> Gate' (grow x) (jrow j)
  Core' x (s, j) y -> Core' (grow x) (grow s, fmap crow j) (grow y)
  Sing' x y -> Sing' (grow x) (grow y)
  Face' f x -> Face' f (grow x)
  Fuse' x f -> Fuse' (grow x) f
  Noun' -> Noun'
  Void' -> Void'
  Type' -> Type'

 where
  jrow :: Jamb a -> Jamb (Hop Rump a)
  jrow (Jamb c t) = Jamb (crow c) (grow t)

  crow :: Code a -> Code (Hop Rump a)
  crow = \case
    -- lookups into the closure are NOT changed; this is okay because the stuck
    -- seminoun references in the value in the clousre ARE.
    Spot a -> Spot a
    Fore x -> Fore (Old x)
    --
    Atom a -> Atom a
    Cell c d -> Cell (crow c) (crow d)
    Lamb c -> Lamb (crow c)
    Crux cs -> Crux (fmap crow cs)
    --
    Pull ar ars c -> Pull ar ars (crow c)
    Plus c -> Plus (crow c)
    Slam c d -> Slam (crow c) (crow d)
    Equl x y -> Equl (crow x) (crow y)
    Test x y z -> Test (crow x) (crow y) (crow z)
    Fish f x -> Fish f (crow x)
    Edit c a d -> Edit (crow c) a (crow d)
    --
    Aura au to -> Aura au to
    Rail c d -> Rail (crow c) (crow d)
    Gate c d -> Gate (crow c) (crow d)
    Core c ds e -> Core (crow c) (fmap crow ds) (crow e)
    Sing c d -> Sing (crow c) (crow d)
    Face f c -> Face f (crow c)
    Fuse c f -> Fuse (crow c) f
    Noun -> Noun
    Void -> Void
    Type -> Type
    --
    With c d -> With (crow c) (crow d)
    Push c d -> Push (crow c) (crow d)

-- | On exiting a tisgar, pare down the type to remove any opaque references to
-- the inner subject, but actually it's an invariant violation for any to exist.
pare :: forall m a. (MonadCheck m, Var a) => Semi (Hop Rump a) -> m (Semi a)
pare bas = go bas
 where
  go = \case
    Spot' l -> bail (PareFree l bas)
    Fore' (New l) -> pure $ Spot' l
    Fore' (Old x) -> pure $ Fore' x
    Hold' s c -> Hold' <$> go s <*> care c
    --
    Atom' a -> pure $ Atom' a
    Cell' x y -> Cell' <$> go x <*> go y
    Lamb' j -> Lamb' <$> jare j
    Crux' cs s -> Crux' <$>traverse care cs <*> go s
    --
    Pull' ar ars x -> Pull' ar ars <$> go x
    Plus' x -> Plus' <$> go x
    Slam' x y -> Slam' <$> go x <*> go y
    Equl' x y -> Equl' <$> go x <*> go y
    Test' x y z -> Test' <$> go x <*> go y <*> go z
    Fish' f x -> Fish' f <$> go x
    Look' x st -> flip Look' st <$> go x
    --
    Aura' au to -> pure $ Aura' au to
    Rail' x j -> Rail' <$> go x <*> jare j
    Gate' x j -> Gate' <$> go x <*> jare j
    Core' x (s, j) y ->
      Core' <$> go x <*> ((,) <$> go s <*> traverse care j) <*> go y
    Sing' x y -> Sing' <$> go x <*> go y
    Face' f x -> Face' f <$> go x
    Fuse' x f -> Fuse' <$> go x <*> pure f
    Noun' -> pure Noun'
    Void' -> pure Void'
    Type' -> pure Type'

  jare :: Jamb (Hop Rump a) -> m (Jamb a)
  jare (Jamb c s) = Jamb <$> care c <*> go s

  care :: Code (Hop Rump a) -> m (Code a)
  care = \case
    -- This stays put because it's actually an axis into the stored closure.
    Spot a -> pure $ Spot a
    Fore (New l) -> bail (PareFree l bas)
    Fore (Old x) -> pure $ Fore x
    --
    Atom a -> pure $ Atom a
    Cell c d -> Cell <$> care c <*> care d
    Lamb c -> Lamb <$> care c
    Crux cs -> Crux <$> traverse care cs
    --
    Pull ar ars c -> Pull ar ars <$> care c
    Plus c -> Plus <$> care c
    Slam c d -> Slam <$> care c <*> care d
    Equl c d -> Equl <$> care c <*> care d
    Test c d e -> Test <$> care c <*> care d <*> care e
    Fish f c -> Fish f <$> care c
    Edit c a d -> Edit <$> care c <*> pure a <*> care d
    --
    Aura au to -> pure $ Aura au to
    Rail c d -> Rail <$> care c <*> care d
    Gate c d -> Gate <$> care c <*> care d
    Core c ds e -> Core <$> care c <*> traverse care ds <*> care e
    Sing c d -> Sing <$> care c <*> care d
    Face f c -> Face f <$> care c
    Fuse c f -> Fuse <$> care c <*> pure f
    Noun -> pure Noun
    Void -> pure Void
    Type -> pure Type
    --
    With c d -> With <$> care c <*> care d
    Push c d -> Push <$> care c <*> care d


--------------------------------------------------------------------------------
-- Core operations of the compiler ---------------------------------------------
--------------------------------------------------------------------------------

-- The calculus of types -------------------------------------------------------

-- | Strip metadata off of outside of type
repo :: Type a -> Type a
repo = \case
  Sing' _ t -> repo t
  Face' _ t -> repo t
  Void' -> Void'
  a@Aura'{} -> a
  c@Cell'{} -> c
  c@Rail'{} -> c
  _ -> Noun'


-- | Try to calculate union of types
join :: (MonadCheck m, Var a) => Type a -> Type a -> m (Type a)
join _ _ = bail $ BailNote "join: Not implemented. Please put a ^- on your ?:"

-- | Try to calculate intersection of types
meet :: (MonadCheck m, Var a) => Type a -> Type a -> m (Type a)
meet _ _ = bail $ BailNote "meet: Not implemented. Please put a ^- on your ?:"


-- | State for tracking the the subtyping relations of recursive types.
-- alternate name: mesh
data Lace a = Lace
  { seg :: Set (Type a)          -- ^ recursion points we have passed on left
  , reg :: Set (Type a)          -- ^ recursion points we have passed on right
  , gil :: Set (Type a, Type a)  -- ^ induction hypotheses
  }

deriving instance (Show a) => Show (Lace a)

{-
laceMap :: (Ord a, Ord b) => (a -> b) -> Lace a -> Lace b
laceMap f Lace{seg, reg, gil} = Lace
  { seg = setFromList $ map f         $ setToList seg
  , reg = setFromList $ map f         $ setToList reg
  , gil = setFromList $ map (f *** f) $ setToList gil
  }
-}

-- | Perform subtyping, coercibility, or equality check.
-- XX figure out proper encoding of recursion via cores or gates
-- XX figure out how seminouns should apply here, if at all
-- XX handle Sing/Fuse here properly. The effect should be that when we
-- encounter either at the head of a type, we read both types, check
-- "compatibility" of the seminouns (e.g. [2 +2] is compatible with [+3 +2]) and
-- then set a flag that causes all interior fuse/sing to be ignored, until it is
-- unset by e.g. passing under gate or something. Should the compatibility check
-- reuse nest? No. Because the type [* *] does not nest under the type [* +2].
-- So it's totally different. Can it be encoded as a Fit? I think so, as "Part",
-- a variant of Same, although instead of passing to Same, internals must not.
fits :: forall a m. (MonadCheck m, Var a)
     => Fit -> Level -> Type a -> Type a -> m ()
fits fit lvl t u = void $ fest fit lvl t u Lace
  { seg = mempty
  , reg = mempty
  , gil = mempty
  }

farm :: forall a b r m. (MonadCheck m)
     => Map Text a -> Map Text b -> r -> (Text -> a -> b -> r -> m r) -> m r
farm a b state act = do
  let arms = keysSet a
  let brms = keysSet b
  when (arms /= brms) $ bail (FarmCore arms brms)
  foldlM step state arms
 where
  step state arm = do
    let x = fromJust $ lookup arm a
    let y = fromJust $ lookup arm b
    act arm x y state

fest :: forall a m. (MonadCheck m, Var a)
     => Fit -> Level -> Type a -> Type a -> Lace a -> m (Lace a)
fest fit lvl t u ace@Lace{seg, reg, gil} = act (ActFits fit t u) case (t, u) of
  _ | (t, u) `elem` gil -> pure ace
--  | t      `elem` seg -> fitsFail
--  | u      `elem` reg -> pure ace  --  ?!?!

  {-
  -- XX The held logic subsumes these cases, so they should probably be removed.
  (Hold' s c, u)
    | (s, c) `elem` seg -> fitsFail  -- XX correct to do this? not in TAPL
    | otherwise         -> fest fit lvl (eval s c) u Lace
        { seg = insertSet (s, c) seg
        , reg
        , gil = insertSet (t, u) gil
        }

  (t, Hold' s c)
    | (s, c) `elem` reg -> fitsFail
    | otherwise         -> fest fit lvl t (eval s c) Lace
        { seg
        , reg = insertSet (s, c) reg
        , gil = insertSet (t, u) gil
        }
  -}

  _ | Just t' <- held t ->
        fest fit lvl t' u Lace
          { seg = insertSet t seg
          , reg
          , gil = insertSet (t, u) gil
          }

  _ | Just u' <- held u ->
        fest fit lvl t u' Lace
          { seg
          , reg = insertSet u' reg
          , gil = insertSet (t, u) gil
          }

  (Hold'{}, _) -> error "fits: invariant violation: Hold' should be held"
  (_, Hold'{}) -> error "fits: invariant violation: Hold' should be held"

  (Face' _ t, u) -> fest fit lvl t u ace
  (t, Face' _ u) -> fest fit lvl t u ace

  (Noun', Noun') -> pure ace
  (Noun', _) -> fitsFail
  (_, Noun') -> case fit of
    FitSame -> fitsFail
    FitNest -> pure ace
    FitCast -> pure ace

  (Void', Void') -> pure ace
  (Void', _) -> case fit of
    FitSame -> fitsFail
    FitNest -> pure ace
    FitCast -> pure ace
  (_, Void') -> fitsFail

  (Spot' a, Spot' b)
    | a == b -> pure ace
    | otherwise      -> fitsFail
  (Spot'{}, _) -> fitsFail
  (_, Spot'{}) -> fitsFail

  (Fore' r, Fore' s)
    | r == s    -> pure ace
    | otherwise -> fitsFail
  (Fore'{}, _) -> fitsFail
  (_, Fore'{}) -> fitsFail

  --

  (Atom' a, Atom' b) | a == b -> pure ace
  (Atom'{}, _) -> fitsFail
  (_, Atom'{}) -> fitsFail

  (Cell' v w, Cell' v' w') -> fest fit lvl v v' ace >>= fest fit lvl w w'

  -- Evaluate the function bodies against a fresh opaque symbol. To get a fresh
  -- symbol, we have a bunch of options:
  --   - Track level as an argument to fits, as Kovacs does, incrementing under
  --     binders. We can then use (lvl + 1, 2) as the new Rump. Downside: not
  --     clear how to get this value when comparing two RTTIs at runtime.
  --     Although, in fact, rtts wil NEVER have Spot's, so...
  --   - Possibly, store a level in each saved, closed over, subject, taking the
  --     larger of the two. Think hard about whether this actually works.
  --   - Take two cons instead of two types, tracking lvl separately for each.
  --     Unclear when these will be different though?
  --   - Use Bound library style bullshit. Change the `a` type argument to allow
  --     an extra variable. This is a bit weird because the Codes are actually
  --     Hop Stub a, and bases have an extra Rump in them (so they are kinda
  --     Hop Rump a, except we couldn't get away with literally doing that), so
  --     we have to insert the variable just under the top level. But it works.
  --     On the other hand in a strict language paying the fmap cost upfront is
  --     not great? But we know for a fact that we'll be processing the entire
  --     body on success anyway, so? But also subtyping means if we're clever
  --     we can avoid fmapping entirely, if a can be encoded such that it's a
  --     subtype of Hop Stub a.
  --
  -- XX does the above work in the presence of Edit?
  --
  (Lamb' j, Lamb' k) ->
    fest fit (lvl + 1) (jamb j new) (jamb k new) ace
   where
    new = rump (lvl + 1, 2)

  -- XX should we read and do comb explosion?

  (Lamb' j, _) ->
    fest fit (lvl + 1) (jamb j new) (Slam' u new) ace
   where
    new = rump (lvl + 1, 2)

  (_, Lamb' k) ->
    fest fit (lvl + 1) (Slam' t new) (jamb k new) ace
   where
    new = rump (lvl + 1, 2)

  -- It seems we arguably should be evaluating all the arms here?
  (Crux' as p, Crux' bs q) -> do
    ace <- fest fit lvl p q ace
    farm as bs ace \_ a b ace ->
      fest fit lvl (eval p a) (eval q b) ace

  -- XX is there an analogy to the Lamb' Slam' above?
  (Crux'{}, _) -> fitsFail
  (_, Crux'{}) -> fitsFail

  (Pull' ar _ t, Pull' br _ u)
    | ar == br  -> fest fit lvl t u ace
    | otherwise -> fitsFail
  (Pull'{}, _) -> fitsFail
  (_, Pull'{}) -> fitsFail

  (Plus' v, Plus' w) -> fest FitSame lvl v w ace
  (Plus'{}, _) -> fitsFail
  (_, Plus'{}) -> fitsFail

  -- Since it hasn't been evaluated away, we are dealing with an opaque type
  -- function application. This means we have no choice but to regard the
  -- function as invariant in its argument.
  (Slam' v w, Slam' v' w') -> fest fit lvl v v' ace >>= fest FitSame lvl w w'
  (Slam'{}, _) -> fitsFail
  (_, Slam'{}) -> fitsFail

  (Equl' v w, Equl' v' w') -> asum
    [ fest FitSame lvl v v' ace >>= fest FitSame lvl w w'
    , fest FitSame lvl v w' ace >>= fest FitSame lvl w v'
    ]
  (Equl'{}, _) -> fitsFail
  (_, Equl'{}) -> fitsFail

  (Test' u v w, Test' u' v' w') -> ogle (fest FitSame lvl u u' ace) >>= \case
    Right ace -> do
     ace <- fest fit lvl v v' ace
     fest fit lvl w w' ace
    Left _ -> do
      ace <- fest fit lvl v v' ace
      ace <- fest fit lvl v w' ace
      ace <- fest fit lvl w v' ace
      fest fit lvl w w' ace
  (Test' _ v w, u) -> do
    ace <- fest fit lvl v u ace
    fest fit lvl w u ace
  (t, Test' _ v w) -> do
    ace <- fest fit lvl t v ace
    fest fit lvl t w ace

  (Fish' f v, Fish' f' v') -> do
    when (f /= f') fitsFail
    fest FitSame lvl v v' ace
  (Fish'{}, _) -> fitsFail
  (_, Fish'{}) -> fitsFail

  -- The assumption is that these are fully evaluated. This rules out Looks
  -- stacked on top of Looks, as well of Looks on top of cells. Accordingly the
  -- rules are pretty tight. I don't think there's any equiv of the beta-eta
  -- conversion we saw above with functions here.
  (Look' b st, Look' c ub)
    | st == ub  -> fest fit lvl b c ace
    | otherwise -> fitsFail
  (Look'{}, _) -> fitsFail
  (_, Look'{}) -> fitsFail

  (Aura' au as, Aura' ag bs) -> case fit of
    FitCast -> tule as bs
    FitNest -> if ag `isPrefixOf` au   then tule as bs else fitsFail
    FitSame -> if ag == au && as == bs then pure ace   else fitsFail
   where
    tule _         Bowl                               = pure ace
    tule (Fork as) (Fork bs) | as `Set.isSubsetOf` bs = pure ace
                             | otherwise              = fitsFail
    tule Bowl      _                                  = fitsFail

  -- Should empties otherwise be ruled out here?
  -- (Aura' au (Fork ss), Void') | ss == mempty -> pure ace

  (Sing' s t, Sing' z u) -> do
    ace <- fest FitSame lvl s z ace
    fest fit lvl t u ace

  (Sing' s t, _) -> fest fit lvl t u ace

  (Fuse' t f, Fuse' u g) -> do
    when (not $ prey g f) fitsFail
    fest fit lvl t u ace

  (Fuse' t f, u) -> fest fit lvl t u ace

  (Rail' v j, Rail' w k) -> do
    ace <- fest fit lvl v w ace
    -- XX should this be read new v, read new v, per Cardelli?
    fest fit (lvl + 1) (jamb j $ read new v) (jamb k $ read new w) ace
   where
    new = rump (lvl + 1, 2)

  (Rail' v j, Cell' w u) -> do
    ace <- fest fit lvl v w ace
    fest fit (lvl + 1) (jamb j $ read new v) u ace
   where
    new = rump (lvl + 1, 2)

  (Cell' v u, Rail' w k) -> do
    ace <- fest fit lvl v w ace
    fest fit (lvl + 1) u (jamb k $ read new v) ace
   where
    new = rump (lvl + 1, 2)

  -- Allow use of cell of types as type
  (Cell' v u, Type') -> do
    ace <- fest fit lvl v Type' ace
    fest fit lvl u Type' ace

  (Gate' v j, Gate' w k) -> do
    ace <- fest fit lvl w v ace
    -- XX should this be read new w, read new w, per Cardelli?
    fest fit (lvl + 1) (jamb j $ read new v) (jamb k $ read new w) ace
   where
    new = rump (lvl + 1, 2)

  (Core' u (clo, arms) v, Core' u' (clo', arms') v') -> do
    -- actual types are covariant
    ace <- fest fit lvl v v' ace
    -- formal types are contravariant
    ace <- fest fit lvl u' u ace
    let as = fmap (`Jamb` clo)  arms
    let bs = fmap (`Jamb` clo') arms'
    let x = read new t
    let y = read new u
    farm as bs ace \_ a b ace ->
      fest fit (lvl + 1) (jamb a x) (jamb b y) ace
   where
    new = rump (lvl + 1, 2)

  (Type', Type') -> pure ace

  (Aura'{}, _) -> fitsFail
  (_, Aura'{}) -> fitsFail
  (_, Sing'{}) -> fitsFail
  (_, Fuse'{}) -> fitsFail
  (Cell'{}, _) -> fitsFail
  (_, Cell'{}) -> fitsFail
  (Rail'{}, _) -> fitsFail
  (_, Rail'{}) -> fitsFail
  (Gate'{}, _) -> fitsFail
  (_, Gate'{}) -> fitsFail
  (Core'{}, _) -> fitsFail
  (_, Core'{}) -> fitsFail

 where
  fitsFail :: m b
  fitsFail = bail (FitsFail fit t u)


-- Find ------------------------------------------------------------------------

-- | Go to the given axis of the given type. You also need to provide a location
-- so we can run Rail Jambs against Rumps as needed.
peek :: (MonadCheck m, Var a) => Loc -> Type a -> Axis -> m (Type a)
peek loc typ a = do
  (_, lin) <- find loc typ [Axis a]
  pure $ long lin

-- | Find result.
data Line a = Line
  { lev :: Level
  , loc :: Axis
  , lyt :: Type a
  , las :: [Dash a]  -- ^ stack of steps taken
  }

-- Record one step of the path we took to reach the result of a successful find.
data Dash a
  -- ^ We have passed under a face annotation
  = DashFace Face
  -- ^ we have passed under a singleton value annotation
  | DashSing (Semi a)
  -- ^ We have passed into the left of a nondependent cell, and record the right
  | DashCellLeft (Type a)
  -- ^ We have passed into the left of a dependent cell, and record the right
  | DashRailLeft (Jamb a)
  -- ^ We have passed into the right of any cell, and record the left
  | DashCellRight (Type a)
  -- ^ We have passed into the payload of a core, and record the battery
  | DashCorePayload (Type a) (Semi a, Map Term (Code a))

deriving instance (Show a) => Show (Line a)
deriving instance (Show a) => Show (Dash a)

-- | Extract the effective type of the find result.
--
-- To understand how this works, consider the type `[a=@ *]?([%foo _])`. The
-- result of finding `a` should be `@?(%foo)`, not `@`; that is, the fork should
-- propagate inwards. We accomplish this propagation via the luf field of Line.
--
-- XX remove
long :: Line a -> Type a
long Line{lyt} = lyt

-- XX inefficient
-- Given a refined subject type, rerun it against its seminoun to advance it
-- further.
retcon :: Var a => Line a -> Con a
retcon Line{lev, lyt} = Con lev $ eval (read (rump (lev, 1)) lyt) $ loft lev lyt

-- | Kind of change to the subject to be rolled up.
data Seal
  = SealFine  -- ^ Refine a part of the subject to a subtype
  | SealEdit  -- ^ Edit a part of the subject to an arbitrary type

-- | When the found type (lyt) has been refined, zip the zipper back up to get
-- the fullsize refined subject type.
--
-- FIXME rename to `draw`, fix comment
seal :: Var a => Seal -> Line a -> Line a
seal mod lin@Line{lev, loc, lyt, las} = case las of
  [] -> lin
  DashFace f : las -> seal mod Line
    { lev
    , loc
    , lyt = Face' f lyt
    , las
    }
  DashSing s : las -> seal mod Line
    { lev
    , loc
    , lyt = Sing' s lyt
    , las
    }
  DashCellLeft tr : las -> seal mod Line
    { lev
    , loc = rise loc
    , lyt = Cell' lyt tr
    , las
    }
  DashRailLeft jr : las -> case mod of
    SealFine -> seal mod Line
      { lev
      , loc = rise loc
      , lyt = Rail' lyt jr  -- XX is this correct? prev did as SealEdit
      , las
      }
    SealEdit -> seal mod Line
      { lev
      , loc = rise loc
      , lyt = Cell' lyt (jamb jr $ read (rump (lev, rise loc / 2)) lyt)
      , las
      }
  DashCellRight tl : las -> seal mod Line
    { lev
    , loc = rise loc
    , lyt = Cell' tl lyt
    , las
    }
  DashCorePayload fom bat : las -> seal mod Line
    { lev
    , loc = rise loc
    , lyt = Core' fom bat lyt
    , las
    }

-- XX think more carefully about return value design
find :: forall a m. (MonadCheck m, Var a)
     => (Level, Axis) -> Type a -> Wing -> m (Stub, Line a)
find (lev, loc) typ win = act (ActFind (lev, loc) typ win) $
  fond Line
    { lev
    , loc
    , lyt = typ
    , las = []
    }
    win

fond :: forall a m. (MonadCheck m, Var a)
     => Line a -> Wing -> m (Stub, Line a)
fond lin = \case
  [] -> pure (Leg 1, lin)
  l:ls -> fond lin ls >>= \case
    -- arm1.arm2: as in 140, go back up to root of core.
    (rest, lin) -> do
      (st, con) <- limb lin l
      pure (pole a st, con)
     where
      a = case rest of
        Leg a -> a
        Arm a _ _ -> a
 where
  limb :: Line a -> Limb -> m (Stub, Line a)
  limb lin = \case
    Ares   -> pure (Leg 1, ares lin)
    Axis a -> (Leg a,) <$> axis a lin
    Ally f -> ally f lin

-- | Strip metadata (faces, forks, sings)
ares :: Var a => Line a -> Line a
ares lin@Line{lev, loc, lyt, las} = case lyt of
  Face' f t -> ares Line
    { lev
    , loc
    , lyt = t
    , las = DashFace f : las
    }

  Sing' s t -> ares Line
    { lev
    , loc
    , lyt = t
    , las = DashSing s : las
    }

  -- also need test case eventually, I think???

  _ -> lin

axis :: forall a m. (MonadCheck m, Var a)
     => Axis -> Line a -> m (Line a)
axis a lin@Line{lev, loc, lyt, las} = case (cut a, lyt) of
  (Nothing, _) -> pure lin

  (_, _) | Just t <- held lyt -> axis a Line
    { lev
    , loc
    , lyt = t
    , las  -- XX should we add a trace frame?
    }

  -- We want, in the pelt calculus, peek 2/peek 3 to give void on void
  (_, Void') -> pure Line
    { lev
    , loc = loc / a
    , lyt = Void'
    , las  -- there's really no good answer here
    }

  (_, Face' f t) -> axis a Line
    { lev
    , loc
    , lyt = t
    , las = DashFace f : las
    }

  (_, Sing' s t) -> axis a Line
    { lev
    , loc
    , lyt = t
    , las = DashSing s : las
    }

  -- This one is interesting. We propagate Tests inwards.
  -- This is necessary for compatibility with the "decision trees"
  -- produced by crop.
  -- XX allegedly broken. Requires dual traces in the Dash infra, it seems.
  (_, Test' x t u) -> do
    lin <- axis a lin { lyt = t }
    lon <- axis a lin { lyt = u }
    -- XX There are questions around leaving behind the las of u
    pure Line { lev, loc = loc / a, las, lyt = Test' x (long lin) (long lon) }

  (Just (L, a), Cell' tl tr) -> axis a Line
    { lev
    , loc = loc / 2
    , lyt = tl
    , las = DashCellLeft tr : las
    }

  (Just (L, a), Rail' tl jr) -> axis a Line
    { lev
    , loc = loc / 2
    , lyt = tl
    , las = DashRailLeft jr : las
    }

  (Just (R, a), Cell' tl tr) -> axis a Line
    { lev
    , loc = loc / 3
    , lyt = tr
    , las = DashCellRight tl : las
    }

  (Just (R, a), Rail' tl jr) -> axis a Line
    { lev
    , loc = loc / 3
    , lyt = jamb jr $ read (rump (lev, loc / 2)) tl
    , las = DashCellRight tl : las
    }

  (Just (L, a), Core' _ _ act) -> axis a Line
    { lev
    , loc = loc / 2
    , lyt = Noun'
    -- This way, if the user edits, she'll get a cell back rather than a core.
    , las = DashCellLeft act : las
    }

  (Just (R, a), Core' fom bat pay) -> axis a Line
    { lev
    , loc = loc / 3
    , lyt = pay
    , las = DashCorePayload fom bat : las
    }

  -- XX an old note reads: "arguably for Liskov, should be Noun :("; rethink
  -- Should this have been Void'? I think so.
  (_, _) -> bail (FindFail (Axis a) lyt)

ally :: forall a m. (MonadCheck m, Var a)
     => Term -> Line a -> m (Stub, Line a)
ally f lin@Line{lyt} =
  maybe (bail $ FindFail (Ally f) lyt) id $ lope lin
 where
  lope :: Line a -> Maybe (m (Stub, Line a))
  lope lin@Line{lev, loc, lyt, las} = case lyt of
    Face' (Mask m) t
      | f == m -> Just $ pure $ (Leg 1,) $ Line
          { lev
          , loc
          , lyt = t
          , las = DashFace (Mask m) : las
          }
      | otherwise -> Nothing

    Face' (Link ls) t
      | Just (a, fs) <- lookup f ls -> Just $ (Leg a,) <$> axis a lin
      | otherwise -> lope Line
          { lev
          , loc
          , lyt = t
          , las = DashFace (Link ls) : las
          }

    Sing' s t -> lope Line
      { lev
      , loc
      , lyt = t
      , las = DashSing s : las
      }

    Cell' tl tr -> asum
      [ fmap (first (pole 2)) <$> lope Line
          { lev
          , loc = loc / 2
          , lyt = tl
          , las = DashCellLeft tr : las
          }
      , fmap (first (pole 3)) <$> lope Line
          { lev
          , loc = loc / 3
          , lyt = tr
          , las = DashCellRight tl : las
          }
      ]

    Rail' tl jr -> asum
      [ fmap (first (pole 2)) <$> lope Line
          { lev
          , loc = loc / 2
          , lyt = tl
          , las = DashRailLeft jr : las
          }
      , fmap (first (pole 3)) <$> lope Line
          { lev
          , loc = loc / 3
          , lyt = jamb jr $ read (rump (lev, loc / 2)) tl
          , las = DashCellRight tl : las
          }
      ]

    Core' fom (clo, bat) act -> asum
      [ lookup f bat <&> \jam -> pure $ (Arm 1 f (keysSet bat),) $ Line
          { lev
          , loc  -- NOT loc / 2
          , lyt  -- type of the whole core
          , las
          }
      , fmap (first (pole 3)) <$> lope Line
          { lev
          , loc = loc / 3
          , lyt = act
          , las = DashCorePayload fom (clo, bat) : las
          }
      ]

    _ | Just t <- held lyt -> lope Line
        { lev
        , loc
        , lyt = t
        , las  -- XX should we add a trace frame?
        }

    _ -> Nothing


-- Pelt system -----------------------------------------------------------------

-- | Strip masks, but not links, from outside of type.
clip :: Type a -> Type a
clip = \case
  Face' Mask{} t -> t
  Face' l@Link{} t -> Face' l t
  t -> t

-- | Upgrade outer mask to link.
clop :: [Face] -> Map Term (Axis, [Face])
clop = \case
  [] -> mempty
  Mask m : fs -> mapFromList [(m, (1, fs))]
  Link ls : fs -> fmap (second (++ fs)) ls

-- | Peg an axis onto every axis in a link.
clap :: Axis -> Map Term (Axis, [Face]) -> Map Term (Axis, [Face])
clap a = fmap \(b, fs) -> (peg a b, fs)

-- | Extract the non-computational content from a pelt (i.e. the faces).
derm :: Pelt -> [Face]
derm = \case
  Punt -> []
  Peer m -> [Mask m]
  Part _ -> []
  Pair p q -> [Link $ clap 2 (clop (derm p)) ++ clap 3 (clop (derm q))]
  -- Pons p q -> derm p ++ derm q
  Pest p _ -> derm p
  Past p _ -> derm p

-- XX rename
mred :: Face -> Pelt
mred = \case
  Mask m -> Peer m
  Link _ -> Punt  -- XX FIXME

-- | Extract the computational content from a pelt (i.e. the testing part).
fish :: Pelt -> Fish
fish fis = case fis of
  Punt -> Tuna
  Peer _ -> Tuna
  Part a -> Sole a
  Pair p q -> char (fish p) (fish q)
  Pest p _ -> fish p
  Past p _ -> fish p
 where
  -- product of fishes
  char :: Fish -> Fish -> Fish
  char Tuna Tuna = Tuna
  char h    j    = Char h j

-- | Convert a fish back into a pelt. The name is meant to invoke releasing a
-- fish after catching it.
pond :: Fish -> Pelt
pond = \case
  Tuna -> Punt
  Sole a -> Part a
  Char h j -> Pair (pond h) (pond j)

{-
-- | Given a pattern, verify that it is compatibile with the given type.
-- Produce a new, broader type corresponding to any upcasts we may have made.
-- Note: This only works for values/types already stored in the subject.
-- This is because it is not possible otherwise to upcast the head of a rail.
toil :: (MonadCheck m, Var a)
     => Con a -> (Level, Axis) -> Pelt -> Type a -> m (Type a)
toil con loc pet typ = act (ActToil con loc pet typ)
 case pet of
  Punt -> pure typ
  Peer f -> pure (Face' (Mask f) typ)
  Part s -> bail (ToilFish pet typ)
  Pair p q -> do
    t <- peek loc typ 2
    t <- toil con (loc / 2) p t
    u <- peek loc typ 3
    u <- toil (hide con t) (loc / 3) q u
    pure (Cell' t u)
  -- Pons p q -> toil con fit p sem =<< toil con fit q sem typ
  Pest p c -> do
    t <- evil con . fst <$> work con FitNest c Type'
    -- Important: the type is reversed here. In this sense, pelts are
    -- contravariant.
    fits FitNest lvl typ t
    toil con loc p t
  Past p c -> do
    t <- evil con . fst <$> work con FitNest c Type'
    fits FitCast typ t
    toil con loc p t
-}


-- Exhaustiveness checking -----------------------------------------------------

-- | Spot test: Nothing means Char, Just n means Sole n. In decomposing fishes
-- for exaustiveness checking, we are often concerned with the "root claw" (or
-- just "claw") of a given Fish. This is the outermost test performed, if any.
type Claw = Maybe Atom

-- | Calculate the set of outermost claws explicitly tested for in the given
-- column. Crucially, Tunas do not test at all, so they don't give rise to
-- elements in the claw set.
clew :: [Fish] -> Set Claw
clew fs = setFromList $ fs >>= \case
  Tuna -> []
  Sole a -> [Just a]
  Char{} -> [Nothing]

-- | Bring a claw back to a fish.
molt :: Claw -> Fish
molt = \case
  Nothing -> Char Tuna Tuna
  Just a -> Sole a

-- | Enumerate all possible claws of this type, possibly an infinite list.
-- Infinite means the type contains @ or is stuck. There are two choices of what
-- to do with stuck types: ban pattern matching on them, or require the user to
-- always provide a default case. The latter seems more Hoonish (non-parametric)
-- so that is what we do for now, god save me.
soar :: Var a => Type a -> [Claw]
soar t = case t of
  _ | Just t <- held t -> soar t
  Aura' _ (Fork as) -> map Just $ toList as
  Aura' _ Bowl      -> map Just [0..]
  Cell' _ _         -> [Nothing]
  Rail' _ _         -> [Nothing]
  Sing' _ t         -> soar t   -- yes, for Joe
  Face' _ t         -> soar t
  Fuse' t Tuna      -> error "soar: invariant violation: Tuna Fuse"
  Fuse' t f         -> toList $ clew [f]
  Noun'             -> allClaws
  Void'             -> []
  -- XX It might be nice to warn the user when they are matching on stuck.
  _                 -> allClaws  -- If the type is stuck, user must supply `_`.
                                 -- Another reason not to redundancy-check.
 where allClaws = Nothing : map Just [0..]

{-
-- | Exhibit a pattern the user has not checked for. Here we are pattern
-- matching a vector of pattern columns against a vector of types.
tear :: Var a => [(Set Fish, Type)] -> Maybe [Fish]
tear = \case
  [] -> Nothing
  (fs, t) : fsts -> case soar t of
    Just cs | c:_ <- setToList (cs `Set.difference` clew fs) ->
      case c of
        Just a -> tear fsts <&> -}

-- | State for the exhaustiveness checker.
data Cube a = Cube
  { lvv :: Level
  , mat :: [[Fish]]
  , hed :: Edge a
  , axs :: [Axis]
  }

-- | Telescope of potentially dependent types tracking the columns of the
-- pattern matrix
data Edge a
  = Ends
  | Eons (Type a) (Semi a -> Edge a)

-- | Nondependently cons a type onto an edge
eras :: Type a -> Edge a -> Edge a
eras t ed = Eons t (const ed)

-- | Advance the telescope of types by reading the seminoun out of the first
-- and supplying it to the rest. An error occurs if empty, nya~.
ripe :: Var a => Level -> Edge a -> Edge a
ripe lvl = \case
  Ends -> error "ripe: empty edge"
  Eons t rest -> rest (read (rump (lvl, 1)) t)

ends :: Var a => Level -> Edge a -> [Type a]
ends lvl = \case
  Ends -> []
  ed@(Eons t _) -> t : ends lvl (ripe lvl ed)

instance (Var a) => Show (Cube a) where
  show Cube{..} = "Cube { lvv =" <> show lvv <> ", mat = " <> show mat
               <> ", hed = " <> had <> "}"
    where had = show (ends lvv hed)

-- | Keep only the rows with Tuna first column. This decrements the column
-- count and produces the so-called "default (sub)matrix." For example, the
-- default submatrix of
--
--     _  1  2  4
--     0  0  _  _
--     _  3  _  2
--     _  _  _  1
--
-- is
--
--     1  2  4
--     3  _  2
--     _  _  1
--
-- Note how the leading _s have been removed and row 2 is deleted because it
-- does not begin with _.
tore :: (MonadCheck m, Var a)
     => Cube a -> m (Cube a)
tore cub@Cube{lvv, mat, hed, axs} = act (ActTore cub) $ pure Cube
  { lvv
  , mat = [row | Tuna : row <- mat]
  , hed = ripe lvv hed
  , axs = tail axs
  }

-- | Keep only the rows where the first entry has the given root claw or is _.
-- For cellular claw, the two subfishes give rise to two new columns in
-- the front. Otherwise the column count decrements. The result is called the
-- "(sub)matrix specialized for" the given claw. For example, consider:
--
--     1          2  3
--     [8 [9 9]]  _  5
--     1          0  _
--     [_ 7]      6  6
--
-- Specializing to 1 gives:
--
--     2  3
--     0  _
--
-- while specializing to ^ (i.e. [_ _]) gives:
--
--     8  [9 9]  _  5
--     _  7      6  6
--
-- In this way, specialization strips off the outer "constructor" of the first
-- pattern and pushes the inner patterns, if any, onto the front of the row.
tear :: (MonadCheck m, Var a)
     => Claw -> Cube a -> m (Cube a)
tear cl cub@Cube{lvv, mat, hed, axs} = act (ActTear cl cub) case cl of
  Just a -> pure Cube
    { lvv
    , mat = [row | f : row <- mat, f == Sole a || f == Tuna]
    , hed = ripe lvv hed
    , axs = tail axs
    }
  Nothing -> pure Cube
    { lvv
    , mat = [f : g : row | (cellular -> Just (f, g)) : row <- mat]
    , hed = case hed of
        Ends -> error ("invariant violation: empty tear: " <> show cl)
        Eons ty rest -> case repo ty of
          Void'     -> Eons Void' \s -> Eons Void'      \z -> rest (Cell' s z)
          Cell' t u -> Eons t     \s -> Eons u          \z -> rest (Cell' s z)
          Rail' t j -> Eons t     \s -> Eons (jamb j s) \z -> rest (Cell' s z)
          _         -> Eons Noun' \s -> Eons Noun'      \z -> rest (Cell' s z)
    , axs = (head axs / 2) : (head axs / 3) : tail axs
    }
   where
    cellular = \case
      Char a b -> Just (a, b)
      Tuna -> Just (Tuna, Tuna)
      Sole{} -> Nothing

-- | Exhibit a pattern the user has not checked for. Here we are pattern
-- matching a row-major matrix of pattern rows against a row of types.
-- (So each [Fish] in the [[Fish]] must be of equal length to the [Type].) The
-- return value is Nothing when exaustive and a row of patterns when not.
-- This row is one pattern (of possibly many) which is missing from the match.
tyre :: (MonadCheck m, Var a)
     => Cube a -> m (Maybe [Fish])
tyre cub@Cube{lvv, mat, hed, axs} = act (ActTyre cub) case hed of
  -- The process of exaustiveness checking recursively strips columns off the
  -- front of the pattern matrix. (Or adds them in the case of cells.)
  -- Eventually we wind up with a zero width matrix that may or may not be zero
  -- height. Whether or not it was zero height determines the final outcome.
  -- Consider by analogy the haskell case () of {}. This is non-exhaustive.
  -- On the other hand case () of { () -> 1; _ -> 2 } is exhaustive. So the
  -- number of rows in the matrix determines whether a nullary vector match is
  -- exhaustive.
  Ends -> pure case mat of { [] -> Just []; _:_ -> Nothing }
  -- In the inductive case, we check whether the user explicitly matches against
  -- every possible claw (outermost pattern layer) of the first column type.
  -- Tuna matches don't count for this check.
  Eons t _ -> let explicits = clew (map head mat)
              in  case filter (`notElem` explicits) $ soar t of
    -- If there is a missing claw, exhaustiveness depends only on what happens
    -- in the rows that start with Tuna. They have to form an exhaustive matrix,
    -- because we already know the rows that start with explicit match do not.
    cl:cls -> tore cub >>= tyre >>= \case
      -- The default matrix is exhaustive, so our matrix is exhaustive
      Nothing -> pure $ Nothing
      -- The default matrix has an unmatched example, so we derive ours from it
      -- by prepending a missing example pattern. This is either _ or something
      -- more specific based on a heuristic.
      Just ex -> pure $ Just (missing : ex)
        where missing = if explicits == mempty then Tuna else molt cl
    -- On the other hand, if the explicit matches in first position are
    -- exhaustive, we segregate the matrix by claw of first match and require
    -- each resultant specialized submatrix to be exhaustive. We glue together
    -- a counterexample from the first submatrix that has one, if any.
    [] -> asum <$> for (soar t) \c -> glue c <$> (tyre =<< tear c =<< cub' c)
     where
      -- Edit the type of the first column to take into account the gained info.
      -- Cruicially, this extra info will be read back out later into a seminoun
      -- in tore or atomic tear.
      cub' cl = do
        hed <- case hed of
          Eons t rest -> Eons (fuse t (molt cl)) <$> pure rest
        pure cub{hed}
      -- The first claw-specialized submatrix that gives a missing example needs
      -- translated back into our language based on the claw in question. For
      -- example if 1 comes first and the 1 patterns in
      --
      --     1  &  &
      --     1  &  _
      --     1  |  &
      --     2  &  &
      --     ...
      --
      -- are nonexaustive, then the missing example `|  |` from the submatrix
      -- specialized to 1 needs to be pulled back by prepending a `1` to it to
      -- get a missing exaple `1  |  |` for the whole.
      glue = \case
        Just a  -> fmap (Sole a :)
        Nothing -> fmap (\(a:b:cs) -> Char a b : cs)

-- | Check that the given set of fish is exhaustive at the given type. If the
-- type is stuck, it behaves like Noun for the sake of exhaustiveness checking.
--
-- This is the entry point of the exhaustiveness checker.
tire :: (MonadCheck m, Var a)
     => Loc -> Set Fish -> Type a -> m ()
tire (lvv, axe) fis typ = act (ActTire lvv fis typ) $
  tyre Cube
    { lvv
    , mat = toList fis <&> \f -> [f]
    , hed = Eons typ \_ -> Ends
    , axs = [axe]
    }
  >>= \case
    Nothing  -> pure ()
    Just [f] -> bail (TireFish (peg (2 ^ lvv) axe) f)
    Just fs -> bail (BailNote ("fish count invariant violation: " <> tshow fs))


-- Type checking ---------------------------------------------------------------

-- | Type check the condition of a Test, producing its Code as well as the
-- possibly refined subjects for the branches to be checked against.
chip :: (MonadCheck m, Var a)
     => Con a -> Soft -> m (Code Void, Con a, Con a, Set Fish)
chip con@Con{lvl, sut} sof = case sof of
  Fis p (Wng w []) -> do
    -- strip faces, etc
    (sud, lin@Line{lyt}) <- find (lvl, 1) sut w
    case sud of
      Leg axe -> do
        let fis = fish p
        let tru = retcon
                $ seal SealFine lin { lyt = face' [Link $ clop $ derm p]
                                          $ fuse lyt fis }
        -- fal <- seal in { lyt = crop lyt fis }
        fal <- pure con
        pure (Fish fis $ Spot axe, tru, fal, singleton $ gill axe fis)

      Arm{} -> fall

  _ -> fall

 where
  fall = do
    (x, ns) <- work con FitNest sof Flag'
    pure (x, con, con, ns)

-- | Perform type checking for centis.
fend :: forall a m. (MonadCheck m, Var a)
     => Con a -> Wing -> [(Wing, Soft)] -> m (Code Void, Type a, Set Fish)
fend con@Con{lvl, sut} w whs = do
  (st, lin) <- find (lvl, 1) sut w
  (c, lin, ms) <- foldlM
    loop
    (Spot (loc lin), lin {las = []}, singleton Tuna)
    whs
  case st of
    Leg _ -> pure (c, lyt lin, ms)
    Arm _ arm arms -> case lyt lin of
      Core' fom (clo, lookup arm -> Just cod) act -> do
        -- late binding check: actual must nest under formal
        fits FitNest lvl act fom
        -- compute return type given actual argument value seminoun
        let ret = jamb Jamb{cod, clo} $ read (rump (lvl, loc lin / 3)) act
        pure (Pull arm arms c, ret, ms)
      t -> bail (BailNote $
        "fend: invariant violation: expected core with arm " <> arm <>
        " but got " <> tshow t)
 where
  loop :: (Code Void, Line a, Set Fish)
       -> (Wing, Soft)
       -> m (Code Void, Line a, Set Fish)
  loop (c, lin, ms) (win, sof) = do
    (st, lin) <- fond lin win
    (rhs, typ, ns) <- play con sof
    case st of
      Arm{} -> bail (EditPull win (lyt lin))
      Leg ax -> pure
        ( Edit c ax rhs
        , seal SealEdit lin { lyt = typ }
        , swam ms ns
        )

-- | Given subject type and knowledge, verify that code has result type.
-- Since the expected result type is known in this mode, we can lighten the
-- user's annotation burden. Read about "bidirectional type checking" to
-- learn more.
work :: forall a m. (MonadCheck m, Var a)
     => Con a -> Fit -> Soft -> Type a -> m (Code Void, Set Fish)
work con@Con{lvl, sut} fit cod gol = act (ActWork con fit cod gol)
  let playFits = do (x, t', ns) <- play con cod
                    fits fit lvl t' gol
                    pure (x, ns)
  in case cod of
    Wng{} -> playFits

    -- for introduction forms except atoms, we push the type constraint inward
    -- this allows the user to type-annotate the whole of a big data structure
    -- indcluding cores and gates, without having to also annotate the insides
    -- unless they want to.
    Atm{} -> playFits

    Cel c d -> case gol of
      -- XX FIXME deal with other metadata like Fork, Sing
      (held -> Just t) -> work con fit cod t
      Face' f t -> work con fit cod t
      Type' -> playFits
      Cell' t u -> do
        (x, ns) <- work con fit c t
        (y, ms) <- work con fit d u
        pure (Cell x y, swam ms ns)
      Rail' t j -> do
        (x, ms) <- work con fit c t
        let u = jamb j $ evil con x
        (y, ns) <- work con fit d u
        pure (Cell x y, swam ms ns)
      _ -> playFits

    Lam a c -> case gol of
      -- XX FIXME deal with other metadata like Fuse' (?)
      (held -> Just t) -> work con fit cod t
      Face' f gol -> work con fit cod gol
      Sing' s gol -> do
        (x, ms) <- work con fit cod gol
        fits FitSame lvl s (evil con x)
        pure (x, ms)
      Gate' paramT resJ -> do
        -- FIXME switch this to type not skin
        (argC, ms) <- work con FitNest a Type'
        let argT = evil con argC
        fits FitNest lvl paramT argT
        let paramK = read (rump (lvl + 1, 2)) paramT
        (x, ns) <- work (hide con argT) fit c (jamb resJ paramK)
        -- You could actually be doing case analysis to determine arg type,
        -- and that needs to be total.
        tire (lvl, 1) ms sut
        -- XX can this be relaxed? The problem is this: Suppose subject type is
        -- ?(%r %g %b). Then this is okay:
        -- ?-  .
        --   %r  |=  _  1
        --   _   |=  _  ?-  +
        --                %g  2
        --                %b  3
        -- ==           ==
        --
        -- but matching under a |= in the then branch of a better constructed
        -- example seems not to be.
        tire (lvl + 1, 1) ns (Cell' argT sut)
        pure (Lamb x, singleton Tuna)
      _ -> playFits

    Cru arms -> case gol of
      -- XX FIXME deal with other metadata like Fuse' (?)
      (held -> Just t) -> work con fit cod t
      Face' f gol -> work con fit cod gol
      Sing' s gol -> do
        (x, ms) <- work con fit cod gol
        fits FitSame lvl s (evil con x)
        pure (x, ms)
      Core' fom (clo, bat) act -> do
        fits fit lvl sut act
        let ken = read sut (rump (lvl, 1))
        let sut = gol
        xs <- farm arms bat mempty \nom arm armT xs -> do
          (x, ms) <- work
            Con{sut, lvl = lvl + 1}
            fit
            arm
            (jamb (Jamb armT clo) ken)
          tire (lvl + 1, 1) ms sut
          pure (insertMap nom x xs)
        pure (Crux xs, singleton Tuna)
      _ -> playFits

    Fac p c -> do
      -- XX think about whether we should instead play here, so that toil can
      -- operate against a more specific scrutinee type.
      (x, ns) <- work con fit c gol
      let fs = derm p
      asum
        [ fits FitNest lvl gol Type' >> pure (face fs x, ns)
        , pure (x, ns)
        ]

    -- elimination forms just use nest
    Plu{} -> playFits
    Sla{} -> playFits
    Equ{} -> playFits
    Fis{} -> playFits  -- not inside Tes

    -- This is the big whammy of work right now. The play implementation calls
    -- join (compute type union), which is not yet implemented. So you need to
    -- annotate the return types of your switches, but because of the way that
    -- work propagagtes goal types inwards, this annotation can be far away and
    -- is likely already present.
    Tes c d e -> do
      (x, tru, fal, fis) <- chip con c
      (y, met) <- work tru fit d gol
      (z, jon) <- work fal fit e gol
      pure (Test x y z, union (swam fis met) jon)

    -- "rhetorical" tests are required to evaluate to true at compile time.
    -- this will be a rigorous exercise of the crop/fuse system and will be
    -- our mechanism of exhaustiveness checking.
    -- XX update this comment in light of the new exhaustiveness checker
    Rhe c d -> do
      (x, tru, _, fis) <- chip con c
      (y, met) <- work tru fit d gol
      pure (y, swam fis met)

    --Run{} -> playFits

    -- likewise with types
    Aur{} -> playFits
    Ral{} -> playFits
    Gat{} -> playFits
    Cor{} -> playFits
    Sin{} -> playFits
    Fus{} -> playFits

    Non -> playFits
    Vod -> playFits
    Typ -> playFits

    Wit c d -> do
      (x, t, pre) <- play con c
      (y, pos) <- work Con { lvl = 0, sut = grow t } fit d (grow gol)
      -- XX investigate when this can be relaxed
      tire (lvl, 1) pos t
      pure (With x y, pre)

    Pus c d -> do
      -- The fishing here is remarkable. The newly introduced part of the
      -- subject is the only exhastiveness requirement. The rest passes through.
      (x, t, ns) <- play con c
      (y, ms) <- work (shew con (evil con x) t) fit d gol
      tire (lvl + 1, 2) (slip L ms) t
      pure (Push x y, swam ns (slip R ms))

    Net{} -> playFits
    Cat{} -> playFits

    --Sin c -> work con fit c gol

-- | Require the given type to be a function type.
-- XX Deppy had a cas rule here; why?
{-needGate :: (MonadCheck m, Var a)
         => Con a -> Type a -> m (Type a, Semi a, Code a)
needGate con = \case
  Gate' t s c -> pure (t, s, c)
  Face' _ t -> needGate con t
  t -> bail $ NeedGate t-}

-- | Given subject type, determine product type of code. In the process, also
-- generate allyless untyped core and exaustiveness checking data.
play :: forall a m. (MonadCheck m, Var a)
     => Con a -> Soft -> m (Code Void, Type a, Set Fish)
play con@Con{lvl, sut} cod = act (ActPlay con cod) case cod of
  Wng w eds -> fend con w eds

  Atm a Rock au ->
    pure (Atom a, Aura' au (Fork $ singleton a), singleton Tuna)

  Atm a Sand au -> pure (Atom a, Aura' au Bowl, singleton Tuna)

  Cel c d -> do
    (x, t, ms) <- play con c
    (y, u, ns) <- play con d
    pure (Cell x y, Cell' t u, swam ms ns)

  Lam a c -> do
    (argC, ms) <- work con FitNest a Type'
    let argT = evil con argC
    -- TODO replace with gold core
    (x, resT, ns) <- play (hide con argT) c
    let ken = read (rump (lvl, 1)) sut
    let resC = loft (lvl + 1) resT
    -- You could actually be doing case analysis to determine arg type,
    -- and that needs to be total.
    tire (lvl, 1) ms sut
    -- We must be total in our case analysis of both the arg *and* the closure.
    tire (lvl + 1, 1) ns (Cell' argT sut)
    pure (Lamb x, Gate' argT (Jamb resC ken), singleton Tuna)

  Cru arms -> do
    -- let enjamb c = Jamb (fmap absurd c) (read (rump (lvl, 1)) sut)
    -- TODO relax this requirement
    tys :: Map Term (Code a) <- for arms \case
      Net{typ} -> fmap absurd . fst <$> work (hide con Void') FitNest typ Type'
      Cat{typ} -> fmap absurd . fst <$> work (hide con Void') FitNest typ Type'
      _ -> bail (BailNote "must annotate every arm with type for now")
    let cor = Core' sut (read (rump (lvl, 1)) sut, tys) sut
    ress <- traverse (play con { sut = cor, lvl = lvl + 1}) arms
    for_ ress (\(_, _, ns) -> tire (lvl + 1, 1) ns cor)
    let crux = Crux (ress <&> \(x, _, _) -> x)
    let crux' = evil con crux
    pure
      ( crux
      -- XX ideally, the singleton would sit only on the battery
      , Sing' crux' cor
      -- (Core' sut <$> for ress (\(_, t, _) -> loft t)
      , singleton Tuna
      )

  Fac p c -> do
    (x, t, ms) <- play con c
    -- XX note that it is not possible to run toil here, so any types you put
    -- in your pelt will silently have no effect. FIXME
    let fs = derm p
    -- XX think about under what circumstances we can strip the first face.
    -- It's annoying to have these lying around in the seminoun.
    -- XX confirm this is right
    asum
      [ fits FitNest lvl t Type' >> pure (face fs x, face' fs t, ms)
      , pure (x, face' fs t, ms)
      ]

  Plu c -> do
    -- Following 140, we do not propagate aura.
    (x, ms) <- work con FitNest c (Aura' "" Bowl)
    pure (Plus x, Aura' "" Bowl, ms)

  Sla c d -> do
    (x, ct, ms) <- play con c
    let go = \case
          (held -> Just t) -> go t
          Face' _ t -> go t
          Sing' _ t -> go t
          Gate' at j -> pure (at, j)
          t -> bail (NeedGate t)
    (at, j) <- go ct
    (y, ns) <- work con FitNest d at
    pure (Slam x y, jamb j $ evil con y, swam ms ns)

  Equ c d -> do
    (x, _, ms) <- play con c
    (y, _, ns) <- play con d
    pure (Equl x y, Flag', swam ms ns)

  Tes c d e -> do
    (x, tru, fal, fis) <- chip con c
    (y, t, met) <- play tru d
    (z, u, jon) <- play fal e
    r <- join t u
    -- FIXME optimization: when the joint fishes cover the whole type, replace
    -- with Tuna.
    pure (Test x y z, r, union (swam fis met) jon)

  -- "rhetorical" tests are required to evaluate to true at compile time.
  -- this will be a rigorous exercise of the crop/fuse system and will be
  -- our mechanism of exhaustiveness checking.
  --
  -- update: now it works differently. enjoy!
  --
  -- XX it may be more elegant, rather than providing ??, to provide a version
  -- of !! (tentatively, ?!) which corresponds to the empty set of fishes. This
  -- will be weirder for code generation though.
  Rhe c d -> do
    (x, tru, _, fis) <- chip con c
    (y, t, met) <- play tru d
    pure (y, t, swam fis met)

  -- For Fis not inside Tes
  Fis p c -> do
    (x, _, ms) <- play con c
    pure (Fish (fish p) x, Flag', ms)

  {-Run sv st fom pt -> do
    st' <- work con FitNest st Type'
    sv' <- work con FitNest sv (evil ken st')
    (fom', _) <- play con fom  -- HACK XX fix when we have recursive types
    pt' <- work con FitNest pt Type'
    let ret = Test (Equl (Atom 0) (Stub $ Leg 3)) sv' (Aura "t")
    pure (Work st' sv' fom' pt', Cell' Flag' ken $ vacuous ret)-}

  Aur au to -> pure (Aura au to, Type', singleton Tuna)

  Ral c d -> do
    (x, ms) <- work con FitNest c Type'
    (y, ns) <- work (hide con (evil con x)) FitNest d Type'
    tire (lvl + 1, 2) (slip L ns) (evil con x)
    pure (Rail x y, Type', swam ms (slip R ns))

  Gat c d -> do
    (x, ms) <- work con FitNest c Type'
    (y, ns) <- work (hide con (evil con x)) FitNest d Type'
    -- XX does the whole of ns need to be exhaustive, actually? also Ral, Cor
    tire (lvl + 1, 2) (slip L ns) (evil con x)
    pure (Gate x y, Type', swam ms (slip R ns))

  Cor act fom bat -> do
    (x, ms) <- work con FitNest act Type'
    (y, ns) <- work con FitNest fom Type'
    ress <- for bat \arm -> work con FitNest arm Type'
    let xs  = fmap fst ress
    let mss = fmap snd ress
    pure (Core y xs x, Type', foldl' swam (swam ms ns) mss)

  Sin sof typ -> do
    (y, ns) <- work con FitNest typ Type'
    (x, ms) <- work con FitNest sof $ evil con y
    pure (Sing x y, Type', swam ns ms)

  Fus typ pet -> do
    (x, ns) <- work con FitNest typ Type'
    let fis = fish pet
    let fac = derm pet
    -- XX do we want something like constant folding in the fst below?
    pure (Face (Link $ clop fac) $ Fuse x fis, Type', ns)

  Non -> pure (Noun, Type', singleton Tuna)

  Vod -> pure (Void, Type', singleton Tuna)

  Typ -> pure (Type, Type', singleton Tuna)

  Wit c d -> do
    (x, t, pre) <- play con c
    -- XX should we make a singleton type here, analogous to storing the
    -- seminoun in 3?
    (y, u, pos) <- play Con{lvl=0, sut=(grow t)} d
    ret <- pare u
    -- XX investigate when this can be relaxed. seems like regression for eg =.
    tire (lvl, 1) pos t
    pure (With x y, ret, pre)

  Pus c d -> do
    (x, t, ms) <- play con c
    (y, u, ns) <- play (shew con (evil con x) t) d
    tire (lvl + 1, 2) (slip L ns) t
    pure (Push x y, u, swam ms (slip R ns))

  Net{sof, typ} -> do
    (x, ms) <- work con FitNest typ Type'
    let t = evil con x
    (y, ns) <- work con FitNest sof t
    pure (y, t, swam ms ns)

  Cat{sof, typ} -> do
    (x, ms) <- work con FitNest typ Type'
    let t = evil con x
    (y, ns) <- work con FitCast sof t
    pure (y, t, swam ms ns)

-- | Read code back to soft, making no attempt to untranslate axes to wings with
-- names.
rest :: forall a m. Var a => Code a -> Soft
rest = \case
  Spot a -> Wng [Axis a] []
  Fore x -> Wng [Ally $ tshow @(Hop () a) $ Old x] []  -- hack for printing
  --
  Atom a -> Atm a Sand (heuAura a)
  Cell c d -> Cel (rest c) (rest d)
  -- XX this loss of facial information may be unfortunate for diagnostic
  -- purposes. Think about this. Fixed by doze?
  Lamb c -> Lam Non (rest c)
  Crux cs -> Cru (fmap rest cs)
  --
  Pull ar _ (Spot a) -> Wng [Ally ar, Axis a] []
  Pull ar _ c -> Wit (rest c) $ Wng [Ally ar] []
  Plus c -> Plu (rest c)
  Slam c d -> Sla (rest c) (rest d)
  Equl c d -> Equ (rest c) (rest d)
  Test c d e -> Tes (rest c) (rest d) (rest e)
  Fish h c -> Fis (pond h) (rest c)
  Edit c a d -> case rest c of
    Wng w eds -> Wng w (eds ++ [([Axis a], rest d)])
    x -> Wit x (Wng [] [([Axis a], rest d)])
  Aura au to -> Aur au to
  Rail c d -> Ral (rest c) (rest d)
  Gate c d -> Gat (rest c) (rest d)
  Core c d e -> Cor (rest e) (rest c) (fmap rest d)
  Sing s t -> Sin (rest s) (rest t)
  Face (Mask m) c -> Fac (Peer m) (rest c)
  Face (Link ls) c -> Fac Punt (rest c)  -- FIXME ?
  Fuse c h -> Fus (rest c) (pond h)
  Noun -> Non
  Void -> Vod
  Type -> Typ
  With c d -> Wit (rest c) (rest d)
  Push c d -> Pus (rest c) (rest d)

-- | Use a subject type to read back wing information in a much less shitty way.
-- doze :: Var a => Type a -> Code Stub -> Soft
-- doze typ = undefined
