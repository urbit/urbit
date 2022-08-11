{-# LANGUAGE Strict #-}

module Practice.DependentHoon4 where

import ClassyPrelude hiding ((/), even, find, head, join, read, tail, catch)
import Prelude (head, tail)

import Control.Arrow (left)
-- import Control.Concurrent.Async (AsyncCancelled)
import Control.Exception (ArithException(..), catch, throw)
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
--   | Lam Soft Soft                  --  |=  t  h                   function value
  | Cru (Map Term Soft)            --  |%  ++  foo  h  --             core value
  --
  | Plu Soft                       --  +(h)                     atomic increment
  | Equ Soft Soft                  --  =(h j)                      equality test
  | Tes Soft Soft Soft             --  ?:  h  j  k                boolean branch
  | Rhe Soft Soft                  --  ??  h  j                rhetorical branch
  | Fis Pelt Soft                  --  ?=(s h)                      pattern test
  --
  | Aur Aura Tool                  --  @ud,  ?(%foo, %bar)           atomic type
  | Ral Soft Soft                  --  {t u}                          sigma type
  | Gat Soft Soft                  --  $-(t u)                      pi/gate type
  | Cor Soft Soft (Map Term Soft)  --  $|  t  u  ++  foo v  --         core type
  | Sin Soft Soft                  --  1|@,  $=  1  @             singleton type
  | Mot Soft Soft                  --  $;(t u)                formal/actual type
  | Fus Soft Pelt                  --  $>  t  p, t?(p)                 fuse type
  | Sal Wing Soft                  --  $?  w  t                    abstract type
  | Non                            --  *                           top/noun type
  | Vod                            --  !                        bottom/void type
  | Typ                            --  $                            type of type
  --
  | Wit Soft Soft
  | Pus Soft Soft
  | Mut Soft Soft Soft
  | Net { sof :: Soft, typ :: Soft }
  | Cat { sof :: Soft, typ :: Soft }
  | Hid Wing Soft                  -- ^?  w  h             cast to abstract type
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
  | Crux Mesh (Map Term (Code a))
  --
  | Pull Term (Set Term) (Code a)
  | Plus (Code a)
  | Equl (Code a) (Code a)
  | Test (Code a) (Code a) (Code a)
  | Fish Fish (Code a)
  | Edit (Code a) Axis (Code a)
  --
  | Aura Aura Tool
  | Rail (Code a) (Code a)
  | Gate (Code a) (Code a)
  | Core (Code a) (Map Term (Code a)) (Code a)
  | Sing (Code a) (Code a)
  | Molt (Code a) (Code a)
  | Seal Axis (Code a)
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

-- | Mark those parts of the payload that should be stubbed out with unknowns
-- when the core value (Crux') is fits-checked. For example [%milk %meat] means
-- "stub out the +6 but not the +7" of this core. By this mechanism, eta-beta
-- equivalence (actually nest) checking is effected for cores.
data Mesh
  = Mesh Mesh Mesh  -- ^ cons two meshes together
  | Meat            -- ^ do not stub out this part of the payload
  | Milk            -- ^ please stub out this part of the payload
  deriving (Eq, Ord, Show, Generic)

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
  -- except in fits where the Amber rule is applied. The text is the display
  -- name for printing, because we don't want to print the whole code. Since
  -- holds arise from arm pulls, we just print the arm name, so the user sees
  -- e.g. `(list @)` where list stands for a hold.
  | Hold' Text (Semi a) (Code a)
  --
  | Atom' Atom
  | Cell' (Semi a) (Semi a)
  | Crux' Mesh (Map Term (Code a)) (Semi a)
-- | Crux' or Bulk'
  --
  | Pull' Text (Set Text) (Semi a)
  | Plus' (Semi a)
  | Equl' (Semi a) (Semi a)
  | Test' (Semi a) (Semi a) (Semi a)
  | Fish' Fish (Semi a)
  | Look' (Semi a) Axis
  | Edit' (Semi a) Axis (Semi a)
  --
  | Aura' Aura Tool
  | Rail' (Semi a) (Jamb a)
  | Gate' (Semi a) (Jamb a)
  | Core'
    -- | formal payload type (to be thought of as part of the battery type)
    (Semi a)
    -- | map of Jambs to calculate arm types, with shared closure.
    -- The rule is that you must cons a semi for the *whole* context value onto
    -- the closure. This may often in practice mean that the same value is
    -- doubled.
    (Semi a, Map Term (Code a))
    -- | actual payload type
    (Semi a)
  | Sing' {- | val -} (Semi a) {- | type -} (Semi a)
  | Molt' {- | formal type -} (Semi a) {- | actual type -} (Semi a)
  | Seal' Axis (Semi a)
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
--  (_,           Hold' t s c)         -> Hold' t s $ With c (Spot a)
    (_,           Spot' r)             -> Spot' $ r / a
    (_,           Fore' x)             -> Fore' $ x / a
    (_,           Look' c st)          -> Look' c $ st / a
--  (_,           Face' _ c)           -> walk a c  -- Faces only on types now
    (Just (L, a), Cell' s _)           -> s / a
    (Just (R, a), Cell' _ s)           -> s / a
    (Just (R, a), Crux' _ _ p)         -> p / a
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
--Lamb c  -> NC (mint c)  $ NC (N1 $ A 0) (N0 1)
  Crux _ cs -> NC (mine mint cs) $ NC (N1 $ A 0) (N0 1)
  --
  Pull a as c -> N9 (loot a as) (mint c)
  Plus c -> N4 (mint c)
  -- Slam c d -> N9 2 $ N10 6 (mint d) $ mint c  -- XX check
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
  Core fom ars act -> NC (N1 CORE)
                    $ NC (N1 $ mint act)
                    $ NC (N1 $ mint fom)
                    $ mine (N1 . mint) ars
  Sing c d -> NC (N1 SING) $ NC (mint c) (mint d)
  Molt c d -> NC (N1 MOLT) $ NC (mint c) (mint d)
  Face (Mask m) c -> NC (N1 FACE) $ NC (A $ utf8Atom m) (mint c)
  -- FIXME, also strip on empty link to avoid colliding with empty mask.
  Face (Link l) c -> NC (N1 FACE) $ NC (C (A 999) (A 888)) (mint c)
  Fuse c f -> NC (N1 FUSE) $ NC (mint c) (land f)
  Seal a c -> NC (N1 SEAL) (mint c)
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
pattern MOLT = A 1_953_263_469
pattern FACE = A 1_701_011_814
pattern FUSE = A 1_702_065_510
pattern SEAL = A 1_818_322_291
pattern NOUN = A 1_853_189_998
pattern VOID = A 1_684_631_414
pattern TYPE = A 1_701_869_940

-- | Find the axis of an arm in a core.
loot :: Term -> Set Term -> Axis
loot _ _ = 1338  -- FIXME

-- | Build the formula tree for the battery of a core.
-- FIXME
mine :: (Code Void -> Noun) -> Map Term (Code Void) -> Noun
mine fun bat = foldr step (A 0) $ mapToList bat
 where
  step (_, cod) bod = C (fun cod) bod

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
  -- XX holds?
  Sing' s _ -> s
  Molt' _ t -> read nul t
  Face' _ t -> read nul t
  Seal' a t -> read nul t  -- FIXME WRONG
  Fuse' t h -> skim (read nul t) h
  Aura' au (Fork ss) | [a] <- setToList ss -> Atom' a
  Test' b t f -> Test' b (read nul t) (read nul f)   -- XX correct?
  Cell' t u -> Cell' (read (nul / 2) t) (read (nul / 3) u)
  Rail' t j -> Cell' lef rit
   where
    lef = read (nul / 2) t
    rit = read (nul / 3) $ jamb j lef
  Core' _ (_, _) act -> Cell' (nul / 2) (read (nul / 3) act)
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
--
edit :: Var a => Semi a -> Axis -> Semi a -> Semi a
edit bas a mod = case (cut a, bas) of
  (Nothing, s) -> mod
  (Just (L, a), Cell' l r) -> Cell' (edit l a mod) r
  (Just (R, a), Cell' l r) -> Cell' l (edit r a mod)
  (Just (R, a), Crux' m bat pay) -> Crux' m bat (edit pay a mod)
  _ -> Edit' bas a mod
  {-
  -- XX does this work for a %= arm pull?
  -- Answer, lol no, crashes and burns horribly on holds. You get
  -- %:[+2:list @ +7:list] for (list @) which is completely unworkable.
  (Just (L, a), _) -> Cell' (edit (bas / 2) a mod) (bas / 3)
  (Just (R, a), _) -> Cell' (bas / 2) (edit (bas / 3) a mod)
  -}

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

-- | Evaluation mode.
data EvalMode
  = EvalHold  -- ^ Evaluate, pausing at each arm pull
  | EvalFull  -- ^ Evaluate all the way. Do NOT use in type checker.

-- | Evaluate the code to a seminoun.
semi :: Var a => EvalMode -> Semi a -> Code a -> Semi a
semi mod sub = \case
  Spot s -> sub / s
  Fore x -> Fore' x
  --
  Atom a -> Atom' a
  -- XX cannot use cell' here because consider e.g. [! 1]. This is not !.
  -- A possibly serious drawback of cell/cons unification.
  Cell c d -> Cell' (semi mod sub c) (semi mod sub d)
  Crux m as -> Crux' m as sub
  --
  Pull a ar c -> pull mod a ar (semi mod sub c)

  Plus c -> plus (semi mod sub c)
  Equl c d -> equl (semi mod sub c) (semi mod sub d)
  Test c d e -> test (semi mod sub c) (semi mod sub d) (semi mod sub e)  -- Laziness!
  Fish f c -> fisk f (semi mod sub c)
  Edit c a d -> edit (semi mod sub c) a (semi mod sub d)
  --
  Aura au to -> Aura' au to
  Rail c d -> rail' (semi mod sub c) (Jamb d sub)
  Gate c d -> gate' (semi mod sub c) (Jamb d sub)
  Core c ds e -> core' (semi mod sub c) (sub, ds) (semi mod sub e)
  Sing c d -> sing' (semi mod sub c) (semi mod sub d)
  Molt c d -> Molt' (semi mod sub c) (semi mod sub d)
  Face f c -> face' [f] (semi mod sub c)
  Fuse c f -> fuse (semi mod sub c) f
  Seal a c -> Seal' a (semi mod sub c)  -- no void->void
  Noun -> Noun'
  Void -> Void'
  Type -> Type'
  With c d -> semi mod (semi mod sub c) d
  Push c d -> semi mod (Cell' (semi mod sub c) sub) d

-- | The partial evaluator for use in the typechecker.
eval :: Var a => Semi a -> Code a -> Semi a
eval = semi EvalHold

-- | Evaluate all the way. You probably don't mean to call this from within the
-- compiler
spin :: Var a => Semi a -> Code a -> Semi a
spin = semi EvalFull

-- | Implement the Pull eliminator.
pull :: Var a => EvalMode -> Text -> Set Text -> Semi a -> Semi a
pull mod ar ax b = case b of
  Crux' _ cs _ | Just c <- lookup ar cs -> case mod of
    EvalHold -> Hold' ar b c
    EvalFull -> semi EvalFull b c
  x -> Pull' ar ax x

-- | Implement the Plus eliminator.
plus :: Semi a -> Semi a
plus = \case
  Atom' a -> Atom' (a + 1)
  x -> Plus' x

{-
-- | Implement the Slam eliminator.
slam :: Var a => Semi a -> Semi a -> Semi a
slam x y = case x of
  {-Lamb' (Jamb{clo, cod}) ->
    slam (Crux' (mapFromList [("", cod)]) (Cell' (Atom' 0) clo)) y-}
  -- I like the reporting better below than using hold.
  Crux' cs pay | Just c <- lookup "" cs -> eval (edit x 6 y) c
  x -> Slam' x y
-}

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
  Hold' _ s c   -> Just (eval s c)
  Pull' ar ax s -> pull EvalHold ar ax <$> held s
  Plus' s       -> plus <$> held s
  -- FIXME think about the equivalent of this. Do we need different handling for
  -- Cell' or an "Edit'" here?
  -- Slam' s t     -> slam <$> held s <*> pure t
  Equl' s t     -> equl <$> held s <*> pure t  -- advance one step at a time
               <|> equl <$> pure s <*> held t
  Test' s t u   -> test <$> held s <*> pure t <*> pure u
  Fish' f s     -> fisk f <$> held s
  Look' s a     -> (/ a) <$> held s
  Edit' s a t   -> edit <$> held s <*> pure a <*> pure t
  --
  Fuse' s f     -> fuse <$> held s <*> pure f
  --
  _ -> Nothing


-- | Convert a rump into an axis. If the rump is at a level higher than this
-- one, that's still ok, provided that it's still on the right side.
slim :: Level -> Loc -> Maybe Axis
slim lvl (l, a)
  | l <= lvl             = pure $ peg (2 ^ (lvl + 1 - l) - 1) a
  | Just (R, a) <- cut a = slim lvl (l - 1, a)  -- turn e.g. +2_3 into +1_1
  | otherwise            = Nothing

loft :: Var a => Level -> Semi a -> Code a
loft lvl = \case
  -- XX we should have some printout here if lvl < l, which is a serious
  -- invariant violation that should be more legible
  Spot' (Leg' (l, a)) -> Spot case slim lvl (l, a) of
    Nothing -> 90000001
    Just ax -> ax
  Fore' x -> Fore x
  -- XX no longer a split morphism. Consider Hold' Term Crux
  Hold' _ s c -> With (loft lvl s) c
  --
  Atom' a -> Atom a
  Cell' a b -> Cell (loft lvl a) (loft lvl b)
  -- cf the discussion on luft below. Otoh, I think we do want contexts printed
  -- for cruxen.
  Crux' m ars pay -> With (loft lvl pay) (Crux m ars)
  --
  Pull' x xr a -> Pull x xr (loft lvl a)
  Plus' a -> Plus (loft lvl a)
  Equl' a b -> Equl (loft lvl a) (loft lvl b)
  Test' a b c -> Test (loft lvl a) (loft lvl b) (loft lvl c)
  Fish' h a -> Fish h (loft lvl a)
  Look' a s -> With (loft lvl a) $ Spot s
  Edit' a ax b -> Edit (loft lvl a) ax (loft lvl b)
  --
  Aura' au to -> Aura au to
  Rail' l j -> Rail (loft lvl l) (luft lvl j)
  Gate' a j -> Gate (loft lvl a) (luft lvl j)
  Core' a (s, as) b ->
    Core (loft lvl a) (fmap (luft lvl . (`Jamb` s)) as) (loft lvl b)
  Sing' a b -> Sing (loft lvl a) (loft lvl b)
  Molt' a b -> Molt (loft lvl a) (loft lvl b)
  Face' f t -> Face f (loft lvl t)
  Fuse' t h -> Fuse (loft lvl t) h
  Seal' a t -> Seal a (loft lvl t)
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

-- And this one is for lambdas, which are secretly cores, ugh.
laft l j = loft (l + 2) $ jamb j $ rump (l + 2, 6)

-- | Given a Code coming straight out of the compiler, read the subject type
-- and evaluate against the resulting seminoun.
evil :: Var a => Con a -> Code Void -> Semi a
evil Con{lvl, sut} cod = eval (read (rump (lvl, 1)) sut) (fmap absurd cod)

-- | Given a Code coming straight out of the compiler, read the subject type
-- and fully evaluate against the resulting seminoun. For external use; do NOT
-- call from the type checker
spun :: Var a => Con a -> Code Void -> Semi a
spun Con{lvl, sut} cod = spin (read (rump (lvl, 1)) sut) (fmap absurd cod)

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

class (Monad m, MonadFail m, MonadIO m, Alternative m) => MonadCheck m where
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

{-
instance MonadCheck m => MonadFail m where
  fail = bail . BailNote . pack -}

-- | Error reporting context, analogous to stack trace item. As the compiler
-- recurses deeper into its operations, it pushes these descriptions to a stack
-- so they can serve as breadcrumbs in error messages.
data Act
  =                    ActRoot
  | forall a. Var a => ActFits Fit Level (Type a) (Type a) Warp
  | forall a. Var a => ActDraw (Line a)
  | forall a. Var a => ActFind (Level, Axis) (Type a) Wing
  | forall a. Var a => ActFuse (Level, Axis) (Type a) Fish
  | forall a. Var a => ActCrop (Level, Axis) (Type a) Fish
  | forall a. Var a => ActToil (Con a) (Level, Axis) Pelt (Type a)
  | forall a. Var a => ActTore (Cube a)
  | forall a. Var a => ActTear Claw (Cube a)
  | forall a. Var a => ActTyre (Cube a)
  | forall a. Var a => ActTire Level (Set Fish) (Type a)
  | forall a. Var a => ActThin Level (Semi a)
  | forall a. Var a => ActScan (Con a) Soft
  | forall a. Var a => ActWork (Con a) Fit Soft (Type a) Warp
  | forall a. Var a => ActPlay (Con a) Soft
  |                    ActDone

-- | Compiler errors.
data Fail
  = forall a. Var a => FairFore (Semi a) a
  -- | Invariant violation: unknown seminoun on exiting tisgar.
  | forall a. Var a => PareFree Rump (Semi (Hop Rump a))
  -- | Trying to refine the type of an arm.
  |                    DrawCore Text
  -- | Cannot locate the given ally in the subject.
  | forall a. Var a => FindFail Limb (Type a)
  -- | Two cores or core types have differing arm sets.
  |                    FarmCore (Set Text) (Set Text)
  -- | The two types do not {nest, cast, equal each other}.
  | forall a. Var a => FitsFail Fit Level (Type a) (Type a)
  -- | Your fish is not compatible with any fish in the fork
  -- ClamFork (Fish) (Set Fish)
  -- | Your pelt performs a test, which is not permitted in this context.
  | forall a. Var a => ToilFish Pelt (Type a)
  -- | Here is a case you failed to consider in your pattern matching.
  |                    TireFish Axis Fish
  -- | You are stuck on something in a context you are exiting (invariant viol.)
  | forall a. Var a => ThinFree Level (Semi a)
  -- | You are trying to edit the return value of an arm pull
  | forall a. Var a => EditPull Wing (Type a)
  -- | You need to put an explicit type annotation on this subexpression.
  |                    ScanMurk Soft
  -- | You are trying to slam something which is not a gate.
  | forall a. Var a => NeedGate (Type a)
  -- | A rhetorical question had a non-rhetorical answer.
  | forall a. Var a => WorkMiss Soft (Semi a)
  -- | A rhetorical question had a non-rhetorical answer.
  | forall a. Var a => PlayMiss Soft (Semi a)
  -- | You are trying to seal a core arm.
  |                    SealPull Wing
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
  act a chk = local (a:) $ chk
    `catchCheck` (\(e::SomeException) -> bail (BailNote $ tshow e))
    -- `catchCheck` (\(e::AsyncCancelled)  -> bail (BailNote "interrupt"))
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
      `catchTrace` (\(e::SomeException) -> bail (BailNote $ tshow e))
      -- `catchTrace` (\(e::AsyncCancelled)  -> bail (BailNote "interrupt"))
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

moot :: Var a => Con a -> Type a -> Semi a -> Type a -> Con a
moot Con{lvl, sut} fom ken act = Con
  { lvl = lvl + 1
  , sut = Cell' (Molt' fom (Sing' ken act)) sut
  }

melt :: Var a => Con a -> Con a
melt Con{lvl, sut} = Con{lvl, sut = molt sut}

-- | Grow the type because we have passed under a tisgar
grow :: forall a. Var a => Type a -> Type (Hop Rump a)
grow = \case
  Spot' r -> Fore' (New r)
  Fore' x -> Fore' (Old x)
  Hold' t s c -> Hold' t (grow s) (crow c)
  --
  Atom' a -> Atom' a
  Cell' x y -> Cell' (grow x) (grow y)
  Crux' m cs s -> Crux' m (fmap crow cs) (grow s)
  --
  Pull' ar ars x -> Pull' ar ars (grow x)
  Plus' x -> Plus' (grow x)
  Equl' x y -> Equl' (grow x) (grow y)
  Test' x y z -> Test' (grow x) (grow y) (grow z)
  Fish' f x -> Fish' f (grow x)
  Look' x st -> Look' (grow x) st
  Edit' x a y -> Edit' (grow x) a (grow y)
  --
  Aura' au to -> Aura' au to
  Rail' x j -> Rail' (grow x) (jrow j)
  Gate' x j -> Gate' (grow x) (jrow j)
  Core' x (s, j) y -> Core' (grow x) (grow s, fmap crow j) (grow y)
  Sing' x y -> Sing' (grow x) (grow y)
  Molt' x y -> Molt' (grow x) (grow y)
  Face' f x -> Face' f (grow x)
  Fuse' x f -> Fuse' (grow x) f
  Seal' a x -> Seal' a (grow x)
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
    Crux m cs -> Crux m (fmap crow cs)
    --
    Pull ar ars c -> Pull ar ars (crow c)
    Plus c -> Plus (crow c)
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
    Molt c d -> Molt (crow c) (crow d)
    Face f c -> Face f (crow c)
    Fuse c f -> Fuse (crow c) f
    Seal a c -> Seal a (crow c)
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
    Hold' t s c -> Hold' t <$> go s <*> care c
    --
    Atom' a -> pure $ Atom' a
    Cell' x y -> Cell' <$> go x <*> go y
    Crux' m cs s -> Crux' m <$>traverse care cs <*> go s
    --
    Pull' ar ars x -> Pull' ar ars <$> go x
    Plus' x -> Plus' <$> go x
    Equl' x y -> Equl' <$> go x <*> go y
    Test' x y z -> Test' <$> go x <*> go y <*> go z
    Fish' f x -> Fish' f <$> go x
    Look' x st -> flip Look' st <$> go x
    Edit' x a y -> Edit' <$> go x <*> pure a <*> go y
    --
    Aura' au to -> pure $ Aura' au to
    Rail' x j -> Rail' <$> go x <*> jare j
    Gate' x j -> Gate' <$> go x <*> jare j
    Core' x (s, j) y ->
      Core' <$> go x <*> ((,) <$> go s <*> traverse care j) <*> go y
    Sing' x y -> Sing' <$> go x <*> go y
    Molt' x y -> Molt' <$> go x <*> go y
    Face' f x -> Face' f <$> go x
    Fuse' x f -> Fuse' <$> go x <*> pure f
    Seal' a x -> Seal' a <$> go x
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
    Crux m cs -> Crux m <$> traverse care cs
    --
    Pull ar ars c -> Pull ar ars <$> care c
    Plus c -> Plus <$> care c
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
    Molt c d -> Molt <$> care c <*> care d
    Face f c -> Face f <$> care c
    Fuse c f -> Fuse <$> care c <*> pure f
    Seal a c -> Seal a <$> care c
    Noun -> pure Noun
    Void -> pure Void
    Type -> pure Type
    --
    With c d -> With <$> care c <*> care d
    Push c d -> Push <$> care c <*> care d


--------------------------------------------------------------------------------
-- Core operations of the compiler ---------------------------------------------
--------------------------------------------------------------------------------

-- Mesh calculus ---------------------------------------------------------------

-- | Given a mesh, replace the indicated parts of the seminoun with rumps
-- stuck on the given location. This is required for fits checking of core
-- values, because those parts of the subject are assumed to change (and may not
-- even be of the correct type for the arm bodies!).
sift :: Var a => Mesh -> Semi a -> Loc -> Semi a
sift mes ken loc = case mes of
  Mesh m n -> Cell' (sift m (ken / 2) (loc / 2)) (sift n (ken / 3) (loc / 3))
  Meat -> ken
  Milk -> rump loc

-- | Given a subject type, construct its mesh. Those parts of the subject type
-- where Molt' is found (arising from =:) are considered "milky"; the others
-- (arising from =+) "meaty." If part of the type is stuck/unknown, it is
-- presumed milk.
knit :: Var a => Type a -> Mesh
knit = \case
  t | Just t' <- held t -> knit t'
  Spot'{} -> Milk
  Fore'{} -> Milk
  Hold'{} -> error "knit: impossible unheld Hold'"
  --
  Atom'{} -> error "knit: atom"
  Cell' t u -> Mesh (knit t) (knit u)
  Crux'{} -> error "knit: crux"
  --
  Pull'{} -> Milk
  Plus'{} -> error "knit: plus"
  Equl'{} -> error "knit: equl"
  Test'{} -> Milk
  Fish'{} -> error "knit: fish"
  Look'{} -> Milk
  Edit'{} -> Milk
  --
  Aura'{} -> Milk
  Rail' t j -> Milk  -- XX could actually be mesh if we wanted to do more
  Gate'{} -> Milk
  Core'{} -> Milk
  Sing' _ _ -> Meat
  Molt' _ _ -> Milk   -- Consider a `| Mead (Semi a)` case for sing in molt.
  Face' _ t -> knit t
  Fuse' t _ -> knit t
  Seal' _ t -> knit t
  Noun' -> Milk
  Void' -> Milk
  Type' -> Milk

-- | Given a type, construct its molten version. Those parts of the type where
-- Molt' is found will be replaced by the replacement types (on the left of the
-- Molt', which are otherwise ignored) in the "molten version." The non-Molt
-- parts will stay the same.
--
-- Consider the below example:
--
--   =+  `t`e
--   =+  `u`f
--   =:  v  `w`g
--   |%
--   ..
--   --
--
-- The subject type before the |% is [e|t f|u $;(v g|w)]. This is also the
-- actual payload type of the core. However, the formal payload type of the core
-- is instead the "molten" version of this type: [e|t f|u v]. The two singletons
-- mean the user may have difficulty in changing these values when pulling an
-- arm. On the other hand, the user is free to change the third value to
-- anything of type v, and may in fact be required to, if w does not nest under
-- v.
molt :: Type a -> Type a
molt = \case
  -- XX we don't bother with Rails. Should we though? Seems we can only do so
  -- on the right, and that would require making Knit computational, which seems
  -- excessive.
  Cell' t u -> Cell' (molt t) (molt u)
  Molt' t _ -> t
  t         -> t


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

-- | Context for managing subtyping relations in the presence of sealing; passed
-- down (like Reader) but not side-to-side.
data Warp = Warp
  { lax :: Set Axis  -- ^ axes sealed to the left
  , rax :: Set Axis  -- ^ axes sealed to the right
  , pax :: Axis      -- ^ current axis
  }
  deriving Show

-- | Empty warp. Knowing when to shed is, uh, a thing that's very important :/
-- I use a pattern so I can make it all caps to be very visible.
pattern SHED :: Warp
pattern SHED <- Warp ((== mempty) -> True) ((== mempty) -> True) 1 where
  SHED = Warp mempty mempty 1

instance Peg Warp where
  wap@Warp{pax} / a = wap { pax = pax / a }

-- | State for tracking the the subtyping relations of recursive types; passed
-- and returned side-to-side (like State).
data Weft a = Weft
  { seg :: Set (Type a)          -- ^ recursion points we have passed on left
  , reg :: Set (Type a)          -- ^ recursion points we have passed on right
  , gil :: Set (Type a, Type a)  -- ^ induction hypotheses
  }

deriving instance (Show a) => Show (Weft a)

{-
laceMap :: (Ord a, Ord b) => (a -> b) -> Weft a -> Weft b
laceMap f Weft{seg, reg, gil} = Weft
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
fits fit lvl t u = void $ fest fit lvl t u
  Warp
    { lax = mempty
    , rax = mempty
    , pax = 1
    }
  Weft
    { seg = mempty
    , reg = mempty
    , gil = mempty
    }

-- | When calling fits from work, we often have need to abstract out parts of
-- the left or right. That is, we may come in with our own pre-filled Warp.
fist :: forall a m. (MonadCheck m, Var a)
     => Fit -> Level -> Type a -> Type a -> Warp -> m ()
fist fit lvl t u wap = void $ fest fit lvl t u wap
  Weft
    { seg = mempty
    , reg = mempty
    , gil = mempty
    }

firm :: forall a r m. (MonadCheck m)
     => Map Text a -> r -> (Text -> a -> r -> m r) -> m r
firm a state act = do
  let arms = keysSet a
  foldlM step state arms
 where
  step state arm = do
    let x = fromJust $ lookup arm a
    act arm x state

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
     => Fit -> Level -> Type a -> Type a -> Warp -> Weft a -> m (Weft a)
fest fit lvl t u wap@Warp{lax, rax, pax} wef@Weft{seg, reg, gil} =
 act (ActFits fit lvl t u wap) case (t, u) of
  _ | (t, u) `elem` gil -> pure wef

  _ | Just t' <- held t ->
        fest fit lvl t' u wap Weft
          { seg = insertSet t seg
          , reg
          , gil = insertSet (t, u) gil
          }

  _ | Just u' <- held u ->
        fest fit lvl t u' wap Weft
          { seg
          , reg = insertSet u' reg
          , gil = insertSet (t, u) gil
          }

  (Hold'{}, _) -> error "fits: invariant violation: Hold' should be held"
  (_, Hold'{}) -> error "fits: invariant violation: Hold' should be held"

  -- type "metadata" section

  (Face' _ t, u) -> fest fit lvl t u wap wef
  (t, Face' _ u) -> fest fit lvl t u wap wef

  -- The data in the formal part of a Molt is completely ignored, except when
  -- we need to read a formal type out of the subject when a core is arising.
  (Molt' _ t, u) -> fest fit lvl t u wap wef
  (t, Molt' _ u) -> fest fit lvl t u wap wef

  (Sing' s t, Sing' z u) -> do
    wef <- fest FitSame lvl s z SHED wef
    fest fit lvl t u wap wef

  (Sing' s t, _) -> do
    note "TTT" t
    note "UUU" u
    fest fit lvl t u wap wef

  (Fuse' t f, Fuse' u g) -> do
    when (not $ prey g f) fitsFail
    fest fit lvl t u wap wef

  (Fuse' t f, u) -> fest fit lvl t u wap wef

  -- Record axis of sealing on the left
  (Seal' axe t, u) ->
    fest fit lvl t u wap
      { lax = insertSet (pax / axe) lax
      , rax
      , pax
      }
      wef

  -- Record axis of sealing on the right; perform binding check on the left.
  (t, Seal' axe u) -> do
    seal lvl axe t  -- TODO perf: thread weft, avoid double-traversal of t.
    fest fit lvl t u wap
      { lax
      , rax = insertSet (pax / axe) rax
      , pax
      }
      wef

  -- Encounter axes of sealing
  _ | pax `elem` rax -> pure wef  -- intuition: gold nests under lead
  _ | pax `elem` lax -> fitsFail  -- intuition: lead doesn't nest under gold

  (Noun', Noun') -> pure wef
  (Noun', _) -> fitsFail
  (_, Noun') -> case fit of
    FitSame -> fitsFail
    FitNest -> pure wef
    FitCast -> pure wef

  (Void', Void') -> pure wef
  (Void', _) -> case fit of
    FitSame -> fitsFail
    FitNest -> pure wef
    FitCast -> pure wef
  (_, Void') -> fitsFail

  (Spot' a, Spot' b)
    | a == b -> pure wef
    | otherwise      -> fitsFail
  (Spot'{}, _) -> fitsFail
  (_, Spot'{}) -> fitsFail

  (Fore' r, Fore' s)
    | r == s    -> pure wef
    | otherwise -> fitsFail
  (Fore'{}, _) -> fitsFail
  (_, Fore'{}) -> fitsFail

  --

  (Atom' a, Atom' b) | a == b -> pure wef
  (Atom'{}, _) -> fitsFail
  (_, Atom'{}) -> fitsFail

  (Cell' v w, Cell' v' w') -> do
    wef <- fest fit lvl v v' wap { pax = pax / 2 } wef
    fest fit lvl w w' wap { pax = pax / 3 } wef

  {-
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
    fest fit (lvl + 1) (jamb j new) (jamb k new) wef
   where
    new = rump (lvl + 1, 2)

  -- XX should we read and do comb explosion?

  (Lamb' j, _) ->
    fest fit (lvl + 1) (jamb j new) (Slam' u new) wef
   where
    new = rump (lvl + 1, 2)

  (_, Lamb' k) ->
    fest fit (lvl + 1) (Slam' t new) (jamb k new) wef
   where
    new = rump (lvl + 1, 2)
  -}

  -- We need a version of the eta-beta rule here, akin to that commented out for
  -- Lamb'. Was expecting this to stump me for days, but it actually took 15m.
  -- The solution is to introduce a new kind of =+ in Code which pushes an
  -- unknown rather than a known. This is the new way unknowns enter into the
  -- subject, having previously entered via Lamb directly.
  --
  -- If we want eta-beta, we have to evaluate all arms here.
  (Crux' m as p, Crux' n bs q) -> do
    wef <- fest fit lvl p q SHED wef
    let p' = sift m p new
    let q' = sift m q new
    farm as bs wef \_ a b wef ->
      fest fit lvl (eval p' a) (eval q' b) SHED wef
   where
    new = (lvl + 1, 2)

  -- XX figure out tests for these
  (Crux' m as p, _) -> do
    wef <- fest fit lvl p (u / 3) SHED wef
    let p' = sift m p new
    let q' = sift m (u / 3) new
    let ks = keysSet as
    firm as wef \nom a -> fest fit lvl (eval p' a) (Pull' nom ks q') SHED
   where
    new = (lvl + 1, 2)

  (_, Crux' n bs q) -> do
    wef <- fest fit lvl (t / 3) q SHED wef
    let p' = sift n (t / 3) new
    let q' = sift n q new
    let ks = keysSet bs
    firm bs wef \nom b -> fest fit lvl (Pull' nom ks p') (eval q' b) SHED
   where
    new = (lvl + 1, 2)

  (Pull' ar _ t, Pull' br _ u)
    -- FitSame for the same reason as with Slam' below. TODO rewrite comment.
    -- In fact it seems we do this with every eliminator. We should say more
    -- about that, when we are not as stupid as we are right now.
    | ar == br  -> fest FitSame lvl t u SHED wef
    | otherwise -> fitsFail
  (Pull'{}, _) -> fitsFail
  (_, Pull'{}) -> fitsFail

  (Plus' v, Plus' w) -> fest FitSame lvl v w SHED wef
  (Plus'{}, _) -> fitsFail
  (_, Plus'{}) -> fitsFail

  -- Since it hasn't been evaluated away, we are dealing with an opaque type
  -- function application. This means we have no choice but to regard the
  -- function as invariant in its argument.
  {-
  (Slam' v w, Slam' v' w') -> fest fit lvl v v' wef >>= fest FitSame lvl w w'
  (Slam'{}, _) -> fitsFail
  (_, Slam'{}) -> fitsFail
  -}

  -- To prove v == w and v' == w' have the same value, we can prove:
  --   - v = v' and w = w'; or
  --   - v = w' and w = v'
  (Equl' v w, Equl' v' w') -> asum
    [ fest FitSame lvl v v' SHED wef >>= fest FitSame lvl w w' SHED
    , fest FitSame lvl v w' SHED wef >>= fest FitSame lvl w v' SHED
    ]
  -- You could also imagine having a (Equl' a b, Atom 0) case here, but I need
  -- to think carefully about whether FitsSame seminoun implies .= runtime noun.
  (Equl'{}, _) -> fitsFail
  (_, Equl'{}) -> fitsFail

  (Test' u v w, Test' u' v' w') -> ogle (fest FitSame lvl u u' SHED wef)
    >>= \case
      Right wef -> do
       wef <- fest fit lvl v v' wap wef
       fest fit lvl w w' wap wef
      Left _ -> do
        wef <- fest fit lvl v v' wap wef
        wef <- fest fit lvl v w' wap wef
        wef <- fest fit lvl w v' wap wef
        fest fit lvl w w' wap wef
  (Test' _ v w, u) -> do
    wef <- fest fit lvl v u wap wef
    fest fit lvl w u wap wef
  (t, Test' _ v w) -> do
    wef <- fest fit lvl t v wap wef
    fest fit lvl t w wap wef

  (Fish' f v, Fish' f' v') -> do
    when (f /= f') fitsFail
    fest FitSame lvl v v' SHED wef
  (Fish'{}, _) -> fitsFail
  (_, Fish'{}) -> fitsFail

  -- The assumption is that these are fully evaluated. This rules out Looks
  -- stacked on top of Looks, as well of Looks on top of cells. Accordingly the
  -- rules are pretty tight. I don't think there's any equiv of the beta-eta
  -- conversion we saw above with functions/arms here.
  (Look' b st, Look' c ub)
    | st == ub  -> fest fit lvl b c SHED wef
    | otherwise -> fitsFail
  (Look'{}, _) -> fitsFail
  (_, Look'{}) -> fitsFail

  -- TODO really think through this
  --   - should we SHED?
  --   - should the FitSame from Slam' be here rather than / in addition to
  --     in Pull'?
  --   - Are there nonidentical cases to consider?
  --       - Differing axes
  --       - (Edit, non-Edit)
  (Edit' s a t, Edit' s' a' t')
    | a == a' -> fest fit lvl s s' SHED wef >>= fest fit lvl t t' SHED
    | otherwise -> fitsFail
  (Edit'{}, _) -> fitsFail
  (_, Edit'{}) -> fitsFail

  (Aura' au as, Aura' ag bs) -> case fit of
    FitCast -> tule as bs
    FitNest -> if ag `isPrefixOf` au   then tule as bs else fitsFail
    FitSame -> if ag == au && as == bs then pure wef   else fitsFail
   where
    tule _         Bowl                               = pure wef
    tule (Fork as) (Fork bs) | as `Set.isSubsetOf` bs = pure wef
                             | otherwise              = fitsFail
    tule Bowl      _                                  = fitsFail

  -- Should empties otherwise be ruled out here?
  -- (Aura' au (Fork ss), Void') | ss == mempty -> pure wef

  (Rail' v j, Rail' w k) -> do
    wef <- fest fit lvl v w wap { pax = pax / 2 } wef
    -- Note that, following Cardelli, we read the *smaller* type.
    let x = read new v
    fest fit (lvl + 1) (jamb j x) (jamb k x) wap { pax = pax / 3 } wef
   where
    new = rump (lvl + 1, 2)

  (Rail' v j, Cell' w u) -> do
    wef <- fest fit lvl v w wap { pax = pax / 2 } wef
    -- Note that, following Cardelli, we read the *smaller* type.
    fest fit (lvl + 1) (jamb j $ read new v) u wap { pax = pax / 3 } wef
   where
    new = rump (lvl + 1, 2)

  (Cell' v u, Rail' w k) -> do
    wef <- fest fit lvl v w wap { pax = pax / 2 } wef
    -- Note that, following Cardelli, we read the *smaller* type.
    fest fit (lvl + 1) u (jamb k $ read new v) wap { pax = pax / 3 } wef
   where
    new = rump (lvl + 1, 2)

  -- Allow use of cell of types as type
  (Cell' v u, Type') -> do
    wef <- fest fit lvl v Type' wap { pax = pax / 2 } wef
    fest fit lvl u Type' wap { pax = pax / 3 } wef

  (Gate' v j, Gate' w k) -> do
    wef <- fest fit lvl w v SHED wef
    -- Note that, following Cardelli, we read the *smaller* type.
    let x = read new w
    fest fit (lvl + 1) (jamb j x) (jamb k x) SHED wef
   where
    new = rump (lvl + 1, 2)

  {-
  -- As a foretaste of the forthcoming ^?/$? machinery, we implement the below
  -- special case, where $-(a b) is a stand-in for $?(+7 $|(^ [a *] % b)),
  -- modulo an impedance mismatch in the layout of the b Jambs.
  (Core' fom (clo, mapToList -> [("", ret)]) act, Gate' w k) -> case fit of
    FitSame -> fitsFail
    FitCast -> go
    FitNest -> go
   where
    go = do
      -- early binding check
      actCon <- peek (lvl, 1) act 3  -- XX is this right with (lvl, 1)?
      fomCon <- peek (lvl, 1) fom 3
      wef <- fest FitNest lvl actCon fomCon wef  -- yes, nest
      -- argument check
      wef <- fest fit lvl (Cell' w Void') fom wef
      -- result check
      let j = Jamb ret clo
      fest fit (lvl + 1) (jamb j $ read new fom) (jamb k $ read (new / 2) w) wef

    -- XX ugh, do I have any of this right?
    new = rump (lvl + 1, 2)
  -}

  (Core' fom (clo, arms) act, Core' fom' (clo', arms') act') -> do
    -- actual types are covariant
    wef <- fest fit lvl act act' wap { pax = pax / 3 } wef
    -- formal types are contravariant
    wef <- fest fit lvl fom' fom wap { pax = pax / 3 } wef
    let as = fmap (`Jamb` clo)  arms
    let bs = fmap (`Jamb` clo') arms'
    -- The arms must nest in all possible universes where the payloads nest.
    -- Because arm bodies can do %=, reading the smaller actual type is too
    -- specific.
    let y = read new fom'
    farm as bs wef \_ a b wef ->
      fest fit (lvl + 1) (jamb a y) (jamb b y) SHED wef
   where
    new = rump (lvl + 1, 2)

  (Type', Type') -> pure wef

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
  fitsFail = bail (FitsFail fit lvl t u)


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
  , lix :: Set Axis  -- ^ axes sealed
  , las :: [Dash a]  -- ^ stack of steps taken
  }

-- | Record one step of the path we took to reach the result of a find.
data Dash a
  -- | We have passed under a face annotation
  = DashFace Face
  -- | We have passed under a singleton value annotation
  | DashSing (Semi a)
  -- | We have passed underneath metadata specifying what this part of the type
  -- should be replaced with when the whole type becomes a core's formal type.
  | DashMolt (Type a)
  -- | We have passed under an abstract type marker
  | DashSeal Axis
  -- | We have passed into the left of a nondependent cell, and record the right
  | DashCellLeft (Type a)
  -- | We have passed into the left of a dependent cell, and record the right
  | DashRailLeft (Jamb a)
  -- | We have passed into the right of any cell, and record the left
  | DashCellRight (Type a)
  -- | We have passed into the payload of a core, and record the battery
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
data Draw
  = DrawFine  -- ^ Refine a part of the subject to a subtype
  | DrawEdit  -- ^ Edit a part of the subject to an arbitrary type

-- | When the found type (lyt) has been refined or edited, zip the zipper back
-- up to get the fullsize refined or edited subject type.
draw :: Var a => Draw -> Line a -> Line a
draw mod lin@Line{lev, loc, lyt, lix, las} = case las of
  [] -> lin
  DashFace f : las -> draw mod Line
    { lev
    , loc
    , lyt = Face' f lyt
    , lix
    , las
    }
  DashSing s : las -> draw mod Line
    { lev
    , loc
    , lyt = case mod of
        DrawFine -> Sing' s lyt
        DrawEdit -> lyt
    , lix
    , las
    }
  DashMolt t : las -> draw mod Line
    { lev
    , loc
    , lyt = Molt' t lyt
    , lix
    , las
    }
  DashSeal a : las -> draw mod Line
    { lev
    , loc
    , lyt = Seal' a lyt
    , lix
    , las
    }
  DashCellLeft tr : las -> draw mod Line
    { lev
    , loc = rise loc
    , lyt = Cell' lyt tr
    , lix
    , las
    }
  DashRailLeft jr : las -> case mod of
    DrawFine -> draw mod Line
      { lev
      , loc = rise loc
      , lyt = Rail' lyt jr  -- XX is this correct? prev did as DrawEdit
      , lix
      , las
      }
    DrawEdit -> draw mod Line
      { lev
      , loc = rise loc
      , lyt = Cell' lyt (jamb jr $ read (rump (lev, rise loc / 2)) lyt)
      , lix
      , las
      }
  DashCellRight tl : las -> draw mod Line
    { lev
    , loc = rise loc
    , lyt = Cell' tl lyt
    , lix
    , las
    }
  DashCorePayload fom bat : las -> draw mod Line
    { lev
    , loc = rise loc
    , lyt = Core' fom bat lyt
    , lix
    , las
    }

-- XX think more carefully about return value design
find :: forall a m. (MonadCheck m, Var a)
     => (Level, Axis) -> Type a -> Wing -> m (Stub, Line a)
find (lev, loc) typ win = act (ActFind (lev, loc) typ win) $
  fond (read (rump (lev, 1)) typ) Line
    { lev
    , loc
    , lyt = typ
    , lix = mempty
    , las = []
    }
    win

fond :: forall a m. (MonadCheck m, Var a)
     => Semi a -> Line a -> Wing -> m (Stub, Line a)
fond ken lin = \case
  [] -> pure (Leg 1, lin)
  l:ls -> fond ken lin ls >>= \case
    -- arm1.arm2: as in 140, go back up to root of core.
    (rest, lin) -> do
      (st, con) <- limb (ken / a) lin l
      pure (pole a st, con)
     where
      a = case rest of
        Leg a -> a
        Arm a _ _ -> a
 where
  limb :: Semi a -> Line a -> Limb -> m (Stub, Line a)
  limb ken lin = \case
    Ares   -> pure (Leg 1, ares lin)
    Axis a -> (Leg a,) <$> axis a ken lin
    Ally f -> ally f ken lin

-- | Strip metadata (faces, forks, sings)
-- XX FIXME this doesn't work right now, rethink
ares :: Var a => Line a -> Line a
ares lin@Line{lev, loc, lyt, lix, las} = case lyt of
  Face' f t -> ares Line
    { lev
    , loc
    , lyt = t
    , lix
    , las = DashFace f : las
    }

  Sing' s t -> ares Line
    { lev
    , loc
    , lyt = t
    , lix
    , las = DashSing s : las
    }

  -- also need test case eventually, I think???

  _ -> lin

axis :: forall a m. (MonadCheck m, Var a)
     => Axis -> Semi a -> Line a -> m (Line a)
axis a ken lin@Line{lev, loc, lyt, lix, las} = case (cut a, lyt) of
  -- We must pass under Molts no matter what
  (_, Molt' fom act) -> axis a ken Line
    { lev
    , loc
    , lyt = act
    , lix
    , las = DashMolt fom : las
    }

  (_, Seal' axe t) -> axis a ken Line
    { lev
    , loc
    , lyt
    , lix = insertSet axe lix
    , las = DashSeal a : las
    }

  (Nothing, _)
    -- FIXME this error message has unhelpful type and axis
    | a `elem` lix -> bail (FindFail (Axis a) lyt)
    | otherwise    -> pure lin

  (_, _) | Just t <- held lyt -> axis a ken Line
    { lev
    , loc
    , lyt = t
    , lix
    , las  -- XX should we add a trace frame?
    }

  -- We want, in the pelt calculus, peek 2/peek 3 to give void on void
  (_, Void') -> pure Line
    { lev
    , loc = loc / a
    , lyt = Void'
    , lix
    , las  -- there's really no good answer here
    }

  (_, Face' f t) -> axis a ken Line
    { lev
    , loc
    , lyt = t
    , lix
    , las = DashFace f : las
    }

  (_, Sing' s t) -> axis a s Line
    { lev
    , loc
    , lyt = t
    , lix
    , las = DashSing s : las
    }

  -- This one is interesting. We propagate Tests inwards.
  -- This is necessary for compatibility with the "decision trees"
  -- produced by crop.
  -- XX allegedly broken. Requires dual traces in the Dash infra, it seems.
  (_, Test' x t u) -> do
    lin <- axis a ken lin { lyt = t }
    lon <- axis a ken lin { lyt = u }
    -- XX There are questions around leaving behind the las of u
    pure Line
      { lev
      , loc = loc / a
      , lyt = Test' x (long lin) (long lon)
      , lix
      , las
      }

  (Just (L, a), Cell' tl tr) -> axis a (ken / 2) Line
    { lev
    , loc = loc / 2
    , lyt = tl
    , lix
    , las = DashCellLeft tr : las
    }

  (Just (L, a), Rail' tl jr) -> axis a (ken / 2) Line
    { lev
    , loc = loc / 2
    , lyt = tl
    , lix
    , las = DashRailLeft jr : las
    }

  (Just (R, a), Cell' tl tr) -> axis a (ken / 3) Line
    { lev
    , loc = loc / 3
    , lyt = tr
    , lix
    , las = DashCellRight tl : las
    }

  (Just (R, a), Rail' tl jr) -> axis a (ken / 3) Line
    { lev
    , loc = loc / 3
    -- FIXME XX are we sure this should be loc / 2 instead of just 2??
    -- What is more, once Jambs have levels inside of them, should we even
    -- be using the ambient level? What is going on here?
    , lyt = jamb jr (ken / 2)
    , lix
    , las = DashCellRight tl : las
    }

  (Just (L, a), Core' _ _ act) -> axis a (ken / 2) Line
    { lev
    , loc = loc / 2
    , lyt = Noun'
    -- This way, if the user edits, she'll get a cell back rather than a core.
    , lix
    , las = DashCellLeft act : las
    }

  (Just (R, a), Core' fom bat pay) -> axis a (ken / 3) Line
    { lev
    , loc = loc / 3
    , lyt = pay
    , lix
    , las = DashCorePayload fom bat : las
    }

  -- XX an old note reads: "arguably for Liskov, should be Noun :("; rethink
  -- Should this have been Void'? I think so.
  (_, _) -> bail (FindFail (Axis a) lyt)

ally :: forall a m. (MonadCheck m, Var a)
     => Term -> Semi a -> Line a -> m (Stub, Line a)
ally f ken lin@Line{lyt} =
  maybe (bail $ FindFail (Ally f) lyt) id $ lope ken lin
 where
  lope :: Semi a -> Line a -> Maybe (m (Stub, Line a))
  lope ken lin@Line{lev, loc, lyt, lix, las} = case lyt of

    _ | loc `elem` lix -> Nothing

    Face' (Mask m) t
      | f == m -> Just $ pure $ (Leg 1,) $ Line
          { lev
          , loc
          , lyt = t
          , lix
          , las = DashFace (Mask m) : las
          }
      | otherwise -> Nothing

    Face' (Link ls) t
      | Just (a, fs) <- lookup f ls -> Just $ (Leg a,) <$> axis a ken lin
      | otherwise -> lope ken Line
          { lev
          , loc
          , lyt = t
          , lix
          , las = DashFace (Link ls) : las
          }

    Sing' s t -> lope s Line
      { lev
      , loc
      , lyt = t
      , lix
      , las = DashSing s : las
      }

    Molt' t u -> lope ken Line
      { lev
      , loc
      , lyt = u
      , lix
      , las = DashMolt t : las
      }

    Seal' a t -> lope ken Line
      { lev
      , loc
      , lyt = t
      , lix = insertSet a lix
      , las
      }

    Cell' tl tr -> asum
      [ fmap (first (pole 2)) <$> lope (ken / 2) Line
          { lev
          , loc = loc / 2
          , lyt = tl
          , lix
          , las = DashCellLeft tr : las
          }
      , fmap (first (pole 3)) <$> lope (ken / 3) Line
          { lev
          , loc = loc / 3
          , lyt = tr
          , lix
          , las = DashCellRight tl : las
          }
      ]

    Rail' tl jr -> asum
      [ fmap (first (pole 2)) <$> lope (ken / 2) Line
          { lev
          , loc = loc / 2
          , lyt = tl
          , lix
          , las = DashRailLeft jr : las
          }
      , fmap (first (pole 3)) <$> lope (ken / 3) Line
          { lev
          , loc = loc / 3
          , lyt = jamb jr (ken / 2)
          , lix
          , las = DashCellRight tl : las
          }
      ]

    Core' fom (clo, bat) act -> asum
      [ lookup f bat <&> \jam -> pure $ (Arm 1 f (keysSet bat),) $ Line
          { lev
          , loc  -- NOT loc / 2
          , lyt  -- type of the whole core
          , lix
          , las
          }
      , fmap (first (pole 3)) <$> lope (ken / 3) Line
          { lev
          , loc = loc / 3
          , lyt = act
          , lix
          , las = DashCorePayload fom (clo, bat) : las
          }
      ]

    _ | Just t <- held lyt -> lope ken Line
        { lev
        , loc
        , lyt = t
        , lix
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
lobe :: Claw -> Fish
lobe = \case
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
        where missing = if explicits == mempty then Tuna else lobe cl
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
          Eons t rest -> Eons (fuse t (lobe cl)) <$> pure rest
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

-- | Guarantee that the given seminoun isn't stuck on anything in the head of
-- the subject, transforming it into a seminoun that is guaranteed to have all
-- levels strictly less than that passed in. This is used in a single place, to
-- make sure core arm types don't reference core arm values or the whole core.
thin :: forall a m. (MonadCheck m, Var a)
     => Level -> Semi a -> m (Semi a)
thin lvl bas = act (ActThin lvl bas) $ go bas
 where
  -- Procedure for reducing excessive levels. Make sure you understand this.
  slim :: Loc -> Maybe Loc
  slim (l, a)
    | l < lvl              = pure (l, a)
    | Just (R, a) <- cut a = slim (l - 1, a)  -- turn e.g. +2_3 into +1_1
    | otherwise            = Nothing
  go = \case
    Spot' (Leg' (l, a)) -> case slim (l, a) of
      Nothing     -> bail (ThinFree l bas)
      Just (l, a) -> pure $ Spot' (Leg' (l, a))
    Fore' x -> pure $ Fore' x
    Hold' t s c -> Hold' t <$> go s <*> pure c
    --
    Atom' a -> pure $ Atom' a
    Cell' x y -> Cell' <$> go x <*> go y
    Crux' m cs s -> Crux' m cs <$> go s
    --
    Pull' ar ars x -> Pull' ar ars <$> go x
    Plus' x -> Plus' <$> go x
    Equl' x y -> Equl' <$> go x <*> go y
    Test' x y z -> Test' <$> go x <*> go y <*> go z
    Fish' f x -> Fish' f <$> go x
    Look' x st -> flip Look' st <$> go x
    Edit' x a y -> Edit' <$> go x <*> pure a <*> go y
    --
    Aura' au to -> pure $ Aura' au to
    Rail' x (Jamb cod clo) -> Rail' <$> go x <*> (Jamb cod <$> go clo)
    Core' x (s, j) y ->
      Core' <$> go x <*> ((, j) <$> go s) <*> go y
    Sing' x y -> Sing' <$> go x <*> go y
    Molt' x y -> Molt' <$> go x <*> go y
    Face' f x -> Face' f <$> go x
    Fuse' x f -> Fuse' <$> go x <*> pure f
    Seal' a x -> Seal' a <$> go x
    Noun' -> pure Noun'
    Void' -> pure Void'
    Type' -> pure Type'

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
                $ draw DrawFine lin { lyt = face' [Link $ clop $ derm p]
                                          $ fuse lyt fis }
        -- fal <- draw in { lyt = crop lyt fis }
        fal <- pure con
        pure (Fish fis $ Spot axe, tru, fal, singleton $ gill axe fis)

      Arm{} -> fall

  _ -> fall

 where
  fall = do
    (x, ns) <- work con FitNest sof Flag'
    pure (x, con, con, ns)

-- | Perform type checking for centis.
-- FIXME WRONG suppose we have a singleton [1 2]|[@ @] and we edit the
-- +2 to 3. The resulting type will be [1 2]|[3|@ @] which is invalid.
-- It may be that we should just abandon singleton types entirely and go back to
-- having a separate seminoun.
fend :: forall a m. (MonadCheck m, Var a)
     => Con a -> Wing -> [(Wing, Soft)] -> m (Code Void, Type a, Set Fish)
fend con@Con{lvl, sut} w whs = do
  (st, lin) <- find (lvl, 1) sut w
  let axe = case st of { Leg a -> a; Arm a _ _ -> a }
  let ken = read (rump (lvl, axe)) sut
  (c, _, lin, ms) <- foldlM
    loop
    (Spot (loc lin), ken, lin {las = []}, singleton Tuna)
    whs
  case st of
    Leg _ -> pure (c, lyt lin, ms)
    Arm _ arm arms -> case lyt lin of
      Core' fom (clo, lookup arm -> Just cod) act -> do
        -- late binding check: actual must nest under formal
        fits FitNest lvl act fom
        -- compute return type given actual argument value seminoun
        note "COD " cod
        note "CLO " clo
        note "RED " $ read (rump (lvl, loc lin / 3)) act
        let ret = jamb Jamb{cod, clo} $ read (rump (lvl, loc lin / 3)) act
        note "RET " ret
        pure (Pull arm arms c, ret, ms)
      t -> bail (BailNote $
        "fend: invariant violation: expected core with arm " <> arm <>
        " but got " <> tshow t)
 where
  loop :: (Code Void, Semi a, Line a, Set Fish)
       -> (Wing, Soft)
       -> m (Code Void, Semi a, Line a, Set Fish)
  loop (c, ken, lin, ms) (win, sof) = do
    note "LON " lin
    (st, lin) <- fond ken lin win
    (rhs, typ, ns) <- case las lin of
      -- XX HACK rethink :/
      DashMolt t : _ -> do
        note "MOLT" t
        do { (rhs, ns) <- work con FitNest sof t; pure (rhs, t, ns) }
    --  <|> play con sof
      _ -> note "MEAT" () >> play con sof
    note "RHS " rhs
    note "TYP " typ
    note "LIN " lin
    let big = draw DrawEdit lin { lyt = Sing' (evil con rhs) typ }
    note "BIG " big
    case st of
      Arm{} -> bail (EditPull win (lyt lin))
      Leg ax -> pure
        ( Edit c ax rhs
        -- FIXME I feel so helpless right now
        , read (rump (lvl, ax)) (lyt big)
        -- The singleton is needed for the dependency to work!
        , big
        , swam ms ns
        )

-- | Perform early binding check to seal a type.
-- TODO thread the Weft into the nest check in (R, Core')
seal :: forall a m. (MonadCheck m, Var a)
     => Level -> Axis -> Type a -> m ()
seal lvl a t = case (cut a, t) of
  (Nothing, _) -> pure ()
  (_, t) | Just t' <- held t -> seal lvl a t'
  (_, Spot'{}) -> sealFail
  (_, Fore'{}) -> sealFail
  (_, Hold'{}) -> error "seal: hold"
  --
  (_, Atom'{}) -> sealFail
  (Just (L, a), Cell' t _) -> seal lvl a t
  (Just (R, a), Cell' _ t) -> seal lvl a t
  (_, Crux'{}) -> sealFail
  --
  (_, Pull'{}) -> sealFail
  (_, Plus'{}) -> sealFail
  (_, Equl'{}) -> sealFail
  (_, Test' _ t u) -> seal lvl a t >> seal lvl a u
  (_, Fish'{}) -> sealFail
  (_, Look'{}) -> sealFail
  (_, Edit'{}) -> sealFail
  --
  (_, Aura'{}) -> pure ()
  (Just (L, a), Rail' t _) -> seal lvl a t
  (Just (R, a), Rail' t j) -> seal (lvl + 1)
                                   a
                                   (jamb j $ read (rump (lvl + 1, 2)) t)
  (Just (L, a), Core'{}) -> pure ()  -- XX think about this.
  (Just (R, a), Core' fom _ act) -> do
    -- Note: Consider a core type nested in the fom/act of a core type. E.g.
    -- $|  ^=  t
    --     $|  A
    --       B
    --     --
    --   ^=  u
    --   $|  C
    --     D
    --   --
    -- --
    -- and suppose we wish to seal +7. The effect is to turn [both of] the inner
    -- core[s] lead. So we must have B < A and D < C. Meanwhile we must *also*
    -- have u=$|(C D) < t=$|(A B) which means A < C and D < B. By transitivity
    -- then, u < t and B < A implies D < C, so we only need to recursively seal
    -- the formal core.
    --
    -- However, what the above analysis fails to understand is that if the
    -- formal nested core type fails the binding check, then arms of the outer
    -- core will never be able to pull the inner core's arms no matter what.
    -- This means the actual core needs to bind if and only if the formal core
    -- binds. The theorem of the previous paragraph in fact says that if t binds
    -- and u < t, then u binds. So to handle internal arm pulls, we don't need
    -- to recursively seal at all.
    --
    -- But we might also pull an inner core arm from *outside* the outer core.
    -- This certainly requires the actual inner core to bind, so we do need to
    -- test it.
    --
    -- Summarizing:
    --   - For the outer core arms to be valid, the actual inner core must bind
    --     iff the formal inner core does. But this is true if the inner cores
    --     nest
    --   - For a person sitting outside the outer core to pull an inner core arm
    --     the actual inner core must bind (which is a weaker condition than the
    --     formal core binding).
    --   - I hope you had fun.
    seal lvl a act  -- in case we have any inner cores
    low <- peek (lvl + 1, 2) act a
    hig <- peek (lvl + 1, 2) fom a
    fits FitNest (lvl + 1) low hig  -- bind check on this (outer) core
  (_, Sing' _ t) -> seal lvl a t
  (_, Molt' _ t) -> seal lvl a t
  (_, Face' _ t) -> seal lvl a t
  (_, Fuse' t _) -> seal lvl a t
  (_, Seal' _ t) -> seal lvl a t  -- FIXME check for axis cover; already sealed
  (_, Noun') -> pure ()
  (_, Void') -> pure ()  -- XX correct?
  (_, Type') -> pure ()

 where
  sealFail = bailFail

-- | Given an "explicitly typed" expression, produce its unevaluated type.
scan :: forall a m. (MonadCheck m, Var a)
     => Con a -> Soft -> m (Type a)
scan con@Con{lvl, sut} cod = act (ActScan con cod) case cod of
  Wng{} -> scanMurk

  Atm _ Sand au -> pure $ Aura' au Bowl
  Atm _ Rock _  -> scanMurk

  Cel c d -> Cell' <$> scan con c <*> scan con d

  Cru cs -> do
    -- FIXME this WRONGLY treats type annotations on arms that contain wings
    -- that refer to the current core. Unclear what to do there. Here Noun'
    -- stands in for the "battery type."
    ts <- for cs \c -> do
      t <- scan (melt $ hide con Noun') c
      -- t <- thin (lvl + 1) t
      pure (With (Spot 2) $ loft lvl t)
    let ken = read (rump (lvl, 1)) sut
    pure $ Core' (molt sut) (ken, ts) sut

  Fac p c -> face' (derm p) <$> scan con c

  Plu{} -> scanMurk
  Equ{} -> scanMurk
  Tes{} -> scanMurk
  Rhe{} -> scanMurk
  Fis{} -> scanMurk

  Aur{} -> pure Type'
  Ral{} -> pure Type'
  Gat{} -> pure Type'
  Cor{} -> pure Type'
  Sin{} -> pure Type'
  Mot{} -> pure Type'
  Fus{} -> pure Type'
  Sal{} -> pure Type'
  Non{} -> pure Type'
  Vod{} -> pure Type'
  Typ{} -> pure Type'

  -- XX can this be relaxed?
  Wit{} -> scanMurk

  Pus c d -> do
    (x, t, _) <- play con c
    scan (shew con (evil con x) t) d

  Mut c d e -> do
    fom <- evil con . fst <$> work con FitNest c Type'
    (x, act, _) <- play con d
    scan (moot con fom (evil con x) act) e

  Net{typ} -> evil con . fst <$> work con FitNest typ Type'
  Cat{typ} -> evil con . fst <$> work con FitNest typ Type'

  Hid{} -> scanMurk  -- FIXME ideally

 where
  scanMurk = bail (ScanMurk cod)

-- | Given subject type and knowledge, verify that code has result type.
-- Since the expected result type is known in this mode, we can lighten the
-- user's annotation burden. Read about "bidirectional type checking" to
-- learn more.
-- TODO ugh pass Weft through
work :: forall a m. (MonadCheck m, Var a)
     => Con a -> Fit -> Soft -> Type a -> m (Code Void, Set Fish)
work con fit cod gol = let
 -- On its inside, work needs to thread a Warp for *some* of its nest checks.
 work :: forall a m. (MonadCheck m, Var a)
      => Con a -> Fit -> Soft -> Type a -> Warp -> m (Code Void, Set Fish)
 work con@Con{lvl, sut} fit cod gol wap@Warp{lax, rax, pax} =
  let
    playFits = do
      (x, t', ns) <- play con cod
      fist fit lvl t' gol wap
      pure (x, ns)
  in act (ActWork con fit cod gol wap) case cod of
    Wng{} -> playFits

    -- for introduction forms except atoms, we push the type constraint inward
    -- this allows the user to type-annotate the whole of a big data structure
    -- indcluding cores and gates, without having to also annotate the insides
    -- unless they want to.
    Atm{} -> playFits

    Cel c d -> case gol of
      (held -> Just gol) -> work con fit cod gol wap
      Sing' s gol -> do
        (x, ms) <- work con fit cod gol wap
        fits FitSame lvl s (evil con x)
        pure (x, ms)
      Molt' _ gol -> work con fit cod gol wap
      Seal' a t -> work con fit cod t wap { rax = insertSet (pax / a) rax }
      Face' f t -> work con fit cod t wap
      Fuse' _ _ -> playFits  -- FIXME?
      --
      Cell' t u -> do
        (x, ms) <- work con fit c t (wap / 2)
        (y, ns) <- work con fit d u (wap / 3)
        pure (Cell x y, swam ms ns)
      Rail' t j -> do
        (x, ms) <- work con fit c t (wap / 2)
        let u = jamb j $ evil con x
        (y, ns) <- work con fit d u (wap / 3)
        pure (Cell x y, swam ms ns)
      Type' -> do
        -- We have to recurse in to ensure faces appear in the generated code.
        (x, ms) <- work con fit c Type' (wap / 2)
        (y, ns) <- work con fit d Type' (wap / 3)
        pure (Cell x y, swam ms ns)
      _ -> playFits

    {-
    Lam a c -> case gol of
      -- XX FIXME deal with other metadata like Fuse' (?)
      (held -> Just t) -> work con fit cod t
      Face' f gol -> work con fit cod gol
      Sing' s gol -> do
        (x, ms) <- work con fit cod gol
        fits FitSame lvl s (evil con x)
        pure (x, ms)
      Gate' golArgT (Jamb resCod resClo) -> do
        (argC, ms) <- work con FitNest a Type'
        let argT = evil con argC
        fits FitNest lvl golArgT argT
        tire (lvl, 1) ms sut
        let fom = Cell' argT sut
        -- This is hard to get right. :/ It will clean up when Gate' becomes
        -- epiphenominal.
        let ken = Cell' (read (rump (lvl + 1, 2)) argT) resClo
        -- resJ's cod expects a subject of shape [arg sut] but the resC we must
        -- put in the core type needs to be run against the awful [[arg sut] arg
        -- sut]. My sincere apologies.
        let resC = With (Cell (Spot 1) (Spot 1)) resCod
        let cod = Cru (mapFromList [("", c)])
        let gol = Core' fom (ken, mapFromList [("", resC)]) fom
        (x, ms) <- work Con{lvl = lvl + 1, sut = fom} fit cod gol
        pure (Push (Atom 0) x, ms)
        {-
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
        -}
      _ -> playFits
    -}

    Cru arms -> case gol of
      (held -> Just gol) -> work con fit cod gol wap
      Sing' s gol -> do
        (x, ms) <- work con fit cod gol wap
        fits FitSame lvl s (evil con x)
        pure (x, ms)
      Molt' _ gol -> work con fit cod gol wap
      Seal' a t -> work con fit cod t wap { rax = insertSet (pax / a) rax }
      Face' f t -> work con fit cod t wap
      Fuse' _ _ -> playFits  -- FIXME?
      --
      Core' fom (clo, bat) act -> do
        let mot = molt sut
        fist fit lvl sut act (wap / 3)
        -- XX this is here because we otherwise have no way to determine the
        -- mesh for the Crux. This limitation is in principle unnecessary,
        -- because we ought to be able to jointly traverse fom and act to find
        -- the axes at which they don't nest. That would free you to cast your
        -- formal type to whatever you want. But also arguably you shouldn't be
        -- allowed to do that with this construct anyway because |% doesn't
        -- innately provide that capability, e.g. in play. Let's see if people
        -- care.
        --
        -- Or said differently, when you do a |%, you get something that runs
        -- against the molted subject, whether worked or played. By casting, you
        -- can lose information, subsequently turning it into something that
        -- can accept fewer things, in the usual way that $-(@ X) < $-(* X).
        fist fit lvl fom (molt sut) (wap / 3)
        -- XX if we go back to separate seminoun this may be a problem.
        let ken = read (rump (lvl, 1)) mot
        -- The arms are checked against a core subject. The formal type is the
        -- molted original subject. The actual type is *also* the molted orig,
        -- reflecting that by the time we get to executing arm code, the actual
        -- contents will necessarily be of that type.
        let sut = Core' mot (clo, bat) mot
        xs <- farm arms bat mempty \nom arm armT xs -> do
          (x, ms) <- work
            Con{sut, lvl = lvl + 1}  -- TODO FIXME is this lvl right????
            fit
            arm
            (jamb (Jamb armT clo) ken)
            SHED
          tire (lvl + 1, 1) ms (Core' fom (clo, bat) fom)
          pure (insertMap nom x xs)
        pure (Crux (knit sut) xs, singleton Tuna)
      _ -> playFits

    Fac p c -> do
      -- XX think about whether we should instead play here, so that toil can
      -- operate against a more specific scrutinee type.
      (x, ns) <- work con fit c gol wap
      let fs = derm p
      asum
        [ fist FitNest lvl gol Type' wap >> pure (face fs x, ns)
        , pure (x, ns)
        ]

    -- elimination forms just use nest
    Plu{} -> playFits
    Equ{} -> playFits
    Fis{} -> playFits  -- not inside Tes

    -- This is the big whammy of work right now. The play implementation calls
    -- join (compute type union), which is not yet implemented. So you need to
    -- annotate the return types of your switches, but because of the way that
    -- work propagagtes goal types inwards, this annotation can be far away and
    -- is likely already present.
    Tes c d e -> do
      (x, tru, fal, fis) <- chip con c
      (y, met) <- work tru fit d gol wap
      (z, jon) <- work fal fit e gol wap
      pure (Test x y z, union (swam fis met) jon)

    -- "rhetorical" tests are required to evaluate to true at compile time.
    -- this will be a rigorous exercise of the crop/fuse system and will be
    -- our mechanism of exhaustiveness checking.
    -- XX update this comment in light of the new exhaustiveness checker
    Rhe c d -> do
      (x, tru, _, fis) <- chip con c
      (y, met) <- work tru fit d gol wap
      pure (y, swam fis met)

    --Run{} -> playFits

    -- likewise with types
    Aur{} -> playFits
    Ral{} -> playFits
    Gat{} -> playFits
    Cor{} -> playFits
    Sin{} -> playFits
    Mot{} -> playFits
    Fus{} -> playFits
    Sal{} -> playFits

    Non -> playFits
    Vod -> playFits
    Typ -> playFits

    Wit c d -> do
      (x, t, pre) <- play con c
      let con' = Con { lvl = 0, sut = grow $ Sing' (evil con x) t }
      (y, pos) <- work con' fit d (grow gol) wap
      -- XX investigate when this can be relaxed
      tire (lvl, 1) pos t
      pure (With x y, pre)

    Pus c d -> do
      -- The fishing here is remarkable. The newly introduced part of the
      -- subject is the only exhastiveness requirement. The rest passes through.
      (x, t, ns) <- play con c
      (y, ms) <- work (shew con (evil con x) t) fit d gol wap
      tire (lvl + 1, 2) (slip L ms) t
      pure (Push x y, swam ns (slip R ms))

    Mut c d e -> do
      (fom, ms) <- work con FitNest c Type' SHED
      (x, act, ns) <- play con d
      (y, os) <- work (moot con (evil con fom) (evil con x) act) fit e gol wap
      tire (lvl + 1, 2) (slip L os) act
      pure (Push x y, swam ms (swam ns (slip R os)))

    Net{} -> playFits
    Cat{} -> playFits
    Hid{} -> playFits

    --Sin c -> work con fit c gol
 in work con fit cod gol SHED

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

  {-
  Lam a c -> do
    (argC, ms) <- work con FitNest a Type'
    tire (lvl, 1) ms sut
    let argT = evil con argC
    (x, t, ns) <- play (hide con argT) c
    -- Note that we produce a "gold core type" here, as in 140. It'll nest under
    -- whatever $- you care to pass it as, so it's actually fine.
    pure (Push (Atom 0) x, t, ns)
    {-
    (argC, ms) <- work con FitNest a Type'
    let argT = evil con argC
    ( Crux (lookup "" -> Just x)
     , Sing' _ (Core' _ (Cell' _ ken, lookup "" -> Just resC') _)
     , ns
     ) <-
      play (hide con argT) (Cru $ mapFromList [("", c)])
    -- The problem with the original resC' is that it expects a value for the
    -- whole actual subject to be pushed, rather than just the +2. This
    -- misalignment will go away when gate types become epiphenominal, sorry.
    let resC = loft (lvl + 1) $ eval (Cell' ken ken) resC'
    -- You could actually be doing case analysis to determine arg type,
    -- and that needs to be total.
    tire (lvl, 1) ms sut
    -- Meanwhile we already do tire in the Cru case below for the body.
    pure (Lamb x, Gate' argT (Jamb resC ken), singleton Tuna)
    -}
  -}

  Cru arms -> do
    cor <- scan (melt con) cod  -- molt con to get fom in act; XX double molt
    let ken = read (rump (lvl, 1)) sut
    res <- for arms $ play Con { sut = cor, lvl = lvl + 1 }
    let cru = Crux (knit sut) (res <&> \(x, _, _) -> x)
    tys <- for res \(_, t, _) -> do
      -- t <- thin (lvl + 1) t
      pure (With (Spot 2) $ loft lvl t)
    let cor = Core' (molt sut) (ken, tys) sut   -- NOTE molt only in fom
    -- XX correct?
    for_ res \(_, _, ns) -> tire (lvl + 1, 1) ns cor
    pure
      ( cru
      , cor
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

  {-
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
    -}

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
    ress <- for bat \arm -> work (hide con (evil con y)) FitNest arm Type'
    let xs  = fmap fst ress
    let mss = fmap snd ress
    pure (Core y xs x, Type', foldl' swam (swam ms ns) mss)

  Sin sof typ -> do
    (y, ns) <- work con FitNest typ Type'
    (x, ms) <- work con FitNest sof $ evil con y
    pure (Sing x y, Type', swam ns ms)

  Mot fom act -> do
    (x, ms) <- work con FitNest fom Type'
    (y, ns) <- work con FitNest act Type'
    pure (Molt x y, Type', swam ms ns)

  Fus typ pet -> do
    (x, ns) <- work con FitNest typ Type'
    let fis = fish pet
    let fac = derm pet
    -- XX do we want something like constant folding in the fst below?
    pure (Face (Link $ clop fac) $ Fuse x fis, Type', ns)

  Sal win typ -> do
    (x, ms) <- work con FitNest typ Type'
    find (lvl, 1) (evil con x) win >>= \case
      (Arm{}, _) -> bail (SealPull win)
      (Leg a, _) -> pure (Seal a x, Type', ms)

  Non -> pure (Noun, Type', singleton Tuna)

  Vod -> pure (Void, Type', singleton Tuna)

  Typ -> pure (Type, Type', singleton Tuna)

  Wit c d -> do
    (x, t, pre) <- play con c
    let con' = Con { lvl = 0, sut = grow $ Sing' (evil con x) t }
    (y, u, pos) <- play con' d
    ret <- pare u
    -- XX investigate when this can be relaxed. seems like regression for eg =.
    tire (lvl, 1) pos t
    pure (With x y, ret, pre)

  Pus c d -> do
    (x, t, ms) <- play con c
    (y, u, ns) <- play (shew con (evil con x) t) d
    tire (lvl + 1, 2) (slip L ns) t
    pure (Push x y, u, swam ms (slip R ns))

  Mut c d e -> do
    (fom, ms) <- work con FitNest c Type'
    (x, act, ns) <- play con d
    (y, t, os) <- play (moot con (evil con fom) (evil con x) act) e
    tire (lvl + 1, 2) (slip L os) act
    pure (Push x y, t, swam ms (swam ns (slip R os)))

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

  Hid win sof -> find (lvl, 1) sut win >>= \case
    (Arm{}, _) -> bail (SealPull win)
    (Leg a, _) -> do
      (x, t, ms) <- play con sof
      seal lvl a t
      pure (x, Seal' a t, ms)

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
  -- Lamb c -> Lam Non (rest c)
  Crux _ cs -> Cru (fmap rest cs)
  --
  Pull ar _ c -> case rest c of
    Wng w eds -> Wng (Ally ar : w) eds
    sof -> Wit sof $ Wng [Ally ar] []
  Plus c -> Plu (rest c)
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
  Molt c d -> Mot (rest c) (rest d)
  Face (Mask m) c -> Fac (Peer m) (rest c)
  Face (Link ls) c -> Fac Punt (rest c)  -- FIXME ?
  Fuse c h -> Fus (rest c) (pond h)
  Seal a c -> Sal [Axis a] (rest c)
  Noun -> Non
  Void -> Vod
  Type -> Typ
  With c d -> Wit (rest c) (rest d)
  Push c d -> Pus (rest c) (rest d)

-- | Use a subject type to read back wing information in a much less shitty way.
-- doze :: Var a => Type a -> Code Stub -> Soft
-- doze typ = undefined
