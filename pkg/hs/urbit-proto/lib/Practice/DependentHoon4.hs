module Practice.DependentHoon4 where

import ClassyPrelude hiding ((/), even, find, join, read)

import Control.Monad.Except hiding (join)
import Control.Monad.Reader hiding (join)
import Control.Monad.State hiding (join)
import Data.Set (isSubsetOf, toAscList)
import qualified Data.Set as Set
import Data.Void

import Practice.HoonCommon hiding (Bass(..))

data Soft
  = Wng [Axis]
  | Atm Atom Aura
  | Cel Aura Aura
  | Lam Pelt Aura
  --
  | Plu Soft
  | Sla Soft
  --
  | Ara Aura
  | Ral Soft Soft
  | Gat Soft Soft
  | Fok [Soft] Soft
  | Non
  | Vod
  | Typ
  --
  | Wit Soft Soft
  | Pus Soft Soft
  | Sin Soft
  deriving (Eq, Ord, Show, Generic)

data Pelt
  = Punt            -- ^ _     wildcard
  -- | Peer Term       -- ^ a     variable
  | Part Atom       -- ^ %foo  constant
  | Pair Pelt Pelt  -- ^ []    cons
  -- | Pons Pelt Pelt  -- ^ a=    as-pattern
  -- | Pest Pelt Soft  -- ^ /   patern nest
  -- | Past (Pelt a) (Code a)  -- ^ ``  pattern cast
  deriving (Eq, Ord, Show, Generic)

data Code a
  = Stub Stub
  | Fore a
  -- | Alts (Set (Code a))
  --
  | Atom Atom
  | Cell (Code a) (Code a)
  | Lamb (Code a)
  --
  | Plus (Code a)
  | Slam (Code a) (Code a)
  --
  | Aura Aura
  | Rail (Code a) (Code a)
  | Gate (Code a) (Code a)
  | Fork (Set (Code a)) (Code a)
  | Noun
  | Void
  | Type
  --
  | With (Code a) (Code a)
  | Push (Code a) (Code a)
  deriving (Generic)

deriving instance Eq   a => Eq   (Code a)
deriving instance Ord  a => Ord  (Code a)
deriving instance Show a => Show (Code a)

data Stub
  = Leg Axis
  deriving (Eq, Ord)

instance Show Stub where
  show = \case
    Leg a -> "+" <> show a

pole :: Axis -> Stub -> Stub
pole a = \case
  Leg b -> Leg (peg a b)

type Level = Nat

type Loc = (Level, Axis)

-- | Frozen wing.
data Rump
  = Leg' Loc

instance Show Rump where
  show = \case
    Leg' (l, a) -> "+" <> show l <> "_" <> show a

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
  | l < m     = compare (peg (2 ^ (m - l)) a) b
  | l > m     = compare a (peg (2 ^ (l - m)) b)
  | otherwise = compare a b

instance Eq Rump where
  Leg' la == Leg' lb = comp la lb == EQ

instance Ord Rump where
  compare (Leg' la) (Leg' lb) = comp la lb

type Type = Semi

-- | Closure
data Jamb a = Jamb { cod :: Code a, clo :: Semi a }

deriving instance Eq   a => Eq   (Jamb a)
deriving instance Ord  a => Ord  (Jamb a)
deriving instance Show a => Show (Jamb a)

data Semi a
  = Rump' Rump
  | Fore' a
  | Alts' (Set (Semi a)) (Semi a)
  --
  | Atom' Atom
  | Cell' (Semi a) (Semi a)
  | Lamb' (Jamb a)
  --
  | Plus' (Semi a)
  | Slam' (Semi a) (Semi a)
  | Look' (Semi a) Stub
  --
  | Aura' Aura
  | Rail' (Semi a) (Jamb a)
  | Gate' (Semi a) (Jamb a)
  | Fork' (Set (Semi a)) (Semi a)
  | Noun'
  | Void'
  | Type'
  deriving (Generic)

deriving instance Eq   a => Eq   (Semi a)
deriving instance Ord  a => Ord  (Semi a)
deriving instance Show a => Show (Semi a)

class SetFunctor f where
  smap :: (Ord a, Ord b) => (a -> b) -> f a -> f b

instance SetFunctor Set where
  smap f = Set.fromList . map f . Set.toList

(>>==) :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
ss >>== f = mconcat $ map f $ setToList ss

elems :: (Ord a, Ord b) => ([a] -> [b]) -> Set a -> Set b
elems f = setFromList . f . setToList

{-
-- | Apply a function to every alt, flattening any Alts in the result.
alts' :: Ord a => Set (Semi a) -> (Semi a -> Semi a) -> Semi a
alts' ss f = Alts' $ elems (concatMap (split . f)) ss
 where
  split :: Ord a => Semi a -> [Semi a]
  split = \case
    Alts' ss -> setToList ss
    x -> [x]
-}

instance SetFunctor Code where
  smap = undefined

instance SetFunctor Semi where
  smap = undefined

instance SetFunctor Jamb where
  smap f (Jamb a b) = Jamb (smap f a) (smap f b)

class Peg a where
  (/) :: a -> Axis -> a

instance Peg Axis where
  a / b = peg a b

instance Peg Stub where
  Leg a / b = Leg (a / b)

instance Peg (Level, Axis) where
  (l, a) / b = (l, a / b)

instance Peg Rump where
  Leg' la / b = Leg' (la / b)

instance Ord a => Peg (Semi a) where
  s / a = look (Leg a) s

-- instance SetFunctor f => Functor f where
--   fmap = smap


-- Evaluator -------------------------------------------------------------------

-- | Read a seminoun representing what we know about the *values* of the subject
-- from the type of the subject. Where we don't know anything, the given
-- seminoun is used to generate unknowns.
read :: Ord a => Semi a -> Type a -> Semi a
read nul = \case
  Alts' ss s -> Alts' (smap (read nul) ss) (read nul s)
  Fork' ss t -> Alts' ss $ read nul t
  Cell' t u -> Cell' (read (nul / 2) t) (read (nul / 3) u)
  Rail' t j -> Cell' lef rit
   where
    lef = read (nul / 2) t
    rit = read (nul / 3) $ jamb j lef
  _ -> nul

jamb :: Ord a => Jamb a -> Semi a -> Semi a
jamb Jamb{..} arg = eval (Cell' arg clo) cod

-- | Axially project a value; i.e. implement Nock 0 or 9.
look :: Ord a => Stub -> Semi a -> Semi a
look s b = home s $ walk a b
 where
  a = case s of
    Leg a -> a

  walk a b = case (cut a, b) of
    (Nothing,     c)                   -> c
    (_,           Alts' ss s)          -> Alts' (smap (walk a) ss) (walk a s)
    (_,           Rump' (Leg' (l, x))) -> Rump' $ Leg' (l, peg x a)
    (_,           Look' c (Leg i))     -> Look' c $ Leg (peg i a)
--  (_,           Face' _ c)           -> walk a c
    (Just (L, b), Cell' c _)           -> walk b c
    (Just (R, b), Cell' _ c)           -> walk b c
    (Just _,      _)                   -> Look' b s

  home s b = case s of
    Leg _ -> b

eval :: Ord a => Semi a -> Code a -> Semi a
eval sub = \case
  Stub s -> look s sub
  Fore x -> Fore' x
  -- Alts ss -> Alts' $ smap (eval sub) ss
  --
  Atom a -> Atom' a
  Cell c d -> Cell' (eval sub c) (eval sub d)
  Lamb a -> Lamb' (Jamb a sub)
  --
  Plus c -> go (eval sub c)
   where
    go = \case
      Alts' ss s -> Alts' (smap go ss) (go s)
      Atom' a -> Atom' (a + 1)
      x -> Plus' x
  -- FYI, this is the only source of exponentiality I am aware of.
  -- Maybe don't make forks of functions, dawg.
  Slam c d -> go (eval sub c)
   where
    y = eval sub d
    go = \case
      -- we could also lose the alts here to get rid of the explosion
      Alts' ss s -> Alts' (smap go ss) (go s)
      Lamb' j -> jamb j $ eval sub d
      x -> Slam' x y
  --
  Aura au -> Aura' au
  Rail c d -> Gate' (eval sub c) (Jamb d sub)
  Gate c d -> Gate' (eval sub c) (Jamb d sub)
  Noun -> Noun'
  Void -> Void'
  Type -> Type'
  With c d -> eval (eval sub c) d
  Push c d -> eval (Cell' (eval sub c) sub) d

loft :: Ord a => Level -> Semi a -> Code a
loft lvl = \case
  -- XX we should have some printout here if lvl < l, which is a serious
  -- invariant violation that should be more legible
  Rump' (Leg' (l, a)) -> Stub (Leg $ peg (2 ^ (lvl - l)) a)
  Fore' x -> Fore x
  Alts' ss s -> loft lvl s -- Alts $ smap (loft lvl) ss
  --
  Atom' a -> Atom a
  Cell' a b -> Cell (loft lvl a) (loft lvl b)
  Lamb' j -> Lamb $ luft lvl j
  --
  Plus' a -> Plus (loft lvl a)
  Slam' a b -> Slam (loft lvl a) (loft lvl b)
  Look' a s -> With (loft lvl a) $ Stub s
  --
  Aura' au -> Aura au
  Rail' l j -> Rail (loft lvl l) (luft lvl j)
  Gate' a j -> Gate (loft lvl a) (luft lvl j)
  Fork' ss t -> Fork (smap (loft lvl) ss) (loft lvl t)
  Noun' -> Noun
  Void' -> Void
  Type' -> Type
 where
  luft l Jamb{..} =
    loft (l + 1) $ eval (Cell' (Rump' (Leg' (l + 1, 2))) clo) cod

{-
evil :: Ord a => Semi a -> Code Void -> Semi a
evil ken = eval ken . vacuous -}


-- The type checking monad -----------------------------------------------------

type Var a = (Eq a, Ord a, Show a)

class (Monad m, Alternative m) => MonadCheck m where
  -- | Push an error reporting stack frame.
  act :: Act -> m a -> m a

  -- | Report an error.
  bail :: Fail -> m a

  -- | Change the error message of the error, if any, that occurs within the
  -- computation.
  bailSwap :: (Fail -> Fail) -> m a -> m a

  -- | Leave a message to be embedded in the trace. Has no effect in non-tracing
  -- modes.
  note :: Note -> m ()

-- | Fail with no message.
bailFail :: MonadCheck m => m a
bailFail = bail BailFail

-- | Error reporting context, analogous to stack trace item. As the compiler
-- recurses deeper into its operations, it pushes these descriptions to a stack
-- so they can serve as breadcrumbs in error messages.
data Act
  =                    ActRoot
  |                    ActDone

-- | Compiler errors.
data Fail
  -- | The two types do not {nest, cast, equal each other}.
  = forall a. Var a => FitsFail Fit (Type a) (Type a)
  | BailNote Text  -- ^ failure with note
  | BailFail  -- ^ unspecified failure

-- | Log items. Appear only in trace mode.
data Note
  = forall a. Var a => NoteType Text (Type a)
  | forall a. Var a => NoteSemi Text (Semi a)
  | forall a. Var a => NoteCode Text (Code a)

instance Semigroup Fail where
  _ <> f = f  -- report last failure in fallback list

instance Monoid Fail where
  mempty = BailNote "mempty"

deriving instance (Show Act)
deriving instance (Show Fail)
deriving instance (Show Note)

-- | Basic type checking monad.
newtype Check a = Check { unCheck :: ReaderT [Act] (Either ([Act], Fail)) a }
  deriving newtype (Functor, Applicative, Monad)

-- | Run the computation in basic type checking mode
runCheck :: Check a -> Either ([Act], Fail) a
runCheck chk = runReaderT (unCheck chk) []

instance Alternative Check where
  empty = bailFail
  Check (ReaderT c) <|> Check (ReaderT d) = Check $ ReaderT \r -> c r <> d r

instance MonadCheck Check where
  act a chk = Check $ local (a:) (unCheck chk)
  bail f = Check $ ask >>= \as -> lift $ Left (as, f)
  bailSwap f chk = Check $ ReaderT \r -> case runReaderT (unCheck chk) r of
    Left (acts, err) -> Left (acts, f err)
    Right x -> Right x
  note _ = pure ()

data ActTree
  = ActTree Act [ActTree]  -- ^ most recent act at front
  | ActNote Note
  deriving Show

type Trace a = ExceptT Fail (State [ActTree]) a

runTrace :: Trace a -> (ActTree, Either Fail a)
runTrace tac = (tree zipper, res)
 where
  (res, zipper) = runState (runExceptT tac) [ActTree ActRoot []]

  tree zz = foldl' insertTree (ActTree ActDone []) zz

insertTree :: ActTree -> ActTree -> ActTree
insertTree inner _outer@(ActTree a cs) = ActTree a (inner : cs)
insertTree _ ActNote{} = error "I can't be bothered to write safe printf code"

instance MonadCheck (ExceptT Fail (State [ActTree])) where
  act a m = do
    modify' (ActTree a [] :)
    res <- m
    modify' \(inner:outer:rest) -> insertTree inner outer : rest
    pure res
  bail = throwError
  bailSwap f m = catchError m (\e -> throwError (f e))
  note n = modify' \(outer:rest) -> insertTree (ActNote n) outer : rest

traceToStack :: ActTree -> [Act]
traceToStack = reverse . go
 where
  go = \case
    ActNote _ -> []
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


--------------------------------------------------------------------------------
-- Core operations of the compiler ---------------------------------------------
--------------------------------------------------------------------------------

-- The calculus of types -------------------------------------------------------

-- | Try to calculate union of types
join :: (MonadCheck m, Var a) => Type a -> Type a -> m (Type a)
join _ _ = bail $ BailNote "join: Not implemented. Please put a ^- on your ?:"

-- | Try to calculate intersection of types
meet :: (MonadCheck m, Var a) => Type a -> Type a -> m (Type a)
meet = undefined

-- | Perform subtyping, coercibility, or equality check.
-- XX figure out proper encoding of recursion via cores or gates
-- XX figure out how seminouns should apply here, if at all
fits :: forall a m. (MonadCheck m, Var a)
     => Fit -> Type a -> Type a -> m ()
fits fit t u = case (t, u) of
  -- XX faces go here

  (Noun', Noun') -> pure ()
  (Noun', _) -> fitsFail
  (_, Noun') -> case fit of
    FitSame -> fitsFail
    FitNest -> pure ()
    FitCast -> pure ()

  (Void', Void') -> pure ()
  (Void', _) -> case fit of
    FitSame -> fitsFail
    FitNest -> pure ()
    FitCast -> pure ()
  (_, Void') -> fitsFail

  (Rump' r, Rump' s)
    | r == s    -> pure ()
    | otherwise -> fitsFail
  (Rump'{}, _) -> fitsFail
  (_, Rump'{}) -> fitsFail

  (Fore' r, Fore' s)
    | r == s    -> pure ()
    | otherwise -> fitsFail
  (Fore'{}, _) -> fitsFail
  (_, Fore'{}) -> fitsFail

  (Alts' ts t, _) -> fits fit t u <|> for_ ts \t -> fits fit t u
  (_, Alts' us u) -> fits fit t u <|> for_ us \u -> fits fit t u

  --

  (Atom' a, Atom' b) | a == b -> pure ()
  (Atom'{}, _) -> fitsFail
  (_, Atom'{}) -> fitsFail

  (Cell' v w, Cell' v' w') -> do fits fit v v'; fits fit w w'

  -- Evaluate the function bodies against a fresh opaque symbol. To get a fresh
  -- symbol, we have a bunch of options:
  --   - Track level as an argument to fits, as Kovacs does, incrementing under
  --     binders. We can then use (lvl + 1, 3) as the new Rump. Downside: not
  --     clear how to get this value when comparing two RTTIs at runtime.
  --     Although, in fact, rtts wil NEVER have rumps, so...
  --   - Possibly, store a level in each saved, closed over, subject, taking the
  --     larger of the two. Think hard about whether this actually works.
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
  (Lamb' j, Lamb' k) -> fits fit (jamb (smap Old j) new) (jamb (smap Old k) new)
    where new = Fore' (New ())

  -- XX should we read and do comb explosion?

  (Lamb' j, _) -> fits fit (jamb (smap Old j) new) (Slam' (smap Old u) new)
    where new = Fore' (New ())

  (_, Lamb' k) -> fits fit (Slam' (smap Old t) new) (jamb (smap Old k) new)
    where new = Fore' (New ())


  (Plus' v, Plus' w) -> fits FitSame v w
  (Plus'{}, _) -> fitsFail
  (_, Plus'{}) -> fitsFail

  -- Since it hasn't been evaluated away, we are dealing with an opaque type
  -- function application. This means we have no choice but to regard the
  -- function as invariant in its argument.
  (Slam' v w, Slam' v' w') -> do fits fit v v'; fits FitSame w w'
  (Slam'{}, _) -> fitsFail
  (_, Slam'{}) -> fitsFail

  -- The assumption is that these are fully evaluated. This rules out Looks
  -- stacked on top of Looks, as well of Looks on top of cells. Accordingly the
  -- rules are pretty tight. I don't think there's any equiv of the beta-eta
  -- conversion we saw above with functions here.
  (Look' b st, Look' c ub)
    | st == ub  -> fits fit b c
    | otherwise -> fitsFail
  (Look'{}, _) -> fitsFail
  (_, Look'{}) -> fitsFail

  (Aura' au, Aura' ag) -> case fit of
    FitCast -> pure ()
    FitNest -> if ag `isPrefixOf` au then pure () else fitsFail
    FitSame -> if ag ==           au then pure () else fitsFail

  (Fork' cs t, Fork' ds u) -> do
    fits fit t u
    case fit of
      FitSame -> when (cs /= ds) fitsFail
      FitNest -> unless (cs `isSubsetOf` ds) fitsFail
      FitCast -> unless (cs `isSubsetOf` ds) fitsFail

  (Fork' ss t, _) | fit /= FitSame -> fits fit t u

  (Rail' v j, Rail' w k) -> do
    fits fit v w
    fits fit (jamb j' $ read new v') (jamb k' $ read new w')
   where
    v' = smap Old v
    w' = smap Old w
    j' = smap Old j
    k' = smap Old k
    new = Fore' (New ())

  (Gate' v j, Gate' w k) -> do
    fits fit v w
    fits fit (jamb j' $ read new v') (jamb k' $ read new w')
   where
    v' = smap Old v
    w' = smap Old w
    j' = smap Old j
    k' = smap Old k
    new = Fore' (New ())

  (Type', Type') -> pure ()

  (Aura'{}, _) -> fitsFail
  (_, Aura'{}) -> fitsFail
  (Fork'{}, _) -> fitsFail
  (_, Fork'{}) -> fitsFail
  (Cell'{}, _) -> fitsFail
  (_, Cell'{}) -> fitsFail
  (Gate'{}, _) -> fitsFail
  (_, Gate'{}) -> fitsFail

 where
  fitsFail = bail (FitsFail fit t u)




-- Find ------------------------------------------------------------------------


peek :: (MonadCheck m, Var a) => Loc -> Type a -> Axis -> m (Type a)
peek loc a typ = undefined

-- Pelt system -----------------------------------------------------------------

repo :: Type a -> Type a
repo = id  -- XX faces

-- | Intersect seminouns.
meld :: (Var a) => Semi a -> Semi a -> Maybe (Semi a)
meld b c = case (b, c) of
  (Rump'{}, a)       -> pure a
  (a,       Rump'{}) -> pure a

  (a, Alts' ss s) ->
    let ss' = elems (catMaybes . map (a `meld`)) ss
    in if Set.empty == ss'
      then Nothing
      else Alts' ss' <$> meld a s

  (Alts' ss s, a) ->
    let ss' = elems (catMaybes . map (`meld` a)) ss
    in if Set.empty == ss'
      then Nothing
      else Alts' ss' <$> meld s a

  (Cell' b c, Cell' b' c') -> Cell' <$> meld b b' <*> meld c c'

  (Atom' a, Atom' b) | a == b -> pure $ Atom' a

  _ -> Nothing

-- | Promote voids to top level.
vain :: Var a => Type a -> Type a
vain = \case
  Fork' ss _ | null ss -> Void'
  Cell' t u -> case (vain t, vain u) of
    (Void', _) -> Void'
    (_, Void') -> Void'
    (t, u) -> Cell' t u
  Rail' t j -> Rail' t j  -- XX FIXME
  t -> t

-- | Add an atomic constant to the collection of possibilities of a given type.
fork :: Var a => Atom -> Type a -> Type a
fork a t = case t of
  Fork' as t -> Fork' (insertSet (Atom' a) as) t
  t -> Fork' (singleton $ Atom' a) t

-- | Subtract the possibility of an atomic constant from a type.
kill :: Var a => Atom -> Type a -> Type a
kill a t = case t of
  Fork' as t -> case deleteSet (Atom' a) as of
    as | null as   -> Void'
       | otherwise -> Fork' as t
  t -> t


fuse :: forall a m. (MonadCheck m, Var a)
     => Loc -> Type a -> Pelt -> m (Type a)
fuse loc typ pet = case pet of
  Punt -> pure typ
  Part a -> pure $ fork a typ
  Pair p q -> do
    t <- peek loc typ 2
    u <- peek loc typ 3
    t <- fuse loc t p
    u <- fuse loc u q
    pure (Cell' t u)

crop :: forall a m. (MonadCheck m, Var a)
     => Loc -> Type a -> Pelt -> m (Type a)
crop loc@(lvl, axe) typ pet = case pet of
  Punt -> pure Void'
  Part a -> pure $ kill a typ
  Pair p q -> do
    t <- peek loc typ 2
    u <- peek loc typ 3
    tSmol <- crop loc t p
    crop loc u q >>= \case
      Void' -> pure (Cell' tSmol u)
      u     -> pure (Cell' t     tes)
       where
        fis = undefined -- Fish' (fish p) tSmol t
        tes = undefined -- Test' fis tSmol t


