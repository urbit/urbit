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
  | Atm Atom Grit Aura
  | Cel Soft Soft
  | Lam Pelt Soft
  --
  | Plu Soft
  | Sla Soft Soft
  | Equ Soft Soft
  | Tes Soft Soft Soft
  | Rhe Soft Soft
  | Fis Pelt Soft
  --
  | Aur Aura
  | Ral Soft Soft
  | Gat Soft Soft
  | Fok (Maybe Soft) [Soft]
  | Non
  | Vod
  | Typ
  --
  | Wit Soft Soft
  | Pus Soft Soft
  | Net { sof :: Soft, typ :: Soft }
  | Cat { sof :: Soft, typ :: Soft }
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

-- TODO "Duet" for $@

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
  | Equl (Code a) (Code a)
  | Test (Code a) (Code a) (Code a)
  | Fish Fish (Code a)
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

-- | Computational content of a refutable pattern. All you need to know to
-- check for match at nock/eval time.
data Fish
  = Tuna            -- ^ definite match
  | Sole Atom       -- ^ equality match
  | Char Fish Fish  -- ^ cellular match
  deriving (Eq, Ord, Show, Generic)

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
  | ALTS' (Set (Semi a)) (Semi a)
  --
  | Atom' Atom
  | Cell' (Semi a) (Semi a)
  | Lamb' (Jamb a)
  --
  | Plus' (Semi a)
  | Slam' (Semi a) (Semi a)
  | Equl' (Semi a) (Semi a)
  | Test' (Semi a) (Semi a) (Semi a)
  | Fish' Fish (Semi a)
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

{-# COMPLETE Rump', Fore', Alts', Atom', Cell', Lamb', Plus', Slam', Equl',
  Test', Fish', Look', Aura', Rail', Gate', Fork', Noun', Void', Type' #-}
pattern Alts' :: Ord a => Set (Semi a) -> Semi a -> Semi a
pattern Alts' ss s <- ALTS' ss s where
  Alts' ss s = case setToList ss of
    [s] -> s
    _ -> ALTS' ss s

-- | Shorthand for the boolean type, which is commonly used.
pattern Flag' :: Ord a => Semi a
pattern Flag' <- Fork' (toAscList -> [Atom' 0, Atom' 1]) (Aura' "f") where
  Flag' = Fork' (setFromList [Atom' 0, Atom' 1]) (Aura' "f")

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

instance Peg Void where
  _ / _ = error "impossible"

instance Peg Axis where
  a / b = peg a b

instance Peg Stub where
  Leg a / b = Leg (a / b)

instance Peg (Level, Axis) where
  (l, a) / b = (l, a / b)

instance Peg Rump where
  Leg' la / b = Leg' (la / b)

instance (Peg a, Peg b) => Peg (Hop b a) where
  Old x / a = Old $ x / a
  New x / a = New $ x / a

instance (Ord a, Peg a) => Peg (Semi a) where
  s / a = case (cut a, s) of
    (Nothing,     s)                   -> s
    (_,           Alts' ss s)          -> Alts' (smap (/ a) ss) (s / a)
    (_,           Rump' r)             -> Rump' $ r / a
    (_,           Fore' x)             -> Fore' $ x / a
    (_,           Look' c st)          -> Look' c $ st / a
--  (_,           Face' _ c)           -> walk a c
    (Just (L, a), Cell' s _)           -> s / a
    (Just (R, a), Cell' _ s)           -> s / a
    (Just _,      _)                   -> Look' s (Leg a)

-- instance Functor f => SetFunctor f where
--   fmap = smap

type Var a = (Eq a, Ord a, Show a, Peg a)


-- Evaluator -------------------------------------------------------------------

-- | Read a seminoun representing what we know about the *values* of the subject
-- from the type of the subject. Where we don't know anything, the given
-- seminoun is used to generate unknowns.
read :: Var a => Semi a -> Type a -> Semi a
read nul = \case
  Alts' ss s -> Alts' (smap (read nul) ss) (read nul s)
  Fork' ss t -> Alts' ss $ read nul t
  Test' b t f -> Test' b (read nul t) (read nul f)   -- XX correct?
  Cell' t u -> Cell' (read (nul / 2) t) (read (nul / 3) u)
  Rail' t j -> Cell' lef rit
   where
    lef = read (nul / 2) t
    rit = read (nul / 3) $ jamb j lef
  _ -> nul

jamb :: Var a => Jamb a -> Semi a -> Semi a
jamb Jamb{..} arg = eval (Cell' arg clo) cod

-- | Axially project a value; i.e. implement Nock 0 or 9.
look :: Var a => Stub -> Semi a -> Semi a
look st b = home $ b / a
 where
  a = case st of
    Leg a -> a

  home :: Semi a -> Semi a
  home b = case st of
    Leg _ -> b

eval :: Var a => Semi a -> Code a -> Semi a
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
  -- The logic in the Alts' smart constructor makes this legit, I hope.
  Equl c d -> case (eval sub c, eval sub d) of
    (x, y) | x == y    -> Atom' 0
           | otherwise -> Equl' x y
  Test c d e -> go (eval sub c)
   where
    go = \case
      Atom' 0 -> eval sub d
      Atom' 1 -> eval sub e
      x      -> Test' x (eval sub d) (eval sub e)  -- Laziness!
  Fish f c -> fish f (eval sub c)
  --
  Aura au -> Aura' au
  Rail c d -> Gate' (eval sub c) (Jamb d sub)
  Gate c d -> Gate' (eval sub c) (Jamb d sub)
  Fork cs c -> Fork' (smap (eval sub) cs) (eval sub c)
  Noun -> Noun'
  Void -> Void'
  Type -> Type'
  With c d -> eval (eval sub c) d
  Push c d -> eval (Cell' (eval sub c) sub) d
 where
  fish :: Var a => Fish -> Semi a -> Semi a
  fish h b = case out h b of
    Just True  -> Atom' 0
    Just False -> Atom' 1
    Nothing    -> Fish' h b
   where
    out h b = case (h, b) of
      (Tuna, _) -> Just True
      (_, Alts' bs b) -> case map (out h) $ setToList bs of
        rs | all (== Just True)  rs -> Just True
           | all (== Just False) rs -> Just False
           | otherwise              -> out h b
      (Sole a, Atom' b)
        | a == b    -> Just True
        | otherwise -> Just False
      (Sole a, Cell'{}) -> Just False
      (Sole{}, _) -> Nothing
      (Char{}, Atom'{}) -> Just False
      (Char h j, Cell' b c) -> (&&) <$> out h b <*> out j c
      -- I guess I could also let you see that gates are cells, but I don't want
      -- to right now?
      (Char{}, _) -> Nothing

loft :: Var a => Level -> Semi a -> Code a
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


evil :: Ord a => Type a -> Code Void -> Semi a
evil ken = undefined

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

-- To make the below, Tuna must be annotated with all possibilities, or we have
-- an "or" pattern and forbid Tuna, using type checker to enumerate, but also
-- somehow undoing this in code generation? Do we need yet another type?
-- We could parametrize tuna, use Fish (Set (Fish Void)) here and Fish () there?
-- In the presence of a type one can convert from the latter to the former?
-- Want also to avoid explosions but I think this can be solved by nesting tunas

-- | Two fish can swim together
herd :: Fish -> Fish -> Fish
herd = undefined

-- | Fish can be truant
fear :: Fish -> Fish -> [Fish]
fear = undefined


-- The type checking monad -----------------------------------------------------

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
  | forall a. Var a => ActWork (Con a) Fit Soft (Type a)
  | forall a. Var a => ActPlay (Con a) Soft
  |                    ActDone

-- | Compiler errors.
data Fail
  -- | The two types do not {nest, cast, equal each other}.
  = forall a. Var a => FitsFail Fit (Type a) (Type a)
  -- | A rhetorical question had a non-rhetorical answer.
  | forall a. Var a => WorkMiss Soft (Semi a)
  -- | A rhetorical question had a non-rhetorical answer.
  | forall a. Var a => PlayMiss Soft (Semi a)
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

hide :: Con a -> Type a -> Con a
hide Con{lvl, sut} typ = Con
  { lvl = lvl + 1
  , sut = Cell' typ sut
  }

shew :: Var a => Con a -> Semi a -> Type a -> Con a
shew Con{lvl, sut} ken typ = Con
  { lvl = lvl + 1
  , sut = Cell' (Fork' (singleton ken) typ) sut
  }
{-
-- | Grow the type because we have passed under a tisgar
grow :: Type a -> Type (Hop Rump a)
grow = \case
  Rump' r -> Fore' (New r)
  Fore' x -> Fore' (Old x)
  --
  Atom' a -> Atom' a
  Cell' x y -> Cell' (grow x) (grow y)
  Lamb' x c -> Lamb' (grow x) (crow c)
  --
  Plus' x -> Plus' (grow x)
  Slam' x y -> Slam' (grow x) (grow y)
  Equl' x y -> Equl' (grow x) (grow y)
  Test' x y z -> Test' (grow x) (grow y) (grow z)
  Fish' f x -> Fish' f (grow x)
  Look' x st -> Look' (grow x) st
  --
  Aura' au -> Aura' au
  Fork' as au -> Fork' as au
  Rail' x y c -> Rail' (grow x) (grow y) (crow c)
  Gate' x y c -> Gate' (grow x) (grow y) (crow c)
  --Face' fs x -> Face' fs (grow x)
  Noun' -> Noun'
  Void' -> Void'
  Type' -> Type'

 where
  crow :: Code a -> Code (Hop Rump a)
  crow = \case
    -- lookups into the closure are NOT changed; this is okay because the stuck
    -- seminoun references in the value in the clousre ARE.
    Stub st -> Stub st
    Fore x -> Fore (Old x)
    --
    Atom a -> Atom a
    Cell c d -> Cell (crow c) (crow d)
    Lamb c -> Lamb (crow c)
    --
    Plus c -> Plus (crow c)
    Slam c d -> Slam (crow c) (crow d)
    Equl x y -> Equl (crow x) (crow y)
    Test x y z -> Test (crow x) (crow y) (crow z)
    Fish f x -> Fish f (crow x)
    --
    Aura au -> Aura au
    Fork as au -> Fork as au
    Rail c d -> Rail (crow c) (crow d)
    Gate c d -> Gate (crow c) (crow d)
    --Face fs c -> Face fs (crow c)
    Noun -> Noun
    Void -> Void
    Type -> Type
    --
    With c d -> With (crow c) (crow d)
    Push c d -> Push (crow c) (crow d)

-- | On exiting a tisgar, pare down the type to remove any opaque references to
-- the inner subject, but actually it's an invariant violation for any to exist.
pare :: (MonadCheck m, Var a) => Semi (Hop Rump a) -> m (Semi a)
pare bas = go bas
 where
  go = \case
    Rump' r -> bail (PareFree r bas)
    Fore' (New r) -> pure $ Rump' r
    Fore' (Old x) -> pure $ Fore' x
    --
    Atom' a -> pure $ Atom' a
    Cell' x y -> Cell' <$> go x <*> go y
    Lamb' x c -> Lamb' <$> go x <*> care c
    --
    Plus' x -> Plus' <$> go x
    Slam' x y -> Slam' <$> go x <*> go y
    Equl' x y -> Equl' <$> go x <*> go y
    Test' x y z -> Test' <$> go x <*> go y <*> go z
    Fish' f x -> Fish' f <$> go x
    Look' x st -> flip Look' st <$> go x
    --
    Aura' au    -> pure $ Aura' au
    Fork' as au -> pure $ Fork' as au
    Rail' x y c -> Rail' <$> go x <*> go y <*> care c
    Gate' x y c -> Gate' <$> go x <*> go y <*> care c
    --Face' fs x -> Face' fs <$> go x
    Noun' -> pure Noun'
    Void' -> pure Void'
    Type' -> pure Type'

  care :: (MonadCheck m, Show a) => Code (Hop Rump a) -> m (Code a)
  care = \case
    -- This stays put because it's actually an axis into the stored closure.
    Stub st -> pure $ Stub st
    Fore (New r) -> bail (PareFree r bas)
    Fore (Old x) -> pure $ Fore x
    --
    Atom a -> pure $ Atom a
    Cell c d -> Cell <$> care c <*> care d
    Lamb c -> Lamb <$> care c
    --
    Plus c -> Plus <$> care c
    Slam c d -> Slam <$> care c <*> care d
    Equl c d -> Equl <$> care c <*> care d
    Test c d e -> Test <$> care c <*> care d <*> care e
    Fish f c -> Fish f <$> care c
    --
    Aura au -> pure $ Aura au
    Fork as au -> pure $ Fork as au
    Rail c d -> Rail <$> care c <*> care d
    Gate c d -> Gate <$> care c <*> care d
    --Face fs c -> Face fs <$> care c
    Noun -> pure Noun
    Void -> pure Void
    Type -> pure Type
    --
    With c d -> With <$> care c <*> care d
    Push c d -> Push <$> care c <*> care d -}


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
    where new = Fore' @(Hop Axis a) (New 1)

  -- XX should we read and do comb explosion?

  (Lamb' j, _) -> fits fit (jamb (smap Old j) new) (Slam' (smap Old u) new)
    where new = Fore' @(Hop Axis a) (New 1)

  (_, Lamb' k) -> fits fit (Slam' (smap Old t) new) (jamb (smap Old k) new)
    where new = Fore' @(Hop Axis a) (New 1)


  (Plus' v, Plus' w) -> fits FitSame v w
  (Plus'{}, _) -> fitsFail
  (_, Plus'{}) -> fitsFail

  -- Since it hasn't been evaluated away, we are dealing with an opaque type
  -- function application. This means we have no choice but to regard the
  -- function as invariant in its argument.
  (Slam' v w, Slam' v' w') -> do fits fit v v'; fits FitSame w w'
  (Slam'{}, _) -> fitsFail
  (_, Slam'{}) -> fitsFail

  (Equl' v w, Equl' v' w') -> asum
    [ fits FitSame v v' >> fits FitSame w w'
    , fits FitSame v w' >> fits FitSame w v'
    ]
  (Equl'{}, _) -> fitsFail
  (_, Equl'{}) -> fitsFail

  (Test' u v w, Test' u' v' w') -> do
    fits FitSame u u'
    fits fit v v'
    fits fit w w'
  (Test'{}, _) -> fitsFail
  (_, Test'{}) -> fitsFail

  (Fish' f v, Fish' f' v') -> do
    when (f /= f') fitsFail
    fits FitSame v v'
  (Fish'{}, _) -> fitsFail
  (_, Fish'{}) -> fitsFail

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
    new = Fore' @(Hop Axis a) (New 1)

  (Rail' v j, Cell' w u) -> do
    fits fit v w
    fits fit (jamb j' $ read new v') u'
   where
    v' = smap Old v
    j' = smap Old j
    u' = smap Old u
    new = Fore' @(Hop Axis a) (New 1)

  (Cell' v u, Rail' w k) -> do
    fits fit v w
    fits fit u' (jamb k' $ read new w')
   where
    u' = smap Old u
    w' = smap Old w
    k' = smap Old k
    new = Fore' @(Hop Axis a) (New 1)

  (Gate' v j, Gate' w k) -> do
    fits fit v w
    fits fit (jamb j' $ read new v') (jamb k' $ read new w')
   where
    v' = smap Old v
    w' = smap Old w
    j' = smap Old j
    k' = smap Old k
    new = Fore' @(Hop Axis a) (New 1)

  (Type', Type') -> pure ()

  (Aura'{}, _) -> fitsFail
  (_, Aura'{}) -> fitsFail
  (Fork'{}, _) -> fitsFail
  (_, Fork'{}) -> fitsFail
  (Cell'{}, _) -> fitsFail
  (_, Cell'{}) -> fitsFail
  (Rail'{}, _) -> fitsFail
  (_, Rail'{}) -> fitsFail
  (Gate'{}, _) -> fitsFail
  (_, Gate'{}) -> fitsFail

 where
  fitsFail = bail (FitsFail fit t u)




-- Find ------------------------------------------------------------------------


peek :: (MonadCheck m, Var a) => Loc -> Type a -> Axis -> m (Type a)
peek loc a typ = undefined

data Line a = Line
  { lem :: Semi a
  , las :: [Dash a]
  }

data Dash a

deriving instance (Show a) => Show (Line a)
deriving instance (Show a) => Show (Dash a)

find :: forall a m. (MonadCheck m, Var a)
     => Type a -> [Axis] -> m (Stub, Line a)
find = undefined

-- Pelt system -----------------------------------------------------------------

repo :: Type a -> Type a
repo = id  -- XX faces

-- | Extract the computational content from a pelt (i.e. the testing part).
fish :: Pelt -> Fish
fish fis = case fis of
  Punt -> Tuna
  Part a -> Sole a
  Pair p q -> char (fish p) (fish q)
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
{-
vain :: Var a => Type a -> Type a
vain = \case
  Fork' ss _ | null ss -> Void'
  Cell' t u -> case (vain t, vain u) of
    (Void', _) -> Void'
    (_, Void') -> Void'
    (t, u) -> Cell' t u
  Rail' t j -> Rail' t j  -- XX FIXME
  t -> t
-}

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
        fis = Fish' (fish p) undefined
        tes = Test' fis tSmol t


-- Type checking ---------------------------------------------------------------

-- | Type check the condition of a Test, producing its Code as well as the
-- possibly refined subjects for the branches to be checked against.
chip :: (MonadCheck m, Var a)
     => Con a -> Soft -> m (Code Void, Con a, Con a)
chip con = \case
  Fis p (Wng w) -> undefined {-do
    (st, lin@Line{lem, lyt}) <- find con w
    (tb, tt) <- fuse con (lem, lyt) p
    tru <- seal con lin{lem=tb, lyt=tt}
    (fb, ft) <- crop con (lem, lyt) p
    fal <- seal con lin{lem=fb, lyt=ft}
    h <- fish p
    -- XX TODO FIXME rezip the find zipper
    pure (Fish h $ Stub st, tru, fal)-}

  sof -> do
    x <- work con FitNest sof Flag'
    pure (x, con, con)

-- | Given subject type and knowledge, verify that code has result type.
-- Since the expected result type is known in this mode, we can lighten the
-- user's annotation burden, e.g. on |= argument. Read about "bidirectional type
-- checking" to learn more.
work :: forall a m. (MonadCheck m, Var a)
     => Con a -> Fit -> Soft -> Type a -> m (Code Void)
work con@Con{lvl, sut} fit cod gol = act (ActWork con fit cod gol)
  let playFits = do (x, t') <- play con cod
                    fits fit t' gol
                    pure x
  in case cod of
    Wng{} -> playFits

    -- for introduction forms except atoms, we push the type constraint inward
    -- this allows the user to type-annotate the whole of a big data structure
    -- indcluding cores and gates, without having to also annotate the insides
    -- unless they want to.
    Atm{} -> playFits

    Cel c d -> case gol of
      -- Face' fs t -> work con fit cod t
      Type' -> playFits
      Cell' t u -> do
        x <- work con fit c t
        y <- work con fit d u
        pure (Cell x y)
      Rail' t j -> do
        x <- work con fit c t
        let u = jamb j $ evil sut x
        y <- work con fit d u
        pure (Cell x y)
      _ -> playFits

    Lam p c -> case gol of
      -- Face' fs t -> work con fit cod t
      Gate' t j-> do
        --t' <- toil con fit p (Rump' $ Leg' (lvl + 1, 3)) t
        --let fs = derm p
        --let can = hide' con $ face' fs t'
        --let u = eval (Cons' sub $ Rump' $ Leg' (lvl + 1, 3)) e
        --y <- work can fit c u
        undefined -- pure (Lamb y)
      _ -> playFits

    {-Fac p c -> do
      -- XX think about whether we should instead play here, so that toil can
      -- operate against a more specific scrutinee type.
      x <- work con fit c typ
      -- XX It's strictly wrong to use typ here; we should use the result of
      -- playing c. But playing c could fail, so...
      _ <- toil con fit p (evil ken x) typ
      let fs = derm p
      asum
        [ fits FitNest typ Type' >> pure (face fs x)
        , pure x
        ]-}

    -- elimination forms just use nest
    Plu{} -> playFits
    Sla{} -> playFits
    Equ{} -> playFits
    Fis{} -> playFits  -- not inside Tes

    Tes c d e -> do
      (x, tru, fal) <- chip con c
      y <- work tru fit d gol
      z <- work fal fit e gol
      pure (Test x y z)

    -- "rhetorical" tests are required to evaluate to true at compile time.
    -- this will be a rigorous exercise of the crop/fuse system and will be
    -- our mechanism of exhaustiveness checking.
    Rhe c d -> do
      x <- work con FitNest c Flag'
      case evil sut x of
        Atom' 0 -> work con fit d gol
        b -> bail (WorkMiss c b)

    --Run{} -> playFits

    -- likewise with types
    Ral{} -> playFits
    Gat{} -> playFits
    --Gold{} -> playFits
    --Lead{} -> playFits

    {-Wit c d -> do
      (x, t) <- play con c
      let kan = evil ken x
      y <- work Con{lvl=0, sut=(grow t), ken=(grow kan)} fit d (grow typ)
      pure $ With x y-}

    Pus c d -> do
      (x, t) <- play con c
      work (shew con (evil sut x) t) fit d gol

    Net{} -> playFits
    Cat{} -> playFits

-- | Require the given type to be a function type.
-- XX Deppy had a cas rule here; why?
{-needGate :: (MonadCheck m, Var a)
         => Con a -> Type a -> m (Type a, Semi a, Code a)
needGate con = \case
  Gate' t s c -> pure (t, s, c)
  Face' _ t -> needGate con t
  t -> bail $ NeedGate t-}

-- | Given subject type and knowledge, determine product type of code
play :: forall a m. (MonadCheck m, Var a)
     => Con a -> Soft -> m (Code Void, Type a)
play con@Con{lvl, sut} cod = act (ActPlay con cod) undefined
  {-Wng w -> do
    (st, Line{lyt}) <- find con w
    pure (Stub st, lyt)

  Atm a Rock t -> pure (Atom a, Fork' (singleton a) t)

  Atm a Sand t -> pure (Atom a, Aura' t)

  Cns c d -> do
    (x, t) <- play con c
    (y, u) <- play con d
    -- XX the below invocation appears identical to "constructing a nondependent
    -- cell". Think hard about this.
    pure (Cons x y, Cell' t ken (loft (lvl + 1) u))

  Lam p c -> do
    -- TODO replace with gold core
    t <- romp con p
    let fs = derm p
    (x, u) <- play (hide' con $ face' fs t) c
    pure (Lamb x, Gate' t ken (loft (lvl + 1) u))

  Fac p c -> do
    (x, t) <- play con c
    t' <- toil con FitNest p (evil ken x) t
    let fs = derm p
    -- XX think about under what circumstances we can strip the first face.
    -- It's annoying to have these lying around in the seminoun.
    -- XX confirm this is right
    asum
      [ fits FitNest t Type' >> pure (face fs x, face' fs t')
      , pure (x, face' fs t')
      ]

  Plu c -> do
    -- Following 140, we do not propagate aura.
    x <- work con FitNest c (Aura' "")
    pure (Plus x, Aura' "")

  Sla c d -> do
    (x, ct) <- play con c
    (at, s, rc) <- needGate con ct
    y <- work con FitNest d at
    pure (Slam x y, eval (Cons' s $ evil ken y) rc)

  Equ c d -> do
    (x, _) <- play con c
    (y, _) <- play con d
    pure (Equl x y, Flag')

  Tes c d e -> do
    (x, tru, fal) <- chip con c
    (y, t) <- play tru d
    (z, u) <- play fal e
    r <- join t u
    pure (Test x y z, r)

  -- "rhetorical" tests are required to evaluate to true at compile time.
  -- this will be a rigorous exercise of the crop/fuse system and will be
  -- our mechanism of exhaustiveness checking.
  Rhe c d -> do
    x <- work con FitNest c Flag'
    case evil ken x of
      Atom' 0 -> play con d
      b -> bail (PlayMiss c b)

  -- For Fis not inside Tes
  Fis p c -> do
    f <- fish p
    (x, _) <- play con c
    pure (Fish f x, Flag')

  Run sv st fom pt -> do
    st' <- work con FitNest st Type'
    sv' <- work con FitNest sv (evil ken st')
    (fom', _) <- play con fom  -- HACK XX fix when we have recursive types
    pt' <- work con FitNest pt Type'
    let ret = Test (Equl (Atom 0) (Stub $ Leg 3)) sv' (Aura "t")
    pure (Work st' sv' fom' pt', Cell' Flag' ken $ vacuous ret)

  Bas (Aur au) -> pure (Aura au, Type')

  Bas Flg -> pure (Fork (setFromList [0, 1]) "f", Type')

  Bas Nul -> pure (Fork (setFromList [0]) "n", Type')

  Bas (Fok as au) -> pure (Fork (setFromList as) au, Type')

  Bas Cel -> pure (Cell Noun Noun, Type')

  Cll c d -> do
    x <- work con FitNest c Type'
    y <- work (hide con (vacuous x)) FitNest d Type'
    pure (Cell x y, Type')

  Gat c d -> do
    x <- work con FitNest c Type'
    y <- work (hide con (vacuous x)) FitNest d Type'
    pure (Gate x y, Type')

  Bas Non -> pure (Noun, Type')

  Bas Vod -> pure (Void, Type')

  Bas Typ -> pure (Type, Type')

  Wit c d -> do
    (x, t) <- play con c
    let kan = evil ken x
    (y, u) <- play Con{lvl=0, sut=(grow t), ken=(grow kan)} d
    ret <- pare u
    pure (With x y, ret)

  Pus c d -> do
    (x, t) <- play con c
    (y, u) <- play (shew con (evil ken x) t) d
    pure (Push x y, u)

  Net{sof, typ} -> do
    x <- work con FitNest typ Type'
    let t = evil ken x
    y <- work con FitNest sof t
    pure (y, t)

  Cat{sof, typ} -> do
    x <- work con FitNest typ Type'
    let t = evil ken x
    y <- work con FitCast sof t
    pure (y, t)-}

-- | Read code back to soft, making no attempt to untranslate axes to wings with
-- names.
rest :: forall a m. Ord a => Code a -> Soft
rest = \case
  Stub (Leg a) -> Wng [a]
  Fore x -> Wng [1337] -- [Ally $ tshow @(Hop () a) $ Old x]  -- hack for printing
  --
  Atom a -> Atm a Sand (heuAura a)
  Cell c d -> Cel (rest c) (rest d)
  -- XX this loss of facial information may be unfortunate for diagnostic
  -- purposes. Think about this. Fixed by doze?
  Lamb c -> Lam Punt (rest c)
  --
  Plus c -> Plu (rest c)
  Slam c d -> Sla (rest c) (rest d)
  Equl c d -> Equ (rest c) (rest d)
  Test c d e -> Tes (rest c) (rest d) (rest e)
  Fish h c -> Fis (pond h) (rest c)
  --
  Aura au -> Aur au
  Rail c d -> Ral (rest c) (rest d)
  Gate c d -> Gat (rest c) (rest d)
  Fork ss t -> Fok (Just $ rest t) (map rest $ toList ss)
  -- Face (Mask m) c -> Fac (Peer m) (rest c)
  -- Face (Link ls) c -> Fac Punt (rest c)  -- FIXME ?
  Noun -> Non
  Void -> Vod
  Type -> Typ
  With c d -> Wit (rest c) (rest d)
  Push c d -> Pus (rest c) (rest d)

-- | Use a subject type to read back wing information in a much less shitty way.
doze :: Var a => Type a -> Code Stub -> Soft
doze typ = undefined

