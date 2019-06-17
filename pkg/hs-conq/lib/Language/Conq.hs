module Language.Conq where

import ClassyPrelude hiding ((<.>), Left, Right)
import Data.Type.Equality
import Type.Reflection
import Data.Coerce
import GHC.Natural
import Control.Category
import Control.Monad.State (State, get, put, evalState, runState)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except (throwE)

import Control.Lens ((&))
import Data.Bits ((.|.), shiftL, shiftR)
import Text.Show (showString, showParen)

import qualified Prelude as P

--------------------------------------------------------------------------------

type Tup a b = (a, b)
data Sum a b = L a | R b
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (Sum a b) where
  show (L x) = case show x of { "()" → "0"; xs → "0" <> xs }
  show (R x) = case show x of { "()" → "1"; xs → "1" <> xs }

--------------------------------------------------------------------------------

data Lit
    = Nil
    | LTup Lit Lit
    | LLef Lit
    | LRit Lit
  deriving (Eq, Ord)

instance Show Lit where
  show = \case
    Nil      -> "~"
    LTup x y -> "[" <> show x <> " " <> show y <> "]"
    LLef Nil -> "1"
    LRit Nil -> "0"
    LLef l   -> "<l " <> show l <> ">"
    LRit r   -> "<r " <> show r <> ">"

--------------------------------------------------------------------------------

class ToLit a where
  toLit :: a -> Lit

instance ToLit () where
  toLit () = Nil

instance (ToLit a, ToLit b) => ToLit (Sum a b) where
  toLit (L l) = LLef (toLit l)
  toLit (R r) = LRit (toLit r)

instance (ToLit a, ToLit b) => ToLit (Tup a b) where
  toLit (l, r) = LTup (toLit l) (toLit r)

--------------------------------------------------------------------------------

data Val
  = VN
  | V0 Val
  | V1 Val
  | VP Val Val

instance Show Val where
  show = show . valExp

valExp :: Val -> Exp
valExp VN = ENull
valExp v    = EWith ENull (go v)
  where
    go = \case
      VN      → ENull
      V0 VN → ELeft
      V1 VN → EWrit
      V0 l    → EWith (go l) ELeft
      V1 r    → EWith (go r) EWrit
      VP x y  → ECons (go x) (go y)

data TyExp
  = TENil
  | TESum TyExp TyExp
  | TETup TyExp TyExp
  | TEFor TyExp TyExp
  | TEAll TyExp
  | TEFix TyExp
  | TERef Int

data Ty
  = TNil
  | TSum Ty Ty
  | TTup Ty Ty
  | TFor Ty Ty
  | TVar Int

instance Show Ty where
  show = \case
    TNil     -> "~"
    TSum x y -> "<" <> show x <> " " <> show y <> ">"
    TTup x y -> "[" <> show x <> " " <> show y <> "]"
    TFor x y -> "(" <> show x <> " => " <> show y <> ")"
    TVar x   -> show x

type Unique  = Int
type Infer a = ExceptT () (State (Map Int Ty, Unique)) a
type Unify a = Maybe a

forAll :: Infer Ty
forAll = do
  (env, n) <- get
  put (env, n+1)
  pure (TVar n)

varIs :: Int -> Ty -> Infer Ty
varIs v (TVar x) | x==v = do
  pure (TVar x)
varIs v t = do
  (env, n) <- get
  put (insertMap v t env, n)
  pure t

finalize :: Ty -> Infer Ty
finalize (TVar v) = resolve v >>= finalize'
finalize t        = finalize' t

finalize' :: Ty -> Infer Ty
finalize' = \case
  TNil     -> pure TNil
  TVar x   -> pure (TVar x)
  TSum x y -> TSum <$> finalize x <*> finalize y
  TTup x y -> TTup <$> finalize x <*> finalize y
  TFor x y -> TFor <$> finalize x <*> finalize y

unify :: Ty -> Ty -> Infer Ty
unify x y = do
  x <- case x of { TVar v -> resolve v; x -> pure x }
  y <- case y of { TVar v -> resolve v; y -> pure y }
  unify' x y

unify' :: Ty -> Ty -> Infer Ty
unify' = curry \case
  ( TNil,       TNil       ) -> pure TNil
  ( TSum a1 b1, TSum a2 b2 ) -> TSum <$> unify a1 a2 <*> unify b1 b2
  ( TTup a1 b1, TTup a2 b2 ) -> TTup <$> unify a1 a2 <*> unify b1 b2
  ( TFor a1 b1, TFor a2 b2 ) -> TFor <$> unify a1 a2 <*> unify b1 b2
  ( ty,         TVar x     ) -> varIs x ty
  ( TVar x,     ty         ) -> varIs x ty
  ( _x,         _y         ) -> throwE ()

resolve :: Int -> Infer Ty
resolve v = do
  (env, _) <- get
  lookup v env & \case
    Nothing       -> pure (TVar v)
    Just (TVar x) -> resolve x
    Just x        -> pure x

expectFor :: Ty -> Infer (Ty, Ty, Ty)
expectFor = \case
  ty@(TFor x y) -> pure (ty, x, y)
  _             -> throwE ()

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (P.Left _)  = Nothing
eitherToMaybe (P.Right x) = Just x

runInfer :: Infer a -> Maybe a
runInfer = eitherToMaybe . flip evalState (mempty, 0) . runExceptT

infer :: Exp -> Infer Ty
infer = \case
  ENull -> do
    a <- forAll
    pure (TFor a TNil)

  ESubj -> do
    a <- forAll
    pure (TFor a a)

  ELeft -> do
    (a, b) <- (,) <$> forAll <*> forAll
    pure (TFor a (TSum a b))

  EWrit -> do
    (a, b) <- (,) <$> forAll <*> forAll
    pure (TFor b (TSum a b))

  EHead -> do
    (a, b) <- (,) <$> forAll <*> forAll
    pure $ TFor (TTup a b) a

  ETail -> do
    (a, b) <- (,) <$> forAll <*> forAll
    pure $ TFor (TTup a b) b

  EDist -> do
    (a, b, c) <- (,,) <$> forAll <*> forAll <*> forAll
    pure $ TFor (TTup (TSum a b) c) (TSum (TTup a c) (TTup b c))

  EEval -> do
    (a, b) <- (,) <$> forAll <*> forAll
    pure $ TFor (TTup a (TFor a b)) b

  EWith x y -> do
    (xt, xi, xo) <- infer x >>= expectFor
    (yt, yi, yo) <- infer y >>= expectFor
    unify xo yi
    pure (TFor xi yo)

  ECons x y -> do
    (xt, xi, xo) <- infer x >>= expectFor
    (yt, yi, yo) <- infer y >>= expectFor
    unify xi yi
    pure (TFor xi (TTup xo yo))

  ECase p q -> do
    (pt, pi, po) <- infer p >>= expectFor
    (qt, qi, qo) <- infer q >>= expectFor
    unify po qo
    pure (TFor (TSum pi qi) po)

data Exp
    = ESubj
    | ENull
    | EEval
    | ELeft
    | EWrit
    | EHead
    | ETail
    | EDist
    | EWith Exp Exp
    | ECons Exp Exp
    | ECase Exp Exp
  deriving (Eq, Ord)

runExp :: Val -> Exp -> Val
runExp s ESubj = s
runExp s e     = uncurry runExp (step s e)

-- Get a formula from a Val ----------------------------------------------------

{-
    for = <opk [opk for]>
    opk = <<dir get> <sim <otr plx>>
    dir = <L R>
    get = <- +>
    sim = <~ .>
    otr = <! %>
    plx = <@ :>
-}

flattenExp :: Exp -> Exp
flattenExp = go
  where
    go (EWith (EWith x y) z) = go (EWith x (EWith y z))
    go (EWith x y)           = EWith x (go y)
    go x                     = x

forVal :: Exp -> Val
forVal = \e ->
    case flattenExp e of
      EWith x y -> V1 $ VP (opkVal x) (forVal y)
      x         -> V0 (opkVal x)
  where
    opkVal :: Exp -> Val
    opkVal = \case
      EWith _ _ -> error "forVal: broken invariant"
      ELeft     -> V0 $ V0 $ V0 VN
      EWrit     -> V0 $ V0 $ V1 VN
      EHead     -> V0 $ V1 $ V0 VN
      ETail     -> V0 $ V1 $ V1 VN
      ENull     -> V1 $ V0 $ V0 VN
      ESubj     -> V1 $ V0 $ V1 VN
      EEval     -> V1 $ V1 $ V0 $ V0 VN
      EDist     -> V1 $ V1 $ V0 $ V1 VN
      ECase x y -> V1 $ V1 $ V1 $ V0 $ VP (forVal x) (forVal y)
      ECons x y -> V1 $ V1 $ V1 $ V1 $ VP (forVal x) (forVal y)


valFor :: Val -> Exp
valFor (V0 l)          = valOpk l
valFor (V1 (VP x y)) = EWith (valOpk x) (valFor y)
valFor _                 = ENull

valOpk :: Val -> Exp
valOpk (V0 (V0 x))        = valDir x
valOpk (V0 (V1 x))        = valGet x
valOpk (V1 (V0 x))        = valSim x
valOpk (V1 (V1 (V0 x))) = valOtr x
valOpk (V1 (V1 (V1 x))) = valPlx x
valOpk _                      = ENull

valDir :: Val -> Exp
valDir (V0 VN) = ELeft
valDir (V1 VN) = EWrit
valDir _           = ENull

valGet :: Val -> Exp
valGet (V0 VN) = EHead
valGet (V1 VN) = ETail
valGet _           = ENull

valSim :: Val -> Exp
valSim (V0 VN) = ENull
valSim (V1 VN) = ESubj
valSim _           = ENull

valOtr :: Val -> Exp
valOtr (V0 VN) = EEval
valOtr (V1 VN) = EDist
valOtr _           = ENull

valPlx :: Val -> Exp
valPlx (V0 (VP x y)) = ECase (valFor x) (valFor y)
valPlx (V1 (VP x y)) = ECons (valFor x) (valFor y)
valPlx _                 = ENull

-- Small-Step Interpreter ------------------------------------------------------

step :: Val -> Exp -> (Val, Exp)
step s = \case
  ENull             -> (VN, ESubj)
  ESubj             -> (s, ESubj)
  EWith ESubj y     -> (s, y)
  EWith x y         -> case step s x of
                         (s', ESubj) -> (s', y)
                         (s', x'   ) -> (s', EWith x' y)
  EEval             -> case s of
                         VP s' f' -> (s', valFor f')
                         _          -> (VN, ESubj)
  ECons ESubj ESubj -> (VP s s, ESubj)
  ECons x y         -> (VP (runExp s x) (runExp s y), ESubj)
  ELeft             -> (V0 s, ESubj)
  EWrit             -> (V1 s, ESubj)
  EHead             -> case s of
                         VP x _ -> (x,    ESubj)
                         _        -> (VN, ESubj)
  ETail             -> case s of
                         VP _ y -> (y,    ESubj)
                         _        -> (VN, ESubj)
  EDist             -> case s of
                         VP (V0 l) x -> (V0 (VP l x), ESubj)
                         VP (V1 r) x -> (V1 (VP r x), ESubj)
                         _               -> (VN, ESubj)
  ECase p q         -> case s of
                         V0 l -> (l, p)
                         V1 r -> (r, q)
                         _      -> (VN, ESubj)

displayExp :: Exp -> String
displayExp (EWith x y) = displayExp x <> "\n" <> displayExp y
displayExp x           = "\t" <> show x

traceRunExp :: Val -> Exp -> IO ()
traceRunExp s e = do
  putStrLn (tshow (valExp s))
  putStrLn (pack $ displayExp e)
  void getLine
  case e of
    ESubj -> putStrLn "DONE"
    _     -> uncurry traceRunExp (step s e)

traceRun :: Conq () r -> IO ()
traceRun = traceRunExp VN . toExp

{-
run sut = \case
  Null     -> ()
  Subj     -> sut
  With x y -> run (run sut x) y
  Eval     -> case sut of (s,f) -> run s f
  Cons x y -> (run sut x, run sut y)
  Left     -> L sut
  Writ     -> R sut
  Head     -> fst sut
  Tail     -> snd sut
  Dist     -> case sut of { (L l,x) -> L (l,x); (R r,x) -> R (r,x); }
  Case p q -> case sut of { L l     -> run l p; R r     -> run r q; }
-}

flattenCons :: Exp -> Exp -> [Exp]
flattenCons = \x -> go [x]
  where
    go acc (ECons x y) = go (x:acc) y
    go acc x           = reverse (x:acc)

instance Show Exp where
  show = \case
    ESubj     -> "."
    ENull     -> "~"
    EEval     -> "!"
    ELeft     -> "0"
    EWrit     -> "1"
    EHead     -> "-"
    ETail     -> "+"
    EDist     -> "%"
    EWith x y -> show y <> show x
    ECons x y -> "(" <> show x <> " " <> show y <> ")"
    ECase x y -> "<" <> show x <> " " <> show y <> ">"

--------------------------------------------------------------------------------

class ToConq a s r where
  toConq :: a -> Conq s r

instance ToConq (Conq s a, Conq a r) s r where
  toConq (x,y) = With x y

instance ToConq (Conq s a, Conq a b, Conq b r) s r where
  toConq (x,y,z) = With (toConq (x,y)) z

instance ToConq (Conq s a, Conq a b, Conq b c, Conq c r) s r where
  toConq (x,y,z,p) = With (toConq (x,y,z)) p

instance ToConq (Conq s a, Conq a b, Conq b c, Conq c d, Conq d r) s r where
  toConq (x,y,z,p,q) = With (toConq (x,y,z,p)) q

--------------------------------------------------------------------------------

data Conq s r where
  Subj :: Conq s s
  Null :: Conq s ()
  Left :: Conq a (Sum a b)
  Writ :: Conq b (Sum a b)
  Head :: Conq (Tup a b) a
  Tail :: Conq (Tup a b) b
  Cons :: Conq s a -> Conq s b -> Conq s (a, b)
  Case :: Conq a r -> Conq b r -> Conq (Sum a b) r
  Dist :: Conq (Sum a b,s) (Sum (a,s) (b,s))
  With :: Conq s a -> Conq a r -> Conq s r
  Eval :: Conq (a, Conq a r) r

instance Category Conq where
  id  = Subj
  (.) = flip With

instance Show (Conq s r) where
  show c = show (toExp c)

--------------------------------------------------------------------------------

run :: s -> Conq s r -> r
run sut = \case
  Null     -> ()
  Subj     -> sut
  With x y -> run (run sut x) y
  Eval     -> case sut of (s,f) -> run s f
  Cons x y -> (run sut x, run sut y)
  Left     -> L sut
  Writ     -> R sut
  Head     -> fst sut
  Tail     -> snd sut
  Dist     -> case sut of { (L l,x) -> L (l,x); (R r,x) -> R (r,x); }
  Case p q -> case sut of { L l     -> run l p; R r     -> run r q; }

times :: Int -> Conq s s -> Conq s s
times 0 _ = id
times 1 f = f
times n f = With f (times (n-1) f)

runTimes :: Int -> s -> Conq s s -> s
runTimes n sut conq = go n
  where
    go 0 = sut
    go 1 = run sut conq
    go n = run (go (n-1)) conq

--------------------------------------------------------------------------------

toExp :: Conq s r -> Exp
toExp = \case
  Subj     -> ESubj
  Null     -> ENull
  Eval     -> EEval
  Left     -> ELeft
  Writ     -> EWrit
  Head     -> EHead
  Tail     -> ETail
  Dist     -> EDist
  Cons x y -> ECons (toExp x) (toExp y)
  Case l r -> ECase (toExp l) (toExp r)
  With x y -> EWith (toExp x) (toExp y)

--------------------------------------------------------------------------------

fromExp :: forall s r. (Typeable s, Typeable r) => Exp -> Maybe (Conq s r)
fromExp = \case
  ESubj ->
    case testEquality (typeRep @s) (typeRep @r) of
      Just Refl -> Just (coerce Subj)
      Nothing   -> Nothing

  _ ->
    Nothing

-- Axis Lookup -----------------------------------------------------------------

a1 :: Conq a a
a1 = Subj

a2 :: Conq (Tup a b) a
a2 = Head

a3 :: Conq (Tup a b) b
a3 = Tail

a4 :: Conq (Tup (Tup a b) c) a
a4 = Head . Head

a5 :: Conq (Tup (Tup a b) c) b
a5 = Tail . Head

a6 :: Conq (Tup a (Tup b c)) b
a6 = Head . Tail

a7 :: Conq (Tup a (Tup b c)) c
a7 = Tail . Tail

a8 :: Conq (((a, b), c), d) a
a8 = Head . Head . Head


-- Eat Operations --------------------------------------------------------------

nothing :: Conq s (Sum () a)
nothing = Left . Null

just :: Conq a (Sum () a)
just = Writ

case' :: Conq (a,s) r -> Conq (b,s) r -> Conq (Sum a b,s) r
case' x y = Case x y . Dist

previewLeft :: Conq (Sum a b) (Sum () a)
previewLeft = Case just nothing

previewWrit :: Conq (Sum a b) (Sum () b)
previewWrit = Case nothing just


-- Pair Operations -------------------------------------------------------------

curry' :: Conq (a, b) c -> Conq s a -> Conq s b -> Conq s c
curry' f x y = With (Cons x y) f

both :: Conq a b -> Conq (a, a) (b, b)
both x = Cons (With Head x) (With Tail x)

dub_equal :: Conq (a, a) Bit -> Conq ((a, a), (a, a)) Bit
dub_equal cmp = With results bit_and
  where
    results = Cons (With (both Head) cmp) (With (both Tail) cmp)

dub_test :: Conq a Bit -> Conq (a, a) Bit
dub_test test = curry' bit_and (With Head test) (With Tail test)

dub_inc :: Conq a a -> Conq a Bit -> Conq (a, a) (a, a)
dub_inc inc null = With bump_low (if' low_zero bump_hig id)
  where
    bump_low = Cons (inc . Head) Tail
    bump_hig = Cons Head (inc . Tail)
    low_zero = null . Head

type Tag a = Sum a a -- Tag with a bit: <0 1>
type Inc a = Conq a (Tag a)

bit_incer :: Inc Bit
bit_incer = Case (Left . Writ) (Writ . Left)

duo_incer' :: Inc Duo
duo_incer' = incer bit_incer

duo_incer :: Inc Duo
duo_incer = Case (Left . Cons true Tail) carry . Dist
  where
    carry = Case (Left . Cons Left Writ) (Writ . Cons Left Left) . Tail

incer :: forall a. Inc a -> Inc (a, a)
incer i =
    Case Left hig . low
  where
    low, hig :: Inc (a, a)
    low = Dist . Cons (i . Head) Tail
    hig = Case (Left . flip') Writ . Dist . Cons (i . Tail) Head

nyb_incer :: Inc Nyb
nyb_incer = incer duo_incer

byt_incer :: Inc Byt
byt_incer = incer nyb_incer

short_incer :: Inc Short
short_incer = incer byt_incer

wide_incer :: Inc Wide
wide_incer = incer short_incer

long_incer :: Inc Long
long_incer = incer wide_incer

bit :: Int -> Bit
bit n = runTimes n val_bit_zero bit_inc


-- Random Combinators ----------------------------------------------------------

dup :: Conq a (a, a)
dup = Cons Subj Subj

eat :: Conq (Sum a a) a
eat = Case Subj Subj

flip' :: Conq (a, b) (b, a)
flip' = Cons Tail Head

if' :: Conq s Bit -> Conq s r -> Conq s r -> Conq s r
if' c t f = case' (f . Tail) (t . Tail) . Cons c Subj

factor :: Conq (Sum (a, c) (b, c)) (Sum a b, c)
factor = Case (Cons (Left . Head) Tail)
              (Cons (Writ . Head) Tail)


-- Boolean Operations ----------------------------------------------------------

type Bit = Sum () ()

true :: Conq s Bit
true = Writ . Null

false :: Conq s Bit
false = Left . Null

bit_not :: Conq Bit Bit
bit_not = Case Writ Left

bit_id :: Conq Bit Bit
bit_id = Case Left Writ

bit_and :: Conq (Bit, Bit) Bit
bit_and = Case false Tail . Dist

bit_or :: Conq (Bit, Bit) Bit
bit_or = Case Tail true . Dist

bit_xor :: Conq (Bit, Bit) Bit
bit_xor = Case Tail (bit_not . Tail) . Dist

bit_equal :: Conq (Bit, Bit) Bit
bit_equal = Case (bit_not . Tail) Tail . Dist

bit_zero :: Conq s Bit
bit_zero = false

val_bit_zero :: Bit
val_bit_zero = run () bit_zero

bit_is_zero :: Conq Bit Bit
bit_is_zero = bit_not

bit_inc :: Conq Bit Bit
bit_inc = bit_not

-- Duo Operations (2 bit) ------------------------------------------------------

type Duo = (Bit, Bit)

duo_zero :: Conq s Duo
duo_zero = Cons bit_zero bit_zero

duo_is_zero :: Conq Duo Bit
duo_is_zero = dub_test bit_is_zero

duo_inc :: Conq Duo Duo
duo_inc = Case (Cons true Tail) (Cons false (bit_not . Tail)) . Dist

duo :: Int -> Duo
duo n = runTimes n (run () duo_zero) duo_inc

duo_equal :: Conq (Duo, Duo) Bit
duo_equal = dub_equal bit_equal


-- Nibble Operations (4 bit) ---------------------------------------------------

type Nyb = (Duo, Duo)

nyb_zero :: Conq a Nyb
nyb_zero = Cons duo_zero duo_zero

nyb_is_zero :: Conq Nyb Bit
nyb_is_zero = dub_test duo_is_zero

nyb_inc :: Conq Nyb Nyb
nyb_inc = dub_inc duo_inc duo_is_zero

nyb :: Int -> Nyb
nyb n = runTimes n (run () nyb_zero) nyb_inc

nyb_equal :: Conq (Nyb, Nyb) Bit
nyb_equal = dub_equal duo_equal


-- Byte Operations (8 bit) -----------------------------------------------------

type Byt = (Nyb, Nyb)

byt_zero :: Conq a Byt
byt_zero = Cons nyb_zero nyb_zero

byt_is_zero :: Conq Byt Bit
byt_is_zero = dub_test nyb_is_zero

byt_inc :: Conq Byt Byt
byt_inc = dub_inc nyb_inc nyb_is_zero

byt :: Int -> Byt
byt n = runTimes n (run () byt_zero) byt_inc

byt_equal :: Conq (Byt, Byt) Bit
byt_equal = dub_equal nyb_equal


-- Short Operations (16 bit) ---------------------------------------------------

type Short = (Byt, Byt)

short_zero :: Conq a Short
short_zero = Cons byt_zero byt_zero

short_is_zero :: Conq Short Bit
short_is_zero = dub_test byt_is_zero

short_inc :: Conq Short Short
short_inc = dub_inc byt_inc byt_is_zero

short :: Int -> Short
short n = runTimes n (run () short_zero) short_inc

short_equal :: Conq (Short, Short) Bit
short_equal = dub_equal byt_equal


-- Wide Operations (32 bit) ----------------------------------------------------

type Wide = (Short, Short)

wide_zero :: Conq a Wide
wide_zero = Cons short_zero short_zero

wide_is_zero :: Conq Wide Bit
wide_is_zero = dub_test short_is_zero

wide_inc :: Conq Wide Wide
wide_inc = dub_inc short_inc short_is_zero

wide :: Int -> Wide
wide n = runTimes n (run () wide_zero) wide_inc

wide_equal :: Conq (Wide, Wide) Bit
wide_equal = dub_equal short_equal


-- Long Operations (64 bit) ----------------------------------------------------

type Long = (Wide, Wide)

long_zero :: Conq a Long
long_zero = Cons wide_zero wide_zero

long_is_zero :: Conq Long Bit
long_is_zero = dub_test wide_is_zero

long_inc :: Conq Long Long
long_inc = dub_inc wide_inc wide_is_zero

long :: Int -> Long
long n = runTimes n (run () long_zero) long_inc

long_equal :: Conq (Long, Long) Bit
long_equal = dub_equal wide_equal

n0 :: Conq a (Sum () a)
n0 = Left . Null

n1 :: Conq a (Sum () (Sum () a))
n1 = Writ . n0

n2  = Writ . n1
n3  = Writ . n2
n4  = Writ . n3
n5  = Writ . n4
n6  = Writ . n5
n7  = Writ . n6
n8  = Writ . n7
n9  = Writ . n8
n10 = Writ . n9


--------------------------------------------------------------------------------

-- ctx for -> (ctx for)
gate :: Val -> Exp -> Val
gate v e = VP v (forVal e)

-- `$-(a a)`: Identity
identFn :: Val
identFn = gate VN (toExp Head)

-- `$-(a b)`: Trivial-Loop
spinFn :: Val
spinFn = gate VN fire

-- `$-((list a) @)`: List-Length
lenFn :: Val
lenFn = gate (V0 VN) lenFnBody

lenFnBody :: Exp
lenFnBody = EWith EDist
          $ ECase (EWith ETail EHead)
          $ EWith (ECons (EWith EHead ETail)
                         (EWith ETail (ECons (EWith EHead EWrit) ETail)))
          $ fire

swapFn :: Val
swapFn = gate VN (toExp swapFnBody)

swapFnBody :: Conq ((a,b),x) (b,a)
swapFnBody = Cons Tail Head . Head

-- ctx lFor rFor -> (ctx (lfor rfor))
coreTwo :: Val -> Exp -> Exp -> Val
coreTwo v l r = VP v (VP (forVal l) (forVal r))

evenOddCore :: Val
evenOddCore = coreTwo VN evArm odArm

evArm :: Exp
evArm = EWith EDist
      $ ECase (toExp true)
      $ fireRit

odArm :: Exp
odArm = EWith EDist
      $ ECase (toExp false)
      $ fireLef

-- (arg (ctx for)) -> ((arg (ctx for)) for)!
fire :: Exp
fire = EWith (toExp reOrg) EEval
  where
  reOrg :: Conq (a,(c,f)) ((a,(c,f)),f)
  reOrg = Cons Subj (Tail . Tail)

-- (arg (ctx (lfor rfor))) -> ((arg (ctx (lfor rfor))) lfor)!
fireLef :: Exp
fireLef = EWith (toExp reOrg) EEval
  where
  reOrg :: Conq (a,(c,(l,r))) ((a,(c,(l,r))),l)
  reOrg = Cons Subj (Head . Tail . Tail)

-- (arg (ctx (lfor rfor))) -> ((arg (ctx (lfor rfor))) rfor)!
fireRit :: Exp
fireRit = EWith (toExp reOrg) EEval
  where
  reOrg :: Conq (a,(c,(l,r))) ((a,(c,(l,r))),r)
  reOrg = Cons Subj (Tail . Tail . Tail)

-- Demos -----------------------------------------------------------------------

type Payload = (Val, Exp)

demo :: Payload -> IO ()
demo (s,f) = traceRunExp s f

dumbLoop :: Exp
dumbLoop = EWith (ECons ESubj ESubj) EEval

dumbLoopP :: Payload
dumbLoopP = (forVal dumbLoop, dumbLoop)

demo_dumb_loop :: IO ()
demo_dumb_loop = demo dumbLoopP

demo_duo_overflow :: IO ()
demo_duo_overflow = traceRun (duo_incer . times 3 (eat . duo_incer) . duo_zero)

demo_nat_constr :: IO ()
demo_nat_constr = traceRun n10

-- [[-e +] +]!
fix :: Val -> Exp -> Payload
fix x e = (VP x (forVal fe), fe)
  where
    fe = EWith (ECons (ECons (EWith EHead e) ETail) ETail) EEval

natOverflow :: Exp
natOverflow = EWith (ECons (ECons (EWith EHead EWrit) ETail) ETail) EEval

natOverflowPay :: Payload
natOverflowPay = fix (V0 VN) EWrit

demo_nat_inc_loop :: IO ()
demo_nat_inc_loop = demo natOverflowPay

duo_zero_val :: Val
duo_zero_val = VP (V0 VN) (V0 (VN))

short_zero_val :: Val
short_zero_val = runExp VN (toExp short_zero)

short_inc_loop :: IO ()
short_inc_loop = demo $ fix (V0 short_zero_val) (toExp (short_incer . eat))
