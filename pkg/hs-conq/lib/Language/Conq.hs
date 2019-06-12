module Language.Conq where

import ClassyPrelude hiding (pure, (<.>), Left, Right)
import Data.Type.Equality
import Type.Reflection
import Data.Coerce
import GHC.Natural
import Control.Category

import Data.Bits ((.|.), shiftL, shiftR)
import Text.Show (showString, showParen)

--------------------------------------------------------------------------------

type Tup a b = (a, b)
data Sum a b = L a | R b
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (Sum a b) where
  show (L x) = case show x of { "()" → "L"; xs → "L" <> xs }
  show (R x) = case show x of { "()" → "R"; xs → "R" <> xs }

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

instance Show Exp where
  showsPrec d = \case
    ESubj     -> showString "."
    ENull     -> showString "~"
    EEval     -> showString "!"
    ELeft     -> showString "L"
    EWrit     -> showString "R"
    EHead     -> showString "-"
    ETail     -> showString "+"
    EDist     -> showString "%"
    EWith x y -> showsPrec d x . showsPrec d y
    ECons x y -> showString "["
               . showsPrec d x
               . showString " "
               . showsPrec d y
               . showString "]"
    ECase x y -> showString "<"
               . showsPrec d x
               . showString " "
               . showsPrec d y
               . showString ">"

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
  Kase :: Conq a r -> Conq b r -> Conq (Sum a b) r
  Dist :: Conq (Sum a b,s) (Sum (a,s) (b,s))
  With :: Conq s a -> Conq a r -> Conq s r
  Eval :: Conq (a, Conq a r) r








  -- Case :: Conq (a,s) r -> Conq (b,s) r -> Conq (Sum a b,s) r

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
  Kase p q -> case sut of { L l     -> run l p; R r     -> run r q; }






--Case p q -> case sut of (L l,x) -> run (l,x) p
--                        (R r,x) -> run (r,x) q

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
--Case l r -> ECase (toExp l) (toExp r)
  Kase l r -> ECase (toExp l) (toExp r)
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
case' x y = Kase x y . Dist

previewLeft :: Conq (Sum a b) (Sum () a)
previewLeft = Kase just nothing

previewWrit :: Conq (Sum a b) (Sum () b)
previewWrit = Kase nothing just


-- Pair Operations -------------------------------------------------------------

curry' :: Conq (a, b) c -> Conq s a -> Conq s b -> Conq s c
curry' f x y = With (Cons x y) f

both :: Conq a b -> Conq (a, a) (b, b)
both x = Cons (With Head x) (With Tail x)

dub_equal :: Conq (a, a) Bit -> Conq ((a, a), (a, a)) Bit
dub_equal cmp = With results and'
  where
    results = Cons (With (both Head) cmp) (With (both Tail) cmp)

dub_test :: Conq a Bit -> Conq (a, a) Bit
dub_test test = curry' and' (With Head test) (With Tail test)

dub_inc :: Conq a a -> Conq a Bit -> Conq (a, a) (a, a)
dub_inc inc null = With bump_low (if' low_zero bump_hig id)
  where
    bump_low = Cons (With Head inc) Tail
    bump_hig = Cons Head (With Tail inc)
    low_zero = With Head null

bit :: Int -> Bit
bit n = runTimes n val_bit_zero bit_inc


-- Boolean Operations ----------------------------------------------------------

type Bit = Sum () ()

true :: Conq s Bit
true = Writ . Null

false :: Conq s Bit
false = Left . Null

not' :: Conq Bit Bit
not' = Kase Writ Left

id' :: Conq Bit Bit
id' = Kase Writ Left

dup :: Conq a (a, a)
dup = Cons Subj Subj

if' :: Conq s Bit -> Conq s r -> Conq s r -> Conq s r
if' c t f = case' (With Tail f) (With Tail t) . Cons c Subj

and' :: Conq (Bit, Bit) Bit
and' = if' a2 a3 false

or' :: Conq (Bit, Bit) Bit
or' = if' a2 true a3

xor' :: Conq (Bit, Bit) Bit
xor' = if' a2 (With a3 not') a3

bit_eq :: Conq (Bit, Bit) Bit
bit_eq = if' a2 a3 (With a3 not')

bit_zero :: Conq s Bit
bit_zero = false

val_bit_zero :: Bit
val_bit_zero = run () bit_zero

bit_is_zero :: Conq Bit Bit
bit_is_zero = not'

bit_inc :: Conq Bit Bit
bit_inc = not'

-- Duo Operations (2 bit) ------------------------------------------------------

type Duo = (Bit, Bit)

duo_zero :: Conq s Duo
duo_zero = Cons bit_zero bit_zero

duo_is_zero :: Conq Duo Bit
duo_is_zero = dub_test bit_is_zero

duo_inc :: Conq Duo Duo
duo_inc = Kase (Cons true Tail) (Cons false (not' . Tail)) . Dist

factor :: Conq (Sum (a, c) (b, c)) (Sum a b, c)
factor = Kase (Cons (Left . Head) Tail)
              (Cons (Writ . Head) Tail)

duo :: Int -> Duo
duo n = runTimes n (run () duo_zero) duo_inc

duo_equal :: Conq (Duo, Duo) Bit
duo_equal = dub_equal bit_eq


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

n2 :: Conq a (Sum () (Sum () (Sum () a)))
n2 = Writ . n1
