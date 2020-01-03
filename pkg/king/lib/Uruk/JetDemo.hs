{-
    DONE Refactor jet handling.
    DONE Stop storing `Fast` arguments in reverse order.
    DONE Store arity (not arity - 1) in Fast.

    TODO Cleanup jet refactor.

      - Rearrange things so that jet matching, arity, and reduction are
        defined together. The current approach is easy to fuck up and hard
        to test.

    TODO Simplify Nat jets

      - Write a `nat` function that converts a church encoded natural
        into a jetted church encoded natural. In jets that operate on
        nats, don't bother keeping everything in the right shape. Simply
        do the operation and then call the nat jet. The nat jet should
        execute the natural number against l_zero and l_succ, and jet
        the result.

    TODO Normalization without jets (all jets implemented with their code)

      - Generalize `reduce` and `normalize` to support unjetted reduction.

    TODO Write tests (show that jets matching doesn't affect result)

      - unmatch jets; match jets == match jets

      - These should all produce the same result:

        - normalize with jets
        - unmatch jets, normalize with jets
        - unmatch jets, normalize without jets, match jets

    TODO Hook up front-end to JetComp
    TODO Implement REPL.
    TODO Implement script-runner.
    TODO Define jets in front-end language using template haskell.
    TODO Implement jet equality.
-}

module Uruk.JetDemo where

import ClassyPrelude
import Data.Bits
import Data.Void

import Data.Function    ((&))
import Data.List        (iterate, (!!))
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

pattern Nat n = Fast 2 (JNat n) []

data Jet
    = Slow Positive Ur Ur -- unmatched jet: arity, tag, body
    | Eye
    | Bee
    | Sea
    | Sn Positive
    | Bn Positive
    | Cn Positive
    | Wait Positive
    | Fix
    | JNat Natural
    | JFol
    | JAdd
    | JInc
    | Dec
    | Mul
    | Sub
    | JUni
    | Lef
    | Rit
    | Cas
    | Con
    | Car
    | Cdr
  deriving (Eq, Ord)

data UrPoly j
    = UrPoly j :@ UrPoly j
    | J Positive
    | K
    | S
    | D
    | Fast !Natural j [UrPoly j]
  deriving (Eq, Ord)

type Ur = UrPoly Jet

jetExpand ∷ Positive → Ur
jetExpand = go
  where go = \case { 1 → J 1; n → go (pred n) :@ J 1 }

unSlow ∷ Ur → [Ur] → Ur
unSlow u = go u . reverse
  where go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

instance Show a => Show (UrPoly a) where
    show = \case
        x :@ y      → "(" <> intercalate "" (show <$> flatten x [y]) <> ")"
        J n          → replicate (fromIntegral n) 'j'
        K            → "k"
        S            → "s"
        D            → "d"
        Fast _ j []  → show j
        Fast _ j us  → fast j us
      where
        flatten (x :@ y) acc = flatten x (y : acc)
        flatten x        acc = (x : acc)

        fast j us = "[" <> intercalate "" (show j : fmap show us) <> "]"

instance Show Jet where
    show = \case
        Slow n t b → show (J n :@ t :@ b)
        JNat n     → "#" <> show n
        Fix        → "!"
        Eye        → "i"
        Bee        → "b"
        Sea        → "c"
        Bn n       → "b" <> show n
        Cn n       → "c" <> show n
        Sn n       → "s" <> show n
        JFol        → ","
        JAdd        → "+"
        JInc        → "^"
        Dec        → "_"
        Mul        → "*"
        Sub        → "-"
        Lef        → "L"
        Rit        → "R"
        Cas        → "%"
        Con        → "&"
        Car        → "<"
        Cdr        → ">"
        JUni        → "~"
        Wait n     → "w" <> show n


-- Normalized Values -----------------------------------------------------------

newtype Val = MkVal { valUr ∷ Ur }
  deriving newtype (Eq, Ord, Show)

urVal ∷ Ur → Val
urVal ur =
    reduce ur & \case
        Nothing → MkVal ur
        Just ru → urVal ru


-- Named Functions -------------------------------------------------------------

data Named a = Named { _nName ∷ String, unNamed ∷ a }

instance Show (Named a) where
  show (Named s _) = s


--------------------------------------------------------------------------------

data Match = MkMatch
    { mFast ∷ Jet
    , mArgs ∷ Positive
    , mName ∷ Val
    , mBody ∷ Val
    }
  deriving (Show)

match ∷ Jet → Positive → Ur → Ur → Match
match j n t b = MkMatch j n (urVal t) (urVal b)

type Check = Named (Positive → JetTag → Val → Maybe Jet)

type DashEntry = Either Match Check

type JetTag  = Val
type Matches = Map (Positive, JetTag, Val) Jet

data Dash = Dash Matches [Check]
  deriving (Show)

simpleEnt ∷ Match → DashEntry
simpleEnt = Left

predikEnt ∷ Check → DashEntry
predikEnt = Right

mkDash ∷ [DashEntry] → Dash
mkDash = foldl' go (Dash mempty [])
  where
    go ∷ Dash → DashEntry → Dash
    go (Dash ms cs) = \case
        Left (MkMatch{..}) → Dash (insertMap (mArgs,mName,mBody) mFast ms) cs
        Right chk          → Dash ms (chk : cs)

dashLookup ∷ Positive → Ur → Ur → Maybe Jet
dashLookup n t b = findMatch <|> passCheck
  where
    (tv,bv)    = (urVal t, urVal b)
    Dash ms cs = dash
    findMatch  = lookup (n, tv, bv) ms
    passCheck  = headMay (mapMaybe (\(Named _ ok) -> ok n tv bv) cs)


-- Jet Dashboard ---------------------------------------------------------------

unChurch ∷ Ur → Maybe Natural
unChurch = \case
    S :@ K                   -> Just 0
    S :@ (S:@(K:@S):@K) :@ n -> succ <$> unChurch n
    _                        -> Nothing

emp ∷ Ur
emp = K

pattern J1 = J 1 :@ K
pattern J2 = J 2 :@ K
pattern J3 = J 3 :@ K
pattern J4 = J 4 :@ K

pattern W2 = Fast 3 (Wait 2) []

-- Z = \f -> (\x -> f (\v -> wait2 x x v)) (\x -> f (\v -> wait2 x x v))
pattern Z = S :@ (S:@(S:@(K:@S):@K):@(K:@(S:@W2:@I)))
              :@ (S:@(S:@(K:@S):@K):@(K:@(S:@W2:@I)))

{-
    TODO:

    Jet registration becomes an infinite loop because jet bodies are
    normalized, but jet matching in the bodies depends on the jet
    dashboard, which depends on the normalized jet body.

    Giving each jet a unique name would solve this, but maybe it's still
    posible to run into this problem by accident? Whatever.

    For now, I'm hacking around this by using a unjetted version of
    `fol` in jet bodies.
-}

ch_succ = S :@ (S :@ (K :@ S) :@ K)
ch_zero = S :@ K

--  zer = \i z -> z
--  suc = \n -> \i z -> i (n i z)
--  one = inc zer
--  fol = \n -> n inc zer
--  uni = K
--  dec = \n -> C (n (\x -> C x (\y -> R zer) (\y -> R (inc y))) (L uni))
--                (\g -> L uni)
--                (\g -> R (J2 (fol g)))
--  mul =
--  sub = \x y -> y (\z -> CAS z LEF DEC) (RIT x)
--  lef = \x l r -> l x
--  rit = \x l r -> r x
--  cas = \b l r -> b l r
--  con = \x y f -> f x y
--  car = \p -> p (\x y -> x)
--  cdr = \p -> b (\x y -> y)

-- fix f x = f (W2 fix f) x
-- fix = Z (\fx -> wait2 Jet2 (\f x -> f (fx f) x))
l_fix = ( (S :@ I)
          :@
          ((W2 :@
            ((S :@ (K :@ ((S :@ (K :@ (J 2 :@ K))) :@ (S :@ I))))
             :@
             ((S :@ W2) :@ I)))
           :@
           ((S :@ (K :@ ((S :@ (K :@ (J 2 :@ K))) :@ (S :@ I))))
            :@
            ((S :@ W2) :@ I))))

j_fix = match Fix 2 emp l_fix
fix = jetExp j_fix

rit = fast Rit
dec = fast Dec
pattern Fol = Fast 1 JFol []
lef = fast Lef
cas = fast Cas
pattern Uni = Fast 2 JUni []
wait n = fast (Wait n)

l_zer = S :@ K
l_one = S :@ (S:@(K:@S):@K) :@ (S:@K)
l_fol = S :@ (S:@I:@(K:@(S:@(S:@(K:@S):@K)))) :@ (K:@(S:@K))
l_dec = S:@(S:@(S:@(K:@cas):@(S:@(S:@I:@(K:@(S:@(S:@cas:@(K:@(K:@(rit:@ch_zero)))):@(K:@(S:@(K:@rit):@ch_succ))))):@(K:@(lef:@Uni)))):@(K:@(K:@(lef :@ Uni)))):@(K:@(S:@(K:@rit):@(S:@(K:@J2):@Fol)))
l_mul = D :@ D :@ D -- TODO
l_sub = S:@(K:@(S:@(S:@I:@(K:@(S:@(S:@cas:@(K:@lef)):@(K:@dec)))))):@(S:@(K:@K):@rit)
l_uni = K
l_lef = S :@ (K:@(S:@(K:@(S:@(K:@K))):@(S:@I))) :@ K
l_rit = S :@ (K:@(S:@(K:@K):@(S:@I))) :@ K
l_cas = I
l_con = S:@(K:@(S:@(K:@(S:@(K:@(S:@(K:@(S:@S:@(K:@K))):@K)):@S)):@(S:@I))):@K
l_car = S:@I:@(K:@K)
l_cdr = S:@I:@(K:@(S:@K))

e_zer = jetExp j_zer
e_one = jetExp j_one
e_fol = jetExp j_fol
e_dec = jetExp j_dec
e_mul = jetExp j_mul
e_sub = jetExp j_sub
e_uni = jetExp j_uni
e_lef = jetExp j_lef
e_rit = jetExp j_rit
e_cas = jetExp j_cas
e_con = jetExp j_con
e_car = jetExp j_car
e_cdr = jetExp j_cdr

j_zer = match (JNat 0) 2 emp l_zer
j_one = match (JNat 1) 2 emp l_one

j_nat ∷ Check
j_nat = Named "nat" chk
  where chk ∷ Positive → JetTag → Val → Maybe Jet
        chk 2 (MkVal K) u = JNat <$> unChurch (valUr u)
        chk n t         b = Nothing

j_wait ∷ Check
j_wait = Named "wait" chk
  where chk ∷ Positive → JetTag → Val → Maybe Jet
        chk n (MkVal I) (MkVal I) = Just $ Wait $ fromIntegral n
        chk _ _         _         = Nothing

j_fol = match JFol 1 emp l_fol
j_dec = match Dec 1 emp l_dec
j_mul = match Mul 2 emp l_mul
j_sub = match Sub 2 emp l_sub
j_uni = match JUni 2 emp l_uni
j_lef = match Lef 3 emp l_lef
j_rit = match Rit 3 emp l_rit
j_cas = match Cas 3 emp l_cas
j_con = match Con 3 emp l_con
j_car = match Car 1 emp l_car
j_cdr = match Cdr 1 emp l_cdr

dash ∷ Dash
dash = mkDash
    [ simpleEnt (monoJet mjI)
    , simpleEnt (monoJet mjB)
    , simpleEnt (monoJet mjC)
    , simpleEnt j_fix
    , simpleEnt j_fol
    , simpleEnt (monoJet mjInc)
    , simpleEnt (monoJet mjAdd)
    , simpleEnt j_dec
    , simpleEnt j_sub
    , simpleEnt j_uni
    , simpleEnt j_lef
    , simpleEnt j_rit
    , simpleEnt j_con
    , simpleEnt j_car
    , simpleEnt j_cdr
    , predikEnt j_nat
    , predikEnt j_cn
    , predikEnt j_sn
    , predikEnt j_bn
    , predikEnt j_wait
    ]


-- Evaluation ------------------------------------------------------------------

--
--  Repeatedly perform reductions until the input is fully normalized.
--
normalize ∷ Ur → IO Ur
normalize ur = do
    putStrLn (">>  " <> tshow ur)
    reduce ur & \case
        Nothing -> pure ur
        Just ru -> normalize ru

normalizeN ∷ Natural -> Ur → IO Ur
normalizeN 0 ur = pure ur
normalizeN n ur = do
    putStrLn (">>  " <> tshow ur)
    reduce ur & \case
        Nothing -> pure ur
        Just ru -> normalizeN (n-1) ru

--
--  Perform one reduction step. Return Nothing if the input is fully
--  normalized.
--
reduce ∷ Ur → Maybe Ur
reduce = \case
    K :@ x :@ y             → Just $ x
    (reduce → Just xv) :@ y → Just $ xv :@ y
    x :@ (reduce → Just yv) → Just $ x :@ yv
    S :@ x :@ y :@ z        → Just $ x :@ z :@ (y :@ z)
    D :@ x                  → Just $ jam x
    J n :@ J 1              → Just $ J (succ n)
    J n :@ t :@ b           → Just $ Fast (fromIntegral n) (match n t b) []
    Fast 0 u us             → Just $ runJet u us
    Fast n u us :@ x        → Just $ Fast (pred n) u (us <> [x])
    _                       → Nothing
  where
    match ∷ Positive → Ur → Ur → Jet
    -- ch n t b = fromMaybe (Slow n t b) $ dashLookup n t b
    match n t b = fromMaybe (error $ show (n,t,b)) $ dashLookup n t b

runJet ∷ Jet → [Ur] → Ur
runJet = curry \case
    (JAdd, xs) → runMonoJet mjAdd xs
    (JInc, xs) → runMonoJet mjInc xs
    (Bee, xs)  → runMonoJet mjB xs
    (Sea, xs)  → runMonoJet mjC xs

    ( Slow n t b,  us      ) → go b us
    ( Wait _,      u:us    ) → go u us
    ( Eye,         [x]     ) → x
    ( Bn _,        f:g:xs  ) → f :@ go g xs
    ( Cn _,        f:g:xs  ) → go f xs :@ g
    ( Sn _,        f:g:xs  ) → go f xs :@ go g xs
    ( Fix,         [f,x]   ) → f :@ (fast Fix :@ f) :@ x
    ( JNat n,      [x,y]   ) → church n :@ x :@ y

    ( JFol,         [x]     ) → x & \case
        Nat x → church x
        x     → l_fol :@ x

    ( Dec,         [x]     ) → x & \case
        Nat 0 → lef :@ Uni
        Nat n → rit :@ Nat (pred n)
        x     → l_rit :@ x

    ( Sub,         [x,y]   ) → (x,y) & \case
        ( Nat x, Nat y ) → sub x y
        ( _,     _     ) → l_sub :@ x :@ y

    ( Cas,         [s,l,r] ) → s & \case
        Fast _ Lef [x] → l :@ x
        Fast _ Rit [x] → r :@ x
        _              → l_cas :@ l :@ r

    ( Con,         [x,y,z] ) → z :@ x :@ y

    ( Car,         [p]     ) → p & \case
        Fast _ Con [x,_] → x
        _                → l_cdr :@ p

    ( Cdr,         [p]     ) → p & \case
        Fast _ Con [_,y] → y
        _                → l_cdr :@ p

    ( Rit,         [x,_,r] ) → r :@ x
    ( Lef,         [x,l,_] ) → l :@ x
    ( JUni,        [x,y]   ) → x -- Uni is `k`

    ( j,           xs      ) → error ("bad jet arity: " <> show (j, length xs))
  where
    sub ∷ Natural → Natural → Ur
    sub x y | y > x = fast Lef :@ Uni
    sub x y         = fast Rit :@ Nat (x-y)

    go ∷ Ur → [Ur] → Ur
    go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

jetArity ∷ Jet → Positive
jetArity = \case
    Slow n _ _ → n
    Eye        → 1
    Bee        → 3
    Sea        → 3
    Sn n       → n+2
    Bn n       → n+2
    Cn n       → n+2
    Wait n     → n+1
    Fix        → 2
    JNat _     → 2
    JFol        → 1
    JAdd        → 2
    JInc        → 1
    Dec        → 1
    Mul        → 2
    Sub        → 2
    JUni       → 2
    Lef        → 3
    Rit        → 3
    Cas        → 3
    Con        → 3
    Car        → 1
    Cdr        → 1

jetBod ∷ Match → Ur
jetBod = valUr . mBody

jetExp ∷ Match → Ur
jetExp (MkMatch _ n t b) = J (fromIntegral n) :@ valUr t :@ valUr b

--
--  Produces a jetted, church-encoded natural number.
--
church ∷ Natural → Ur
church 0 = S :@ K
church n = S :@ (S:@(K:@S):@K) :@ church (pred n)

churchJet ∷ Natural → Ur
churchJet n = J 2 :@ K :@ church n

waitJet ∷ Positive → Ur
waitJet n = J (n+1) :@ I :@ I

int ∷ Integral a => a -> Int
int = fromIntegral


-- Bulk Variants of B, C, and S ------------------------------------------------

bn, cn, sn ∷ Positive → Ur

bn n = iterate ((B:@        B):@) B !! (int n - 1)
cn n = iterate ((B:@(B:@C):@B):@) C !! (int n - 1)
sn n = iterate ((B:@(B:@S):@B):@) S !! (int n - 1)

bnJet, cnJet, snJet ∷ Positive → Ur

bnJet n = J (n+2) :@ K :@ bn n
cnJet n = J (n+2) :@ K :@ cn n
snJet n = J (n+2) :@ K :@ sn n

j_bn ∷ Check
j_bn = Named "bn" chk
  where
    chk n (MkVal K) (MkVal b)               = Bn <$> go n b
    chk n _         k                       = Nothing
    go 3 B                                  = Just 1
    go n (Fast 1 Bee [B, go(n-1) → Just r]) = Just (r+1)
    go n e                                  = Nothing

j_cn ∷ Check
j_cn = Named "cn" chk
  where
    chk n (MkVal K) (MkVal b)                          = Cn <$> go n b
    chk n _         k                                  = Nothing
    go 3 C                                             = Just 1
    go n (Fast 1 Bee [C, Fast 2 Bee [go(n-1)→Just r]]) = Just (r+1)
    go n _                                             = Nothing

j_sn ∷ Check
j_sn = Named "sn" chk
  where
    chk n (MkVal K) (MkVal b)                          = Sn <$> go n b
    chk n _         k                                  = Nothing
    go 3 S                                             = Just 1
    go n (Fast 1 Bee [s, Fast 2 Bee [go(n-1)→Just r]]) = Just (r+1)
    go n _                                             = Nothing

fast ∷ Jet → Ur
fast j = Fast (fromIntegral $ jetArity j) j []

unMatch ∷ Jet → Ur
unMatch = go
  where
    go ∷ Jet → Ur
    go = \case
        Eye        → mjExp mjI
        Bee        → mjExp mjB
        Sea        → mjExp mjC
        Sn n       → snJet n
        Bn n       → bnJet n
        Cn n       → cnJet n
        Fix        → jetExp j_fix
        JInc       → mjExp mjInc
        JFol       → jetExp j_fol
        Dec        → jetExp j_dec
        Mul        → jetExp j_mul
        Sub        → jetExp j_sub
        JAdd       → mjExp mjAdd
        JUni       → jetExp j_uni
        Lef        → jetExp j_lef
        Rit        → jetExp j_rit
        Cas        → jetExp j_cas
        Con        → jetExp j_con
        Car        → jetExp j_car
        Cdr        → jetExp j_cdr
        JNat n     → churchJet n
        Wait n     → waitJet n
        Slow n t b → J n :@ t :@ b

withoutJets ∷ Ur → Ur
withoutJets = allowJets . unJet

allowJets ∷ UrPoly Void → UrPoly Jet
allowJets (Fast _ j _) = absurd j
allowJets (x :@ y)     = allowJets x :@ allowJets y
allowJets (J n)        = J n
allowJets K            = K
allowJets S            = S
allowJets D            = D

unJet ∷ UrPoly Jet → UrPoly Void
unJet (Fast _ j xs) = unJet (foldl' (:@) (unMatch j) xs)
unJet (x :@ y)      = unJet x :@ unJet y
unJet (J n)         = J n
unJet K             = K
unJet S             = S
unJet D             = D

--
--  Serialize and Uruk expression to a natural.
--
jam ∷ Ur → Ur
jam = Nat . snd . go
  where
    go ∷ Ur → (Int, Natural)
    go = \case
        J 1         → (3, 0)
        K           → (3, 2)
        S           → (3, 4)
        D           → (3, 6)
        J n         → go (jetExpand n)
        Fast _ j xs → go (foldl' (:@) (unMatch j) xs)
        x :@ y      → (rBits, rNum)
          where (xBits, xNum) = go x
                (yBits, yNum) = go y
                rBits = 1 + xBits + yBits
                rNum  = 1 .|. shiftL xNum 1
                          .|. shiftL yNum (1+xBits)


-- Jets with Fixed Bodies and Arities ------------------------------------------

data MonoJet = MonoJet
  { mjFast ∷ Jet
  , mjArgs ∷ Positive
  , mjName ∷ Val
  , mjBody ∷ Val
  , mjExec ∷ [Ur] → Maybe Ur
  }

monoJet ∷ MonoJet → Match
monoJet MonoJet{..} = MkMatch mjFast mjArgs mjName mjBody

mjExp ∷ MonoJet → Ur
mjExp (MonoJet _ n t b _) = J n :@ valUr t :@ valUr b

runMonoJet ∷ MonoJet → [Ur] → Ur
runMonoJet MonoJet{..} xs =
    fromMaybe fallback (mjExec xs)
  where
    fallback = Fast 0 (Slow mjArgs (valUr mjName) (valUr mjBody)) xs


-- Identity  -------------------------------------------------------------------

pattern I = Fast 1 Eye []

{-
    id = \x -> x
-}
mjI ∷ MonoJet
mjI = MonoJet{..}
  where
    mjFast = Eye
    mjArgs = 1
    mjName = MkVal K
    mjExec [x] = Just x
    mjExec _   = error "bad-id"
    mjBody = MkVal (S :@ K :@ K)


-- Flip ------------------------------------------------------------------------

pattern C = Fast 3 Sea []

mjC ∷ MonoJet
mjC = MonoJet{..}
  where
    mjFast = Sea
    mjArgs = 3
    mjName = MkVal K
    mjExec = \case [f,g,x] → Just (f :@ x :@ g)
                   _       → error "bad-C"
    mjBody = MkVal (S :@ (K :@ (S :@ (K :@ (S :@ S :@ (K :@ K))) :@ K)) :@ S)


-- Function Composition --------------------------------------------------------

pattern B = Fast 3 Bee []

mjB ∷ MonoJet
mjB = MonoJet{..}
  where
    mjFast = Bee
    mjArgs = 3
    mjName = MkVal K
    mjExec = \case [f,g,x] → Just (f :@ (g :@ x))
                   _       → error "bad-B"
    mjBody = MkVal (S :@ (K :@ S) :@ K)


-- Increment -------------------------------------------------------------------

pattern Inc = Fast 1 JInc []

{-
    inc = \n -> J2 (\i z -> i (fol n i z))
-}
mjInc ∷ MonoJet
mjInc = MonoJet{..}
  where
    mjFast = JInc
    mjArgs = 1
    mjName = MkVal (Nat 1)
    mjExec [Nat x] = Just $ Nat $ succ x
    mjExec [_]     = Nothing
    mjExec _       = error "bad-inc"
    mjBody = MkVal $
        S :@ (K :@ J2)
          :@ (S :@ (K :@ (S :@ (S :@ (K :@ S) :@ K)))
                :@ Fol)


-- Add -------------------------------------------------------------------------

pattern Add = Fast 2 JAdd []

{-
    add = \x y -> J2 (fol (\i z -> (fol x) i (fol y)))
-}
mjAdd ∷ MonoJet
mjAdd = MonoJet{..}
  where
    mjFast = JAdd
    mjArgs = 2
    mjName = MkVal K
    mjExec [Nat x, Nat y] = Just $ Nat (x+y)
    mjExec _              = Nothing
    mjBody = MkVal $
        S :@ (K :@ (S :@ (K :@ J2)))
          :@ (S :@ (K :@ (S :@ (K :@ Fol)))
                :@ (S :@ (K :@ (S :@ (K :@ (S :@ (K :@ K)))))
                      :@ (S :@ (S :@ (K :@ (S :@ (K :@ (S :@ (K :@ S) :@ K))
                                              :@ S))
                                  :@ Fol)
                            :@ (K :@ (S :@ (K :@ K) :@ Fol)))))
