{-
    DONE Refactor jet handling.
    TODO Cleanup jet refactor.
    TODO Normalization without jets (all jets implemented with their code)
    TODO Turn all matched jets into unmatched jets.
    TODO Rematch all jets.
    TODO Write tests (show that jets matching doesn't affect result)

    This is an extremely simple (and very, very slow) Uruk evaluator.

    It evaluates Uruk by applying reduction rules until we reach a normal
    form, and printing the expression after every reduction.

    This is an extremely dumb evaluation strategy, but it's dead simple
    and closely matches the spec. It's also a useful debugging tool,
    since it shows each reduction step.
-}

module Uruk.JetDemo where

import ClassyPrelude
import Data.Bits
import Data.Void

import Data.Function ((&))
import Data.List     ((!!), iterate)
import GHC.Natural   (Natural)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

pattern I = Fast 0 Eye []
pattern B = Fast 2 Bee []
pattern C = Fast 2 Sea []

data Jet
    = Slow Natural Ur Ur -- arity, tag, body
    | Eye
    | Bee
    | Sea
    | Sn Natural
    | Bn Natural
    | Cn Natural
    | Wait Natural
    | Fix
    | Nat Natural
    | Fol
    | Add
    | Inc
    | Dec
    | Mul
    | Sub
    | Uni
    | Lef
    | Rit
    | Cas
    | Con
    | Car
    | Cdr
  deriving (Eq, Ord)

data UrPoly j
    = UrPoly j :@ UrPoly j
    | J Natural
    | K
    | S
    | D
    | Fast Natural j [UrPoly j]
  deriving (Eq, Ord)

type Ur = UrPoly Jet

jetExpand ∷ Natural → Ur
jetExpand = go
  where go = \case { 0 → J 0; n → go (pred n) :@ J 0 }

unSlow ∷ Ur → [Ur] → Ur
unSlow u = go u . reverse
  where go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

instance Show Ur where
    show = \case
        x :@ y      → "(" <> intercalate " " (show <$> flatten x [y]) <> ")"
        J n          → replicate (fromIntegral (succ n)) 'j'
        K            → "k"
        S            → "s"
        D            → "d"
        Fast _ j us  → fast j us
      where
        flatten (x :@ y) acc = flatten x (y : acc)
        flatten x        acc = (x : acc)

        fast ∷ Jet → [Ur] → String
        fast j us = "[" <> intercalate " " (show j : (show <$> reverse us))
                          <> "]"

instance Show Jet where
    show = \case
        Slow n t b → show (J n :@ t :@ b)

        Nat n       → "#" <> show n

        Fix         → "fix"

        Eye         → "i"
        Bee         → "b"
        Sea         → "c"

        Bn n      → "b" <> show n
        Cn n      → "c" <> show n
        Sn n      → "s" <> show n

        Fol         → "fol"
        Add         → "add"
        Inc         → "inc"
        Dec         → "dec"
        Mul         → "mul"
        Sub         → "sub"

        Lef         → "lef"
        Rit         → "rit"
        Cas         → "cas"

        Con         → "con"
        Car         → "car"
        Cdr         → "cdr"
        Uni         → "uni"

        Wait n      → "wait-" <> show n


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
    , mArgs ∷ Word
    , mName ∷ Val
    , mBody ∷ Val
    }
  deriving (Show)

match ∷ Jet → Natural → Ur → Ur → Match
match j n t b = MkMatch j (fromIntegral (n-1)) (urVal t) (urVal b)

type Check = Named (Word → JetTag → Val → Maybe Jet)

type DashEntry = Either Match Check

type JetTag  = Val
type Matches = Map (Word, JetTag, Val) Jet

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

dashLookup ∷ Natural → Ur → Ur → Maybe Jet
dashLookup n t b = findMatch <|> passCheck
  where
    (nw,tv,bv) = (fromIntegral n, urVal t, urVal b)
    Dash ms cs = dash
    findMatch  = lookup (nw, tv, bv) ms
    passCheck  = headMay (mapMaybe (\(Named n ok) -> ok nw tv bv) cs)


-- Jet Dashboard ---------------------------------------------------------------

unChurch ∷ Ur → Maybe Natural
unChurch = \case
    S :@ K                   -> Just 0
    S :@ (S:@(K:@S):@K) :@ n -> succ <$> unChurch n
    _                        -> Nothing

emp ∷ Ur
emp = K

pattern J1 = J 0 :@ K
pattern J2 = J 1 :@ K
pattern J3 = J 2 :@ K
pattern J4 = J 3 :@ K

pattern W2 = Fast 2 (Wait 2) []

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

l_i = S :@ K :@ K
j_i = match Eye 1 emp l_i
e_i = jetExp j_i

l_b = S :@ (K :@ S) :@ K
j_b = match Bee 3 emp l_b
e_b = jetExp j_b

l_c = S :@ (K :@ (S :@ (K :@ (S :@ S :@ (K :@ K))) :@ K)) :@ S
j_c = match Sea 3 emp l_c
e_c = jetExp j_c

ch_succ = S :@ (S :@ (K :@ S) :@ K)
ch_zero = S :@ K

--  zer = \i z -> z
--  suc = \n -> \i z -> i (n i z)
--  one = inc zer
--  fol = \n -> n inc zer
--  inc = \n -> J2 (\i z -> i (fol n i z))
--  add = \x y -> J2 (fol (\i z -> (fol x) i (fol y)))
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
            ((S :@ (K :@ ((S :@ (K :@ (J 1 :@ K))) :@ (S :@ I))))
             :@
             ((S :@ W2) :@ I)))
           :@
           ((S :@ (K :@ ((S :@ (K :@ (J 1 :@ K))) :@ (S :@ I))))
            :@
            ((S :@ W2) :@ I))))

j_fix = match Fix 2 emp l_fix
fix = jetExp j_fix

rit = fast Rit
dec = fast Dec
fol = fast Fol
lef = fast Lef
cas = fast Cas
uni = fast Uni
wait n = fast (Wait n)

l_zer = S :@ K
l_one = S :@ (S:@(K:@S):@K) :@ (S:@K)
l_fol = S :@ (S:@I:@(K:@(S:@(S:@(K:@S):@K)))) :@ (K:@(S:@K))
l_inc = S :@ (K:@J2) :@ (S:@(K:@(S:@(S:@(K:@S):@K))) :@ l_fol)
l_dec = S:@(S:@(S:@(K:@cas):@(S:@(S:@I:@(K:@(S:@(S:@cas:@(K:@(K:@(rit:@ch_zero)))):@(K:@(S:@(K:@rit):@ch_succ))))):@(K:@(lef:@uni)))):@(K:@(K:@(lef :@ uni)))):@(K:@(S:@(K:@rit):@(S:@(K:@J2):@fol)))
l_mul = D :@ D :@ D -- TODO
l_sub = S:@(K:@(S:@(S:@I:@(K:@(S:@(S:@cas:@(K:@lef)):@(K:@dec)))))):@(S:@(K:@K):@rit)
l_add = S :@ (K:@(S:@(K:@J2))) :@ (S:@(K:@(S:@(K:@l_fol))):@(S:@(K:@(S:@(K:@(S:@(K:@K))))):@(S:@(S:@(K:@(S:@(K:@(S:@(K:@S):@K)):@S)):@l_fol):@(K:@(S:@(K:@K):@l_fol)))))
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
e_inc = jetExp j_inc
e_add = jetExp j_add
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

j_zer = match (Nat 0) 2 emp l_zer
j_one = match (Nat 1) 2 emp l_one

j_nat ∷ Check
j_nat = Named "nat" chk
  where chk ∷ Word → JetTag → Val → Maybe Jet
        chk 1 (MkVal K) u = Nat <$> unChurch (valUr u)
        chk n t         b = Nothing

j_wait ∷ Check
j_wait = Named "wait" chk
  where chk ∷ Word → JetTag → Val → Maybe Jet
        chk n (MkVal I) (MkVal I) = Just $ Wait $ fromIntegral n
        chk _ _         _         = Nothing

j_fol = match Fol 1 emp l_fol
j_inc = match Inc 1 emp l_inc
j_dec = match Dec 1 emp l_dec
j_mul = match Mul 2 emp l_mul
j_sub = match Sub 2 emp l_sub
j_add = match Add 2 emp l_add
j_uni = match Uni 2 emp l_uni
j_lef = match Lef 3 emp l_lef
j_rit = match Rit 3 emp l_rit
j_cas = match Cas 3 emp l_cas
j_con = match Con 3 emp l_con
j_car = match Car 1 emp l_car
j_cdr = match Cdr 1 emp l_cdr

dash ∷ Dash
dash = mkDash
    [ simpleEnt j_i
    , simpleEnt j_b
    , simpleEnt j_c
    , simpleEnt j_fix
    , simpleEnt j_fol
    , simpleEnt j_inc
    , simpleEnt j_add
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
    J n :@ J 0              → Just $ J (succ n)
    J n :@ t :@ b           → Just $ Fast n (match n t b) []
    Fast 0 u us :@ x        → Just $ runJet u (reverse (x:us))
    Fast n u us :@ x        → Just $ Fast (pred n) u (x:us)
    _                       → Nothing
  where
    match ∷ Natural → Ur → Ur → Jet
    -- ch n t b = fromMaybe (Slow n t b) $ dashLookup n t b
    match n t b = fromMaybe (error $ show (n,t,b)) $ dashLookup n t b

runJet ∷ Jet → [Ur] → Ur
runJet = curry \case
    ( Slow n t b,  us      ) → go b us
    ( Wait _,      u:us    ) → go u us
    ( Eye,         [x]     ) → x
    ( Bee,         [f,g,x] ) → f :@ (g :@ x)
    ( Sea,         [f,g,x] ) → f :@ x :@ g
    ( Bn _,        f:g:xs  ) → f :@ go g xs
    ( Cn _,        f:g:xs  ) → go f xs :@ g
    ( Sn _,        f:g:xs  ) → go f xs :@ go g xs
    ( Fix,         [f,x]   ) → f :@ (fast Fix :@ f) :@ x
    ( Nat n,       [x,y]   ) → church n :@ x :@ y

    ( Fol,         [x]     ) → x & \case
        Fast _ (Nat x) [] → church x
        x                 → l_fol :@ x

    ( Inc,         [x]     ) → x & \case
        Fast _ (Nat n) [] → fast $ Nat (succ n)
        x                 → l_inc :@ x

    ( Dec,         [x]     ) → x & \case
        Fast _ (Nat 0) [] → lef :@ uni
        Fast _ (Nat n) [] → rit :@ fast (Nat (pred n))
        x                 → l_rit :@ x

    ( Add,         [x,y]   ) → (x,y) & \case
        ( Fast _ (Nat x) [], Fast _ (Nat y) [] ) → fast $ Nat (x+y)
        ( x,                 y                 ) → l_add :@ x :@ y

    ( Sub,         [x,y]   ) → (x,y) & \case
        ( Fast _ (Nat x) [], Fast _ (Nat y) [] ) → sub x y
        ( _,                 _                 ) → l_sub :@ x :@ y

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
    ( Uni,         [x,y]   ) → x -- Uni is `k`

    ( j,           xs      ) → error ("bad jet arity: " <> show (j, length xs))
  where
    sub ∷ Natural → Natural → Ur
    sub x y | y > x = fast Lef :@ fast Uni
    sub x y         = fast Rit :@ fast (Nat (x-y))

    go ∷ Ur → [Ur] → Ur
    go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

jetArity ∷ Jet → Natural
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
    Nat _      → 2
    Fol        → 1
    Add        → 2
    Inc        → 1
    Dec        → 1
    Mul        → 2
    Sub        → 2
    Uni        → 2
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
churchJet n = J 1 :@ K :@ church n

waitJet ∷ Natural → Ur
waitJet n = J n :@ I :@ I

int ∷ Integral a => a -> Int
int = fromIntegral


-- Bulk Variants of B, C, and S ------------------------------------------------

bn, cn, sn ∷ Natural → Ur

bn n = iterate ((B:@        B):@) B !! (int n - 1)
cn n = iterate ((B:@(B:@C):@B):@) C !! (int n - 1)
sn n = iterate ((B:@(B:@S):@B):@) S !! (int n - 1)

bnJet, cnJet, snJet ∷ Natural → Ur

bnJet 0 = error "impossible BLnjet"
bnJet n = J (n+1) :@ K :@ bn n
cnJet 0 = error "Impossible Cn Jet"
cnJet n = J (n+1) :@ K :@ cn n
snJet 0 = error "impossible Sn jet"
snJet n = J (n+1) :@ K :@ sn n

j_bn ∷ Check
j_bn = Named "bn" chk
  where
    chk n (MkVal K) (MkVal b)               = Bn <$> go n b
    chk n _         k                       = Nothing
    go 2 B                                  = Just 1
    go n (Fast 0 Bee [go(n-1) → Just r, b]) = Just (r+1)
    go n e                                  = Nothing

j_cn ∷ Check
j_cn = Named "cn" chk
  where
    chk n (MkVal K) (MkVal b)                          = Cn <$> go n b
    chk n _         k                                  = Nothing
    go 2 C                                             = Just 1
    go n (Fast 0 Bee [Fast 1 Bee [go(n-1)→Just r], C]) = Just (r+1)
    go n _                                             = Nothing

j_sn ∷ Check
j_sn = Named "sn" chk
  where
    chk n (MkVal K) (MkVal b)                          = Sn <$> go n b
    chk n _         k                                  = Nothing
    go 2 S                                             = Just 1
    go n (Fast 0 Bee [Fast 1 Bee [go(n-1)→Just r], S]) = Just (r+1)
    go n _                                             = Nothing

fast ∷ Jet → Ur
fast j = Fast (jetArity j - 1) j []

unMatch ∷ Jet → Ur
unMatch = go
  where
    go ∷ Jet → Ur
    go = \case
        Eye        → jetExp j_i
        Bee        → jetExp j_b
        Sea        → jetExp j_c
        Sn n       → snJet n
        Bn n       → bnJet n
        Cn n       → cnJet n
        Fix        → jetExp j_fix
        Inc        → jetExp j_inc
        Fol        → jetExp j_fol
        Dec        → jetExp j_dec
        Mul        → jetExp j_mul
        Sub        → jetExp j_sub
        Add        → jetExp j_add
        Uni        → jetExp j_uni
        Lef        → jetExp j_lef
        Rit        → jetExp j_rit
        Cas        → jetExp j_cas
        Con        → jetExp j_con
        Car        → jetExp j_car
        Cdr        → jetExp j_cdr
        Nat n      → churchJet n
        Wait n     → waitJet n
        Slow n t b → J n :@ t :@ b

unFast ∷ Ur → [Ur] → Ur
unFast x xs = foldl' (:@) x xs

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
unJet (Fast _ j xs) = unJet (foldr (:@) (unMatch j) xs)
unJet (x :@ y)      = unJet x :@ unJet y
unJet (J n)         = J n
unJet K             = K
unJet S             = S
unJet D             = D

--
--  Serialize and Uruk expression to a natural.
--
jam ∷ Ur → Ur
jam = fast . Nat . snd . go
  where
    go ∷ Ur → (Int, Natural)
    go = \case
        J 0         → (3, 0)
        K           → (3, 2)
        S           → (3, 4)
        D           → (3, 6)
        J n         → go (jetExpand n)
        Fast _ j xs → go (unMatch j)
        x :@ y      → (rBits, rNum)
          where (xBits, xNum) = go x
                (yBits, yNum) = go y
                rBits = 1 + xBits + yBits
                rNum  = 1 .|. shiftL xNum 1
                          .|. shiftL yNum (1+xBits)
