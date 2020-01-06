{-
    DONE Refactor jet handling.
    DONE Stop storing `Fast` arguments in reverse order.
    DONE Store arity (not arity - 1) in Fast.

    TODO Cleanup jet refactor.

      - Rearrange things so that jet matching, arity, and reduction are
        defined together. The current approach is easy to fuck up and hard
        to test.

    TODO Jet equality for naturals.
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

    TODO Use cords for jet names.
    TODO Make the `Lazy` jet take variable number of arguments.

    TODO Update printer to print cords.

    TODO Hook up front-end to JetComp
    TODO Implement REPL.
    TODO Implement script-runner.
    TODO Define jets in front-end language using template haskell.

      - Right now, jets are just defined as a big pile of S and K.
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
    | JFix
    | JNat Natural
    | JFol
    | JAdd
    | JInc
    | JDec
    | Mul
    | JSub
    | JDed
    | JLaz
    | JUni
    | JLef
    | JRit
    | Cas
    | JCon
    | JCar
    | JCdr
  deriving (Eq, Ord)

data UrPoly j
    = UrPoly j :@ UrPoly j
    | J Positive
    | K
    | S
    | D
    | Lazy (UrPoly j)
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
        Lazy x       → "{" <> show x <> "}"
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
        JFix        → "!"
        Eye        → "i"
        Bee        → "b"
        Sea        → "c"
        Bn n       → "b" <> show n
        Cn n       → "c" <> show n
        Sn n       → "s" <> show n
        JFol        → ","
        JAdd        → "+"
        JInc        → "^"
        JDec        → "_"
        Mul        → "*"
        JSub        → "-"
        JLef        → "L"
        JRit        → "R"
        Cas        → "%"
        JCon        → "&"
        JCar        → "<"
        JCdr        → ">"
        JDed        → "u"
        JLaz        → "|"
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
--  mul =
--  cas = \b l r -> b l r

cas = fast Cas
wait n = fast (Wait n)

l_zer = S :@ K
l_one = S :@ (S:@(K:@S):@K) :@ (S:@K)
l_mul = D :@ D :@ D -- TODO
l_cas = I

e_zer = jetExp j_zer
e_one = jetExp j_one
e_mul = jetExp j_mul
e_cas = jetExp j_cas

j_zer = match (JNat 0) 2 emp l_zer
j_one = match (JNat 1) 2 emp l_one

j_wait ∷ Check
j_wait = Named "wait" chk
  where chk ∷ Positive → JetTag → Val → Maybe Jet
        chk n (MkVal I) (MkVal I) = Just $ Wait $ fromIntegral n
        chk _ _         _         = Nothing

j_mul = match Mul 2 emp l_mul
j_cas = match Cas 3 emp l_cas

dash ∷ Dash
dash = mkDash
    [ simpleEnt (singJet sjI)
    , simpleEnt (singJet sjB)
    , simpleEnt (singJet sjC)
    , simpleEnt (singJet sjFix)
    , simpleEnt (singJet sjFol)
    , simpleEnt (singJet sjInc)
    , simpleEnt (singJet sjAdd)
    , simpleEnt (singJet sjDec)
    , simpleEnt (singJet sjSub)
    , simpleEnt (singJet sjUni)
    , simpleEnt (singJet sjLef)
    , simpleEnt (singJet sjRit)
    , simpleEnt (singJet sjCon)
    , simpleEnt (singJet sjCar)
    , simpleEnt (singJet sjCdr)
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
    Lazy x :@ y             → Just $ x :@ y
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
    (JAdd, xs) → runSingJet sjAdd xs
    (JInc, xs) → runSingJet sjInc xs
    (Bee,  xs) → runSingJet sjB   xs
    (Sea,  xs) → runSingJet sjC   xs
    (JFix, xs) → runSingJet sjFix xs
    (JFol, xs) → runSingJet sjFol xs
    (JDec, xs) → runSingJet sjDec xs
    (JSub, xs) → runSingJet sjSub xs
    (JDed, xs) → runSingJet sjDed xs
    (JLaz, xs) → runSingJet sjLaz xs
    (JUni, xs) → runSingJet sjUni xs
    (JLef, xs) → runSingJet sjLef xs
    (JRit, xs) → runSingJet sjRit xs
    (JCon, xs) → runSingJet sjCon xs
    (JCar, xs) → runSingJet sjCar xs
    (JCdr, xs) → runSingJet sjCdr xs

    ( Slow n t b,  us      ) → go b us
    ( Wait _,      u:us    ) → go u us
    ( Eye,         [x]     ) → x
    ( Bn _,        f:g:xs  ) → f :@ go g xs
    ( Cn _,        f:g:xs  ) → go f xs :@ g
    ( Sn _,        f:g:xs  ) → go f xs :@ go g xs
    ( JNat n,      [x,y]   ) → church n :@ x :@ y

    ( Cas,         [s,l,r] ) → s & \case
        Fast _ JLef [x] → l :@ x
        Fast _ JRit [x] → r :@ x
        _              → l_cas :@ l :@ r

    ( j,           xs      ) → error ("bad jet arity: " <> show (j, length xs))
  where

    go ∷ Ur → [Ur] → Ur
    go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

jetArity ∷ Jet → Positive
jetArity = \case
    Slow n _ _ → n
    Eye        → sjArgs sjI
    Bee        → sjArgs sjB
    Sea        → sjArgs sjC
    Sn n       → n+2
    Bn n       → n+2
    Cn n       → n+2
    Wait n     → n+1
    JFix       → sjArgs sjFix
    JNat _     → 2
    JFol       → sjArgs sjFol
    JAdd       → sjArgs sjAdd
    JInc       → sjArgs sjInc
    JDec       → sjArgs sjDec
    Mul        → 2
    JSub       → sjArgs sjSub
    JDed       → sjArgs sjDed
    JLaz       → sjArgs sjLaz
    JUni       → sjArgs sjUni
    JLef       → sjArgs sjLef
    JRit       → sjArgs sjRit
    Cas        → 3
    JCon       → sjArgs sjCon
    JCar       → sjArgs sjCar
    JCdr       → sjArgs sjCdr

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
        Eye        → sjExp sjI
        Bee        → sjExp sjB
        Sea        → sjExp sjC
        Sn n       → snJet n
        Bn n       → bnJet n
        Cn n       → cnJet n
        JFix       → sjExp sjFix
        JInc       → sjExp sjInc
        JFol       → sjExp sjFol
        JDec        → sjExp sjDec
        Mul        → jetExp j_mul
        JSub        → sjExp sjSub
        JAdd       → sjExp sjAdd
        JDed       → sjExp sjDed
        JLaz       → sjExp sjLaz
        JUni       → sjExp sjUni
        JLef        → sjExp sjLef
        JRit        → sjExp sjRit
        Cas        → jetExp j_cas
        JCon        → sjExp sjCon
        JCar        → sjExp sjCar
        JCdr        → sjExp sjCdr
        JNat n     → churchJet n
        Wait n     → waitJet n
        Slow n t b → J n :@ t :@ b

withoutJets ∷ Ur → Ur
withoutJets = allowJets . unJet

allowJets ∷ UrPoly Void → UrPoly Jet
allowJets (Fast _ j _) = absurd j
allowJets (Lazy x)     = allowJets x
allowJets (x :@ y)     = allowJets x :@ allowJets y
allowJets (J n)        = J n
allowJets K            = K
allowJets S            = S
allowJets D            = D

unJet ∷ UrPoly Jet → UrPoly Void
unJet (Fast _ j xs) = unJet (foldl' (:@) (unMatch j) xs)
unJet (Lazy x)      = Lazy (unJet x)
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
        Lazy x      → go x
        Fast _ j xs → go (foldl' (:@) (unMatch j) xs)
        x :@ y      → (rBits, rNum)
          where (xBits, xNum) = go x
                (yBits, yNum) = go y
                rBits = 1 + xBits + yBits
                rNum  = 1 .|. shiftL xNum 1
                          .|. shiftL yNum (1+xBits)


-- Jets with Fixed Bodies and Arities ------------------------------------------

data SingJet = SingJet
  { sjFast ∷ Jet
  , sjArgs ∷ Positive
  , sjName ∷ Val
  , sjBody ∷ Val
  , sjExec ∷ [Ur] → Maybe Ur
  }

singJet ∷ SingJet → Match
singJet SingJet{..} = MkMatch sjFast sjArgs sjName sjBody

sjExp ∷ SingJet → Ur
sjExp (SingJet _ n t b _) = J n :@ valUr t :@ valUr b

runSingJet ∷ SingJet → [Ur] → Ur
runSingJet SingJet{..} xs =
    fromMaybe fallback (sjExec xs)
  where
    fallback = Fast 0 (Slow sjArgs (valUr sjName) (valUr sjBody)) xs


-- Jets with Varied Bodies and Arities -----------------------------------------

data ManyJet = ManyJet
  { mjName ∷ Val
  , mjArgs ∷ Jet → Positive
  , mjBody ∷ Jet → Ur
  , mjRead ∷ Val → Maybe Jet
  , mjExec ∷ Jet → [Ur] → Maybe Ur
  }

-- manyJet ∷ ManyJet → Match
-- manyJet ManyJet{..} = MkMatch mjFast mjArgs mjName mjBody

-- mjExp ∷ ManyJet → Ur
-- mjExp (ManyJet _ n t b _) = J n :@ valUr t :@ valUr b

-- runManyJet ∷ ManyJet → [Ur] → Ur
-- runManyJet ManyJet{..} xs =
    -- fromMaybe fallback (mjExec xs)
  -- where
    -- fallback = Fast 0 (Slow mjArgs (valUr mjName) (valUr mjBody)) xs



-- Identity  -------------------------------------------------------------------

pattern I = Fast 1 Eye []

{-
    id = \x -> x
-}
sjI ∷ SingJet
sjI = SingJet{..}
  where
    sjFast = Eye
    sjArgs = 1
    sjName = MkVal K
    sjExec [x] = Just x
    sjExec _   = error "bad-id"
    sjBody = MkVal (S :@ K :@ K)


-- Flip ------------------------------------------------------------------------

pattern C = Fast 3 Sea []

sjC ∷ SingJet
sjC = SingJet{..}
  where
    sjFast = Sea
    sjArgs = 3
    sjName = MkVal K
    sjExec = \case [f,g,x] → Just (f :@ x :@ g)
                   _       → error "bad-C"
    sjBody = MkVal (S :@ (K :@ (S :@ (K :@ (S :@ S :@ (K :@ K))) :@ K)) :@ S)


-- Function Composition --------------------------------------------------------

pattern B = Fast 3 Bee []

sjB ∷ SingJet
sjB = SingJet{..}
  where
    sjFast = Bee
    sjArgs = 3
    sjName = MkVal K
    sjExec = \case [f,g,x] → Just (f :@ (g :@ x))
                   _       → error "bad-B"
    sjBody = MkVal (S :@ (K :@ S) :@ K)


-- Crash -----------------------------------------------------------------------

pattern Ded = Fast 1 JDed []

sjDed ∷ SingJet
sjDed = SingJet{..}
  where
    sjFast = JDed
    sjArgs = 1
    sjName = MkVal K
    sjExec _ = error "ded"
    sjBody = MkVal (Fast 1 JFix [I])


-- Lazy Application ------------------------------------------------------------

pattern Laz = Fast 2 JLaz []

sjLaz ∷ SingJet
sjLaz = SingJet{..}
  where
    sjFast = JLaz
    sjArgs = 2
    sjName = MkVal (Nat 15)
    sjExec [x,y] = Just $ Lazy (x :@ y)
    sjExec _     = error "bad-laz"
    sjBody = MkVal I



-- Unit ------------------------------------------------------------------------

pattern Uni = Fast 2 JUni []

sjUni ∷ SingJet
sjUni = SingJet{..}
  where
    sjFast = JUni
    sjArgs = 2
    sjName = MkVal K
    sjExec [x,_] = Just x
    sjExec _     = error "bad-uni"
    sjBody = MkVal K


-- Left ------------------------------------------------------------------------

pattern Lef = Fast 3 JLef []

sjLef ∷ SingJet
sjLef = SingJet{..}
  where
    sjFast = JLef
    sjArgs = 3
    sjName = MkVal (Nat 9)
    sjExec [x,l,_] = Just (l :@ x)
    sjExec _       = error "bad-lef"
    sjBody = MkVal (S :@ (K:@(S:@(K:@(S:@(K:@K))):@(S:@I))) :@ K)


-- Right -----------------------------------------------------------------------

pattern Rit = Fast 3 JRit []

{-
    rit = \x l r -> r x
-}
sjRit ∷ SingJet
sjRit = SingJet{..}
  where
    sjFast = JRit
    sjArgs = 3
    sjName = MkVal (Nat 10)
    sjExec [x,_,r] = Just (r :@ x)
    sjExec _       = error "bad-rit"
    sjBody = MkVal (S :@ (K:@(S:@(K:@K):@(S:@I))) :@ K)


-- Recursion -------------------------------------------------------------------

pattern Fix = Fast 2 JFix []

{-
    fix f x = f (W2 fix f) x
    fix = Z (\fx -> wait2 Jet2 (\f x -> f (fx f) x))
-}
sjFix ∷ SingJet
sjFix = SingJet{..}
  where
    sjFast = JFix
    sjArgs = 2
    sjName = MkVal (Nat 2)
    sjExec [f,x] = Just (f :@ (Fix :@ f) :@ x)
    sjExec _     = error "bad-fix"
    sjBody = MkVal $
        ( (S :@ I)
          :@
          ((W2 :@
            ((S :@ (K :@ ((S :@ (K :@ (J 2 :@ K))) :@ (S :@ I))))
             :@
             ((S :@ W2) :@ I)))
           :@
           ((S :@ (K :@ ((S :@ (K :@ (J 2 :@ K))) :@ (S :@ I))))
            :@
            ((S :@ W2) :@ I))))


-- Nat to Church Natural -------------------------------------------------------

pattern Fol = Fast 1 JFol []

{-
    fol = \n -> n inc zer
-}
sjFol ∷ SingJet
sjFol = SingJet{..}
  where
    sjFast = JFol
    sjArgs = 1
    sjName = MkVal (Nat 2)
    sjExec [Lazy x] = Just (Fol :@ x)
    sjExec [Nat x]  = Just (church x)
    sjExec [_]      = Nothing
    sjExec _        = error "bad-fol"
    sjBody = MkVal $
        S :@ (S :@ I :@ (K :@ (S :@ (S :@ (K :@ S) :@ K))))
          :@ (K :@ (S :@ K))


-- Increment -------------------------------------------------------------------

pattern Inc = Fast 1 JInc []

{-
    inc = \n -> J2 (\i z -> i (fol n i z))
-}
sjInc ∷ SingJet
sjInc = SingJet{..}
  where
    sjFast = JInc
    sjArgs = 1
    sjName = MkVal (Nat 1)
    sjExec [Lazy x] = Just (Inc :@ x)
    sjExec [Nat x]  = Just $ Nat $ succ x
    sjExec [_]      = Nothing
    sjExec _        = error "bad-inc"
    sjBody = MkVal $
        S :@ (K :@ J2)
          :@ (S :@ (K :@ (S :@ (S :@ (K :@ S) :@ K)))
                :@ Fol)


-- Decrement -------------------------------------------------------------------

pattern Dec = Fast 1 JDec []

{-
    dec = \n -> C (n (\x -> C x (\y -> R zer) (\y -> R (inc y))) (L uni))
                  (\g -> L uni)
                  (\g -> R (J2 (fol g)))
-}
sjDec ∷ SingJet
sjDec = SingJet{..}
  where
    sjFast = JDec
    sjArgs = 1
    sjName = MkVal (Nat 3)

    sjExec [Lazy x] = Just (Dec :@ x)
    sjExec [Nat 0]  = Just (Lef :@ Uni)
    sjExec [Nat x]  = Just (Rit :@ Nat (pred x))
    sjExec [_]      = Nothing
    sjExec _        = error "bad-dec"

    sjBody = MkVal $
        S :@ (S :@ (S :@ (K :@ cas)
                      :@ (S :@ (S :@ I
                                  :@ (K :@ (S :@ (S :@ cas
                                                    :@ (K:@(K:@(Rit:@ch_zero))))
                                              :@ (K:@(S:@(K:@Rit):@ch_succ)))))
                            :@ (K :@ (Lef :@ Uni))))
                :@ (K :@ (K :@ (Lef :@ Uni))))
          :@ (K:@(S:@(K:@Rit):@(S:@(K:@J2):@Fol)))


-- Add -------------------------------------------------------------------------

pattern Add = Fast 2 JAdd []

{-
    add = \x y -> J2 (fol (\i z -> (fol x) i (fol y)))
-}
sjAdd ∷ SingJet
sjAdd = SingJet{..}
  where
    sjFast = JAdd
    sjArgs = 2
    sjName = MkVal K
    sjExec [Lazy x, y     ] = Just $ (Add :@ x :@ y)
    sjExec [x,      Lazy y] = Just $ (Add :@ x :@ y)
    sjExec [Nat  x, Nat y ] = Just $ Nat (x+y)
    sjExec _              = Nothing
    sjBody = MkVal $
        S :@ (K :@ (S :@ (K :@ J2)))
          :@ (S :@ (K :@ (S :@ (K :@ Fol)))
                :@ (S :@ (K :@ (S :@ (K :@ (S :@ (K :@ K)))))
                      :@ (S :@ (S :@ (K :@ (S :@ (K :@ (S :@ (K :@ S) :@ K))
                                              :@ S))
                                  :@ Fol)
                            :@ (K :@ (S :@ (K :@ K) :@ Fol)))))


-- Subtract --------------------------------------------------------------------

pattern Sub = Fast 2 JSub []

{-
    sub = \x y -> y (\z -> CAS z LEF DEC) (RIT x)
-}
sjSub ∷ SingJet
sjSub = SingJet{..}
  where
    sjFast = JSub
    sjArgs = 2
    sjName = MkVal (Nat 4)
    sjExec [Lazy x, y     ] = Just $ Sub :@ x :@ y
    sjExec [x,      Lazy y] = Just $ Sub :@ x :@ y
    sjExec [Nat x,  Nat y ] = Just $ sub x y
    sjExec [_,      _     ] = Nothing
    sjExec _                = error "bad-sub"
    sjBody = MkVal $
        S :@ (K :@ (S:@(S:@I:@(K:@(S:@(S:@cas:@(K:@Lef)):@(K:@Dec))))))
          :@ (S :@ (K :@ K) :@ Rit)

    sub ∷ Natural → Natural → Ur
    sub x y | y > x = fast JLef :@ Uni
    sub x y         = fast JRit :@ Nat (x-y)


-- Cons ------------------------------------------------------------------------

pattern Con = Fast 3 JCon []

{-
    con = \x y f -> f x y
-}
sjCon ∷ SingJet
sjCon = SingJet{..}
  where
    sjFast = JCon
    sjArgs = 3
    sjName = MkVal (Nat 12)
    sjExec [x,y,f] = Just (f :@ x :@ y)
    sjExec _       = error "bad-con"
    sjBody = MkVal $
        S :@ (K:@(S:@(K:@(S:@(K:@(S:@(K:@(S:@S:@(K:@K))):@K)):@S)):@(S:@I)))
          :@ K


-- Car -------------------------------------------------------------------------

pattern Car = Fast 1 JCar []

{-
    car = \p -> p (\x y -> x)
-}
sjCar ∷ SingJet
sjCar = SingJet{..}
  where
    sjFast = JCar
    sjArgs = 1
    sjName = MkVal (Nat 13)
    sjExec [Lazy x]            = Just (Car :@ x)
    sjExec [Fast _ JCon [x,_]] = Just x
    sjExec [_]                 = Nothing
    sjExec _                   = error "bad-car"
    sjBody = MkVal $
        S :@ I :@ (K :@ K)


-- Cdr -------------------------------------------------------------------------

pattern Cdr = Fast 1 JCdr []

{-
    cdr = \p -> b (\x y -> y)
-}
sjCdr ∷ SingJet
sjCdr = SingJet{..}
  where
    sjFast = JCdr
    sjArgs = 1
    sjName = MkVal (Nat 14)
    sjExec [Lazy x]            = Just (Cdr :@ x)
    sjExec [Fast _ JCon [_,y]] = Just y
    sjExec [_]                 = Nothing
    sjExec _                   = error "bad-cdr"
    sjBody = MkVal $
        S :@ I :@ (K :@ (S :@ K))


-- Natural Numbers -------------------------------------------------------------

j_nat ∷ Check
j_nat = Named "nat" chk
  where chk ∷ Positive → JetTag → Val → Maybe Jet
        chk 2 (MkVal K) u = JNat <$> unChurch (valUr u)
        chk n t         b = Nothing
