{-
    DONE Refactor jet handling.
    DONE Stop storing `Fast` arguments in reverse order.
    DONE Store arity (not arity - 1) in Fast.

    DONE Simplify Nat jets

      - Write a `pak` function that converts a church encoded natural
        into a jetted church encoded natural. In jets that operate on
        nats, don't bother keeping everything in the right shape. Simply
        do the operation and then call the nat jet. The nat jet should
        execute the natural number against l_zero and l_succ, and jet
        the result.

    DONE Jet equality for naturals.

    DONE Implement Atom Multiplication

    DONE Implement Bool Jets: True, False, If

    DONE Implement Fast Decrement

        ```
        fastDec 0 = 0
        fastDec n = n-1
        ```

    DONE Compile Ackermann to Uruk and run it.


    -- Flesh out Jets ----------------------------------------------------------

    TODO Cleanup jet refactor.

      - Rearrange things so that jet matching, arity, and reduction are
        defined together. The current approach is easy to fuck up and hard
        to test.


    -- Testing -----------------------------------------------------------------

    TODO Normalization without jets (all jets implemented with their code)

      - Generalize `reduce` and `normalize` to support unjetted reduction.

    TODO Write tests (show that jets matching doesn't affect result)

      - unmatch jets; match jets == match jets

      - These should all produce the same result:

        - normalize with jets
        - unmatch jets, normalize with jets
        - unmatch jets, normalize without jets, match jets


    -- Front End ---------------------------------------------------------------

    TODO Use cords for jet names.

        - Implement Text -> Atom
        - Update nat printer to print strings where possible.

    TODO Hook up front-end to JetComp
    TODO Implement REPL.
    TODO Implement script-runner.


    -- High-Level Jet Definition -----------------------------------------------

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

data Jet
    = Slow !Positive !Ur !Ur -- unmatched jet: arity, tag, body
    | Eye
    | Bee
    | Sea
    | Sn !Positive
    | Bn !Positive
    | Cn !Positive
    | JSeq
    | Wait !Natural
    | JFix
    | JNat !Natural
    | JBol !Bool
    | JIff
    | JPak
    | JZer
    | JEql
    | JAdd
    | JInc
    | JDec
    | JFec
    | JMul
    | JSub
    | JDed
    | JUni
    | JLef
    | JRit
    | JCas
    | JCon
    | JCar
    | JCdr
  deriving (Eq, Ord) -- , Show)

data UrPoly j
    = UrPoly j :@ UrPoly j
    | J Positive
    | K
    | S
    | D
    | Free !Natural
    | Fast !Natural j [UrPoly j]
  deriving (Eq, Ord) -- , Show)

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
        Free n       → "{" <> show n <> "}"
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
        JPak       → "p"
        JFix       → "!"
        Eye        → "i"
        Bee        → "b"
        Sea        → "c"
        Bn n       → "b" <> show n
        Cn n       → "c" <> show n
        Sn n       → "s" <> show n
        JSeq       → "Q"
        JBol True  → "Y"
        JBol False → "N"
        JIff       → "?"
        JAdd       → "+"
        JEql       → "="
        JZer       → "{=0}"
        JInc       → "^"
        JDec       → "_"
        JFec       → "__"
        JMul       → "*"
        JSub       → "-"
        JLef       → "L"
        JRit       → "R"
        JCas       → "%"
        JCon       → "&"
        JCar       → "<"
        JCdr       → ">"
        JDed       → "u"
        JUni       → "~"
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

dash ∷ Dash
dash = mkDash
    [ simpleEnt (singJet sjI)
    , simpleEnt (singJet sjB)
    , simpleEnt (singJet sjC)
    , simpleEnt (singJet sjSeq)
    , simpleEnt (singJet sjFix)
    , simpleEnt (singJet sjInc)
    , simpleEnt (singJet sjAdd)
    , simpleEnt (singJet sjMul)
    , simpleEnt (singJet sjDec)
    , simpleEnt (singJet sjFec)
    , simpleEnt (singJet sjSub)
    , simpleEnt (singJet sjUni)
    , simpleEnt (singJet sjLef)
    , simpleEnt (singJet sjRit)
    , simpleEnt (singJet sjCon)
    , simpleEnt (singJet sjCar)
    , simpleEnt (singJet sjCdr)
    , simpleEnt (singJet sjZer)
    , simpleEnt (singJet sjEql)
    , simpleEnt (singJet sjCas)
    , simpleEnt (singJet sjPak)
    , predikEnt j_nat
    , predikEnt j_bol
    , predikEnt j_cn
    , predikEnt j_sn
    , predikEnt j_bn
    , predikEnt j_wait
    ]


-- Evaluation ------------------------------------------------------------------

--
--  Repeatedly perform reductions until the input is fully normalized.
--
normalizeI ∷ Ur → IO Ur
normalizeI ur = do
    putStrLn (">>  " <> tshow ur)
    void getLine
    reduce ur & \case
        Nothing -> pure ur
        Just ru -> normalizeI ru

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
    -- ch n t b = Slow n t b
    -- ch n t b = fromMaybe (Slow n t b) $ dashLookup n t b
    match n t b = fromMaybe (error $ show (n,t,b)) $ dashLookup n t b


runJet ∷ Jet → [Ur] → Ur
runJet = curry \case
    (JPak, xs) → runSingJet sjPak xs
    (JAdd, xs) → runSingJet sjAdd xs
    (JMul, xs) → runSingJet sjMul xs
    (JInc, xs) → runSingJet sjInc xs
    (JEql, xs) → runSingJet sjEql xs
    (JZer, xs) → runSingJet sjZer xs
    (Eye,  xs) → runSingJet sjI   xs
    (Bee,  xs) → runSingJet sjB   xs
    (Sea,  xs) → runSingJet sjC   xs
    (JIff, xs) → runSingJet sjIff xs
    (JSeq, xs) → runSingJet sjSeq xs
    (JFix, xs) → runSingJet sjFix xs
    (JDec, xs) → runSingJet sjDec xs
    (JFec, xs) → runSingJet sjFec xs
    (JSub, xs) → runSingJet sjSub xs
    (JDed, xs) → runSingJet sjDed xs
    (JUni, xs) → runSingJet sjUni xs
    (JCas, xs) → runSingJet sjCas xs
    (JLef, xs) → runSingJet sjLef xs
    (JRit, xs) → runSingJet sjRit xs
    (JCon, xs) → runSingJet sjCon xs
    (JCar, xs) → runSingJet sjCar xs
    (JCdr, xs) → runSingJet sjCdr xs

    ( Slow n t b,  us      ) → go b us
    ( Wait _,      u:us    ) → go u us
    ( Bn _,        f:g:xs  ) → f :@ go g xs
    ( Cn _,        f:g:xs  ) → go f xs :@ g
    ( Sn _,        f:g:xs  ) → go f xs :@ go g xs
    ( JNat n,      [x,y]   ) → applyNat n x y
    ( JBol c,      [x,y]   ) → if c then x else y

    ( j,           xs      ) → error ("bad jet arity: " <> show (j, length xs))
  where

    applyNat 0 i z = z
    applyNat n i z = i :@ (applyNat (pred n) i z)

    go ∷ Ur → [Ur] → Ur
    go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

unMatch ∷ Jet → Ur
unMatch = go
  where
    go ∷ Jet → Ur
    go = \case
        Eye        → sjExp sjI
        Bee        → sjExp sjB
        Sea        → sjExp sjC
        JSeq       → sjExp sjSeq
        JFix       → sjExp sjFix
        JIff       → sjExp sjIff
        JInc       → sjExp sjInc
        JZer       → sjExp sjZer
        JEql       → sjExp sjEql
        JDec       → sjExp sjDec
        JFec       → sjExp sjFec
        JMul       → sjExp sjMul
        JSub       → sjExp sjSub
        JAdd       → sjExp sjAdd
        JDed       → sjExp sjDed
        JUni       → sjExp sjUni
        JLef       → sjExp sjLef
        JRit       → sjExp sjRit
        JCas       → sjExp sjCas
        JCon       → sjExp sjCon
        JCar       → sjExp sjCar
        JCdr       → sjExp sjCdr

        JNat n     → natJet n
        JBol b     → boolJet b
        JPak       → sjExp sjPak
        Wait n     → waitJet n
        Sn n       → snJet n
        Bn n       → bnJet n
        Cn n       → cnJet n

        Slow n t b → J n :@ t :@ b

withoutJets ∷ Ur → Ur
withoutJets = allowJets . unJet

allowJets ∷ UrPoly Void → UrPoly Jet
allowJets (Fast _ j _) = absurd j
allowJets (Free n)     = Free n
allowJets (x :@ y)     = allowJets x :@ allowJets y
allowJets (J n)        = J n
allowJets K            = K
allowJets S            = S
allowJets D            = D

unJet ∷ UrPoly Jet → UrPoly Void
unJet (Fast _ j xs) = unJet (foldl' (:@) (unMatch j) xs)
unJet (Free n)      = Free n
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
        Free _      → error "expression contains free variable"
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

-- data ManyJet = ManyJet
    -- { mjName ∷ Val
    -- , mjArgs ∷ Jet → Positive
    -- , mjBody ∷ Jet → Ur
    -- , mjRead ∷ Val → Maybe Jet
    -- , mjExec ∷ Jet → [Ur] → Maybe Ur
    -- }

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


-- Bulk S Combinator -----------------------------------------------------------

sn ∷ Positive → Ur
sn n = iterate ((B:@(B:@S):@B):@) S !! (fromIntegral n - 1)

snJet ∷ Positive → Ur
snJet n = J (n+2) :@ K :@ sn n

j_sn ∷ Check
j_sn = Named "sn" chk
  where
    chk n (MkVal K) (MkVal b)                          = Sn <$> go n b
    chk n _         k                                  = Nothing
    go 3 S                                             = Just 1
    go n (Fast 1 Bee [s, Fast 2 Bee [go(n-1)→Just r]]) = Just (r+1)
    go n _                                             = Nothing


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


-- Bulk Flip -------------------------------------------------------------------

cn ∷ Positive → Ur
cn n = iterate ((B:@(B:@C):@B):@) C !! (fromIntegral n - 1)

cnJet ∷ Positive → Ur
cnJet n = J (n+2) :@ K :@ cn n

j_cn ∷ Check
j_cn = Named "cn" chk
  where
    chk n (MkVal K) (MkVal b)                          = Cn <$> go n b
    chk n _         k                                  = Nothing
    go 3 C                                             = Just 1
    go n (Fast 1 Bee [C, Fast 2 Bee [go(n-1)→Just r]]) = Just (r+1)
    go n _                                             = Nothing


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


-- Bulk Composition ------------------------------------------------------------

bnJet ∷ Positive → Ur
bnJet n = J (n+2) :@ K :@ bn n

bn ∷ Positive → Ur
bn n = iterate ((B:@B):@) B !! (fromIntegral n - 1)

j_bn ∷ Check
j_bn = Named "bn" chk
  where
    chk n (MkVal K) (MkVal b)               = Bn <$> go n b
    chk n _         k                       = Nothing
    go 3 B                                  = Just 1
    go n (Fast 1 Bee [B, go(n-1) → Just r]) = Just (r+1)
    go n e                                  = Nothing


-- Seq -------------------------------------------------------------------------

pattern Seq = Fast 2 JSeq []

sjSeq ∷ SingJet
sjSeq = SingJet{..}
  where
    sjFast = JSeq
    sjArgs = 2
    sjName = MkVal (Nat 17)
    sjExec = \case [x,y] → Just y
                   _     → error "bad-seq"
    sjBody = MkVal (S :@ K)


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


-- Boolean ---------------------------------------------------------------------

pattern Bol n = Fast 2 (JBol n) []
pattern No = Bol False
pattern Ya = Bol True

j_bol ∷ Check
j_bol = Named "bol" chk
  where chk ∷ Positive → JetTag → Val → Maybe Jet
        chk 2 (MkVal S) (MkVal K)        = Just (JBol True)
        chk 2 (MkVal S) (MkVal (S :@ K)) = Just (JBol False)
        chk n t         b                = Nothing

encBool ∷ Bool → Ur
encBool True  = K
encBool False = S :@ K

boolJet ∷ Bool → Ur
boolJet b = J 2 :@ S :@ encBool b


-- If --------------------------------------------------------------------------

pattern Iff = Fast 3 JIff []

{-
    cas = \b l r -> b l r
-}
sjIff ∷ SingJet
sjIff = SingJet{..}
  where
    sjFast = JIff
    sjArgs = 3
    sjName = MkVal (Nat 16)
    sjExec [Bol c,t,f]       = Just (if c then t else f)
    sjExec [_,_,_]           = Nothing
    sjExec _                 = error "bad-iff"
    sjBody = MkVal I


-- Case ------------------------------------------------------------------------

pattern Cas = Fast 3 JCas []

{-
    cas = \b l r -> b l r
-}
sjCas ∷ SingJet
sjCas = SingJet{..}
  where
    sjFast = JCas
    sjArgs = 3
    sjName = MkVal (Nat 15)
    sjExec [s,l,r] = run s l r
    sjExec _       = error "bad-lef"
    sjBody = MkVal I

    run (Fast _ JLef [x]) l r = Just (l :@ x)
    run (Fast _ JRit [x]) l r = Just (r :@ x)
    run _                 l r = Nothing


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

-- Z = \f -> (\x -> f (\v -> W2 x x v)) (\x -> f (\v -> W2 x x v))
pattern Z = S :@ (S:@(S:@(K:@S):@K):@(K:@(S:@W2:@I)))
              :@ (S:@(S:@(K:@S):@K):@(K:@(S:@W2:@I)))

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
    sjName = MkVal K
    -- xec [f,x] = Nothing
    sjExec [f,x] = Just (f :@ (Fix :@ f) :@ x)
    sjExec _     = error "bad-fix"
    sjBody = MkVal $
        S :@ I
          :@ Fast 1 (Wait 2)
            [ (S :@ (K :@ ((S :@ (K :@ (J 2 :@ K))) :@ (S :@ I))))
                :@ ((S :@ Fast 3 (Wait 2) []) :@ I)
            , (S :@ (K :@ ((S :@ (K :@ (J 2 :@ K))) :@ (S :@ I))))
                :@ ((S :@ Fast 3 (Wait 2) []) :@ I)
            ]


-- Increment -------------------------------------------------------------------

pattern Inc = Fast 1 JInc []

{-
    inc = \n -> Pak (\i z -> i (n i z))
-}
sjInc ∷ SingJet
sjInc = SingJet{..}
  where
    sjFast = JInc
    sjArgs = 1
    sjName = MkVal (Nat 1)
    sjExec [Nat x]       = Just $ Nat $ succ x
    sjExec [_]           = Nothing
    sjExec _             = error "bad-inc"
    sjBody = MkVal $
        S :@ (K :@ Pak) :@ (S :@ (S :@ (K :@ S) :@ K))


-- Decrement -------------------------------------------------------------------

pattern Dec = Fast 1 JDec []

{-
    dec = \n -> n (\x -> C x (\y -> Rit 0) (\y -> Rit (Inc y)))
                  (Lef Uni)
-}
sjDec ∷ SingJet
sjDec = SingJet{..}
  where
    sjFast = JDec
    sjArgs = 1
    sjName = MkVal (Nat 3)

    sjExec [Nat 0]       = Just (Lef :@ Uni)
    sjExec [Nat x]       = Just (Rit :@ Nat (pred x))
    sjExec [_]           = Nothing
    sjExec _             = error "bad-dec"

    sjBody = MkVal $
        S :@ (S :@ I
                :@ (K :@ (S :@ (S :@ Cas :@ (K :@ (K :@ Fast 2 JRit [Nat 0])))
                            :@ (K :@ (S :@ (K :@ Rit) :@ Inc)))))
          :@ (K :@ (Fast 2 JLef [Uni]))


-- Fast Decrement --------------------------------------------------------------

pattern Fec = Fast 1 JFec []

{-
    fec = \n -> Cas (dec n) (k 0) i
-}
sjFec ∷ SingJet
sjFec = SingJet{..}
  where
    sjFast = JFec
    sjArgs = 1
    sjName = MkVal (Nat 3)

    sjExec [Nat 0]       = Just (Nat 0)
    sjExec [Nat x]       = Just (Nat (pred x))
    sjExec [_]           = Nothing
    sjExec _             = error "bad-dec"

    sjBody = MkVal $
        S :@ (S :@ (S :@ (K :@ Cas) :@ Dec)
                :@ (K :@ (K :@ Nat 0)))
          :@ (K :@ I)


-- Add -------------------------------------------------------------------------

pattern Add = Fast 2 JAdd []

{-
    add = \x y -> Pak (\i z -> x i (y i z))
-}
sjAdd ∷ SingJet
sjAdd = SingJet{..}
  where
    sjFast = JAdd
    sjArgs = 2
    sjName = MkVal K
    sjExec [Nat x, Nat y]   = Just $ Nat (x+y)
    sjExec _                = Nothing
    sjBody = MkVal $
        S :@ (K :@ (S :@ (K :@ Pak)))
          :@ (S :@ (K :@ S)
                :@ (S :@ (K :@ (S :@ (K :@ S) :@ K))))



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
    sjExec [Nat x, Nat y]   = Just $ sub x y
    sjExec [_, _]           = Nothing
    sjExec _                = error "bad-sub"
    sjBody = MkVal $
        S :@ (K :@ (S:@(S:@I:@(K:@(S:@(S:@Cas:@(K:@Lef)):@(K:@Dec))))))
          :@ (S :@ (K :@ K) :@ Rit)

    sub ∷ Natural → Natural → Ur
    sub x y | y > x = Lef :@ Uni
    sub x y         = Rit :@ Nat (x-y)


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
    sjExec [Fast _ JCon [_,y]] = Just y
    sjExec [_]                 = Nothing
    sjExec _                   = error "bad-cdr"
    sjBody = MkVal $
        S :@ I :@ (K :@ (S :@ K))


-- Natural Numbers -------------------------------------------------------------

pattern Nat n = Fast 2 (JNat n) []

j_nat ∷ Check
j_nat = Named "nat" chk
  where chk ∷ Positive → JetTag → Val → Maybe Jet
        chk 2 (MkVal K) u = JNat <$> unChurch (valUr u)
        chk n t         b = Nothing

natJet ∷ Natural → Ur
natJet n = J 2 :@ K :@ church n
  where
    church ∷ Natural → Ur
    church 0 = S :@ K
    church n = S :@ (S :@ (K :@ S) :@ K) :@ church (pred n)


-- Church To Natural -----------------------------------------------------------

pattern Pak = Fast 1 JPak []

{-
    pak = \n -> J2 (n inc zero)
-}
sjPak ∷ SingJet
sjPak = SingJet{..}
  where
    sjFast = JPak
    sjArgs = 1
    sjName = MkVal (Nat 16)
    sjExec [Nat n]       = Just $ Nat n
    sjExec [_]           = Nothing
    sjExec _             = error "bad-pak"
    sjBody = MkVal $
        S :@ (K :@ J2)
          :@ (S :@ (S :@ I :@ (K :@ succ))
                :@ (K :@ zero))

    succ = S :@ (S :@ (K :@ S) :@ K)
    zero = S :@ K


-- Delayed Evaluation ----------------------------------------------------------

waitJet ∷ Natural → Ur
waitJet n = J (fromIntegral $ n+1) :@ I :@ I

waitArity ∷ Natural → Positive
waitArity n = fromIntegral (n+1)

waitExec ∷ Natural → [Ur] → Maybe Ur
waitExec _ []     = Nothing
waitExec _ (u:us) = Just (go u us)
  where
    go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

j_wait ∷ Check
j_wait = Named "wait" chk
  where chk ∷ Positive → JetTag → Val → Maybe Jet
        chk n (MkVal I) (MkVal I) = Just $ Wait (fromIntegral n - 1)
        chk _ _         _         = Nothing


-- Natural Is Zero Test --------------------------------------------------------

pattern Zer = Fast 1 JZer []

{-
    isZero = \n -> n (K No) Ya
-}
sjZer ∷ SingJet
sjZer = SingJet{..}
  where
    sjFast = JZer
    sjArgs = 1
    sjName = MkVal K
    sjExec [Nat x]       = Just $ if x==0 then Ya else No
    sjExec [_]           = Nothing
    sjExec _             = error "bad-zer"

    sjBody = MkVal $
        S :@ (S :@ I :@ (K :@ (K :@ No))) :@ (K :@ Ya)



-- Natural Equality ------------------------------------------------------------

pattern Eql = Fast 2 JEql []

{-
    equals = \x y -> Cas (sub x y) (K No) IsZero
-}
sjEql ∷ SingJet
sjEql = SingJet{..}
  where
    sjFast = JEql
    sjArgs = 2
    sjName = MkVal K

    sjExec [Nat x, Nat y]   = Just $ if x==y then Ya else No
    sjExec [_,_]            = Nothing
    sjExec _                = error "bad-eql"

    sjBody = MkVal $
        S :@ (S :@ (K :@ S)
                :@ (S :@ (S :@ (K :@ S)
                            :@ (S :@ (K :@ (S :@ (K :@ Cas))) :@ Sub))
                      :@ (K :@ (K :@ (K :@ No)))))
          :@ (K :@ (K :@ Zer))


-- Atom Multiplication ---------------------------------------------------------

pattern Mul = Fast 2 JMul []

{-
    mul = \x y -> Pak (\i z -> x (y i) z)
-}
sjMul ∷ SingJet
sjMul = SingJet{..}
  where
    sjFast = JMul
    sjArgs = 2
    sjName = MkVal K
    sjExec [Nat x, Nat y]   = Just $ Nat (x*y)
    sjExec _                = Nothing
    sjBody = MkVal $
        S :@ (K :@ (S :@ (K :@ Pak)))
          :@ (S :@ (K :@ S) :@ K)
