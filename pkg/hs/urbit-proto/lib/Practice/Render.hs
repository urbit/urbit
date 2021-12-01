module Practice.Render where

import ClassyPrelude

import Data.Either (fromRight)
import Data.List (unfoldr)
import Data.Text (stripEnd)
import Data.Void
import Urbit.Atom (atomUtf8)

import Practice.DependentLambda
import Practice.Hoon2DependentLambda
import Practice.HoonCommon
import Practice.HoonSyntax

-- My garbagaciously written prettyprinter -------------------------------------

-- | Like hoon's tank, but for tall printing only, and also different.
data Tank
  = Leaf Text
  -- | Backstep multiline
  | Palm Text [Tank]
  -- | Non-backstep multiline
  | Rose Text Text [Tank]
  -- | Jogging, switching intelligently between kingside and queenside
  | Fern Text Text [Tank] [(Tank, Tank)]
  -- | Hopping
  | Stem Text Text [Tank] [(Text, Tank, Tank)]
  deriving (Show)

-- The nature of the outermost layer of a wide form.
data Kind
  = Scat  -- ^ The wide form can be embedded anywhere
  | Long  -- ^ The wide form cannot occur to the left of a binary operator
  deriving (Show)

-- | Prettyprinting intermediate form. We represent the tall form of a hoon as a
-- tank, and the wide form, if any, as a text.
data Roll
  = Smol Kind Tank Text
  | Huge Tank
  deriving (Show)

-- XX the existence of this function is really a smell
tank :: Roll -> Tank
tank = \case
  Smol _ _ x -> Leaf x
  Huge t -> t

smol :: Roll -> Maybe (Kind, Text)
smol = \case
  Smol k _ l -> Just (k, l)
  Huge _ -> Nothing

leaf :: Text -> Roll
leaf t = Smol Scat (Leaf t) t


-- Rendering rolls -------------------------------------------------------------

renderRoll :: Roll -> Text
renderRoll = \case
  Smol _ _ x -> x
  Huge t -> renderTank t


-- Rendering tanks -------------------------------------------------------------

data Line = Line { ind :: !Int, tex :: Text }

dent :: Int -> Line -> Line
dent s l = l { ind = s + ind l }

renderTank :: Tank -> Text
renderTank = renderLines . toList . tankLines

renderLines :: [Line] -> Text
renderLines = intercalate "\n"
            . map \Line{..} -> stripEnd $ replicate ind ' ' <> tex

tankLines :: Tank -> DList Line
tankLines = \case
  Leaf x -> pure $ Line 0 x
  Palm r ts -> hang r 0 ts
  Rose run end (t:ts) -> concat
    [ push run 0 $ tankLines t
    , dent (length run + 2) <$> (concat $ map tankLines ts)
    , pure $ Line 0 end
    ]
  Rose run end [] -> pure $ Line 0 (run <> "  " <> end)
  Fern run end hs legs -> concat
    [ hang run 1 hs
    , concatMap leg legs
    , pure $ Line 0 end
    ]
  Stem run end hs arms -> concat
    [ hang run 1 hs
    , concatMap arm arms
    , pure $ Line 0 end
    ]
 where
  hang :: Text -> Int -> [Tank] -> DList Line
  hang run xtra = \case
    t:ts -> push run xtra (tankLines t) <> tack xtra ts
    []   -> pure $ Line 0 run

  fang :: Text -> Int -> [Tank] -> DList Line
  fang run xtra ts = case for ts \case Leaf x -> Just x; _ -> Nothing of
    Just xs -> pure $ Line 0 $ run <> "  " <> intercalate "  " xs
    Nothing -> hang run xtra ts

  back :: Int -> [Int]
  back = unfoldr \case
    0 -> Nothing
    numRemaining -> Just (2 * (numRemaining - 1), numRemaining - 1)

  tack :: Int -> [Tank] -> DList Line
  tack xtra ts = concat $
    zipWith (\i t -> dent i <$> tankLines t) (back $ length ts + xtra) ts

  push :: Text -> Int -> DList Line -> DList Line
  push r tailSize ls = case toList ls of
    Line i x : ls -> fromList $
      Line i (r <> replicate cnt ' ' <> x) : (dent (length r + cnt) <$> ls)
     where
      cnt = max 2 (2 * tailSize)
    [] -> error "tankLines: invariant violation: empty tank"

  leg :: (Tank, Tank) -> DList Line
  leg = \case
    (Leaf p, Leaf q) | length p + length q + 2 <= 40 ->
      pure $ Line 4 (p <> "  " <> q)
    (a, b) -> tack 0 [a, b, Leaf "::"]  -- TODO get rid of tail in last one

  arm :: (Text, Tank, Tank) -> DList Line
  arm (run, t1, t2) = fang run 1 [t1, t2]


-- Producing rolls from syntax -------------------------------------------------

-- | Can be prettyprinted.
class Rolling r where
  roll :: r -> Roll

instance Rolling Text where
  roll = leaf

instance Rolling (Code Void) where
  roll = roll . shut absurd

instance Rolling (Base Void) where
  roll = roll . loft

instance Rolling (Code Term) where
  roll = roll . shut id

instance Rolling (Base Term) where
  roll = roll . loft

instance Rolling ([Act], Fail) where
  roll (as, f) = Huge $ Rose "" "" [tank $ roll as, tank $ roll f]

instance Rolling [Act] where
  roll as = Huge $ Rose "trace:" "" $ map (tank . roll) $ reverse as

instance Rolling Act where
  roll = \case
    ActFits e f t u -> Huge $ Stem (tshow f <> ":") "" []
      [ ("have", tank $ roll $ fmap e t, Leaf "")
      , ("need", tank $ roll $ fmap e u, Leaf "")
      ]
    ActFind e t w -> Huge $ Stem "find:" "" []
      [ ("type", tank $ roll $ fmap e t, Leaf "")
      , ("wing", tank $ roll w, Leaf "")
      ]
    ActWork e f c t -> Huge $ Stem "work:" "" []
      [ ("mode", Leaf $ tshow f,         Leaf "")
      , ("code", tank $ roll $ fmap e c, Leaf "")
      , ("type", tank $ roll $ fmap e t, Leaf "")
      ]
    ActPlay e c -> Huge $ Stem "play:" "" []
      [ ("code", tank $ roll $ fmap e c, Leaf "")
      ]
    ActNote t -> Huge $ Palm "note:" [Leaf t]


instance Rolling Fail where
  roll = \case
    FitsFail e f t u -> Huge $ Stem (tshow f <> "-fail:") "" []
      [ ("have", tank $ roll $ fmap e t, Leaf "")
      , ("need", tank $ roll $ fmap e u, Leaf "")
      ]
    NeedGate e t -> Huge $ Palm "need-gate:" [tank $ roll $ fmap e t]
    BailNote t -> Huge $ Palm "bail-note:" [Leaf t]
    BailFail -> leaf "bail-fail:"

-- | Limit the width of a wide form in two ways.
chop :: Int -> Roll -> Roll
chop ribbon = \case
  Smol k t l
    | length l <= ribbon -> Smol k t l
    | otherwise -> Huge t
  Huge t -> Huge t

-- | Chop with default limits. Obviously these should be configurable, but I
-- care about other things more right now.
chip :: Roll -> Roll
chip = chop 40

-- | The main entry point to the pretty printer.
render :: Rolling r => r -> Text
render = renderRoll . roll

printLimb :: Limb -> Text
printLimb = \case
  Axis a -> "+" <> tshow a
  Ally "" -> "%"
  Ally n -> n

instance Rolling Limb where
  roll = leaf . printLimb

instance Rolling Wing where
  roll w = leaf (go w)
   where
    go = \case
      []     -> "."
      [l]    -> printLimb l
      l:ls   -> printLimb l <> "." <> go ls

going :: ((Kind, Text) -> (Kind, Text)) -> (Tank -> Tank) -> Roll -> Roll
going f g r = chip case smol r of
  Just ks -> let (k, s) = f ks in Smol k (g $ tank r) s
  Nothing -> Huge (g $ tank r)

fixed :: Text -> Text -> Text -> [Roll] -> Roll
fixed tallR wideL wideR rs = chip case traverse smol rs of
  Just kls -> Smol Scat tnk $
    wideL <> intercalate " " (map snd kls) <> wideR
  Nothing -> Huge tnk
 where
  tnk = Palm tallR (map tank rs)

running :: Text -> Text -> Text -> Text -> [Roll] -> Roll
running tallT tallB wideL wideR rs = chip case traverse smol rs of
  Just kls -> Smol Scat tnk $
    wideL <> intercalate " " (map snd kls) <> wideR
  Nothing -> Huge tnk
 where
  tnk = Rose tallT tallB (map tank rs)

jogging :: Text -> Text -> Text -> Text -> [Roll] -> [(Roll, Roll)] -> Roll
jogging run end lef rit hs js = case ( traverse smol hs
                                     , traverse smol $ map fst js
                                     , traverse smol $ map snd js
                                     ) of
  (Just hs, Just ls, Just rs) -> Smol Scat tnk $ lef <> hed <> bod <> rit
   where
    hed = concat [h <> " " | (_, h) <- hs]
    bod = intercalate ", " [l <> " " <> r | ((_, l), (_, r)) <- zip ls rs]
  (_, _, _) -> Huge tnk
 where
  tnk = Fern run end (map tank hs) (map (tank *** tank) js)

binary :: Text -> Text -> Roll -> Roll -> Roll
binary run op r1 r2 = case (r1, r2) of
  (Smol Scat t1 x, Smol _ t2 y) -> Smol Long (Palm run [t1, t2]) (x <> op <> y)
  _ -> Huge (Palm run [tank r1, tank r2])

showRock :: Atom -> Term -> Text
showRock a = \case
  "f" -> case a of
    0 -> "%&"
    1 -> "%|"
    _ -> "%" <> tshow a
  "n" -> case a of 0 -> "~"; _ -> "%" <> tshow a
  "t" -> "%" <> case atomUtf8 a of
    Right x -> tshow x  -- XX quotes, escapes
    Left _  -> tshow a
  "tas" -> "%" <> case a of
    0 -> "%"
    _ -> fromRight (tshow a) $ atomUtf8 a
  _ -> "%" <> tshow a

showSand :: Atom -> Term -> Text
showSand a = \case
  "f" -> case a of
    0 -> "&"
    1 -> "|"
    _ -> tshow a
  "n" -> case a of 0 -> "~"; _ -> tshow a
  "t" -> case atomUtf8 a of
    Right x -> tshow x  -- XX quotes, escapes
    Left _  -> tshow a
  _ -> tshow a

showGlow :: Atom -> Term -> Text
showGlow a au = "$" <> case au of
  "f" -> case a of
    0 -> "&"
    1 -> "|"
    _ -> tshow a
  "n" -> case a of 0 -> "~"; _ -> tshow a
  "t" -> case atomUtf8 a of
    Right x -> tshow x  -- XX quotes, escapes
    Left _  -> tshow a
  "tas" -> case a of
    0 -> "$"
    _ -> fromRight (tshow a) $ atomUtf8 a
  _ -> tshow a

instance Rolling Bass where
  roll = \case
    Non -> leaf "*"
    Cel -> leaf "^"
    Flg -> leaf "?"
    Nul -> leaf "$~"
    Vod -> leaf "!"
    Fok [a] au -> leaf $ showGlow a au
    Fok as au -> leaf $ "?(" <> intercalate " " (map (`showGlow` au) as) <> ")"
    Aur au -> leaf $ "@" <> au
    Typ -> leaf "$"

instance Rolling Hoon where
  roll = \case
    Wung w -> roll w
    Adam Rock a au -> leaf $ showRock a au
    Adam Sand a au -> leaf $ showSand a au
    --
    Bass b -> roll b
    Bccb h -> going (\(_, s) -> (Long, "_" <> s)) (Palm "$_" . singleton)
            $ roll h
    Bccl s ss -> running "$:" "==" "{" "}" (map roll $ s:ss)
    Bccn aaus -> Huge $ Rose "$%" "==" $ map clause aaus
     where
      clause = \case
        (a, au, Bccl s ss) -> tank $ roll $ Bccl (Bass $ Fok [a] au) (s:ss)
        (a, au, s) -> tank $ roll $ Bccl (Bass $ Fok [a] au) [s]
    Bcdt s ms -> Huge $ Stem "$." "--" [tank $ roll s] (arms ms)
    Bchp s t -> fixed "$-" "$-(" ")" [roll s, roll t]
    Bckt s t -> fixed "$^" "$^(" ")" [roll s, roll t]
    Bcts s t -> binary "$=" "|" (roll s) (roll t)
    Bcpt s t -> fixed "$@" "$@(" ")" [roll s, roll t]
    Bcwt ms -> Huge $ Stem "$!" "--" [] (arms ms)
    --
    Brcn ms -> Huge $ Stem "|%" "--" [] (arms ms)
    Brts s h -> fixed "|=" "|=(" ")" [roll s, roll h]
    --
    Clcb h j -> Huge $ Palm ":_" [tank $ roll h, tank $ roll j]
    Clkt h j k l -> fixed ":^" "[" "]" [roll h, roll j, roll k, roll l]
    Clhp h j -> fixed ":-" "[" "]" [roll h, roll j]
    Clls h j k -> fixed ":+" "[" "]" [roll h, roll j, roll k]
    Clsg hs -> running ":~" "==" "~[" "]" (map roll hs)
    Cltr hs -> running ":*" "==" "[" "]" (map roll hs)
    --
    Cndt h j -> Huge $ Palm "%." [tank $ roll h, tank $ roll j]
    Cnhp h j -> fixed "%-" "(" ")" [roll h, roll j]
    Cncl h hs -> running "%:" "==" "(" ")" (map roll (h:hs))
    Cnkt h j k l -> fixed "%^" "(" ")" [roll h, roll j, roll k, roll l]
    Cnls h j k -> fixed "%+" "(" ")" [roll h, roll j, roll k]
    Cnts w whs -> case ( roll w
                       , traverse smol $ map (roll . fst) whs
                       , traverse smol $ map (roll . snd) whs
                       ) of
      (Smol Scat _ w, Just ls, Just rs) -> Smol Long tnk $ w <> "(" <> x <> ")"
       where
        x = intercalate ", " [l <> " " <> r | ((_, l), (_, r)) <- zip ls rs]
      (_, _, _) -> Huge tnk
     where
      tnk = Fern "%=" "==" [tank $ roll w] [(tank $ roll ww, tank $ roll h)
                                           | (ww, h) <- whs ]
    --
    Dtkt h j -> fixed ".^" ".^(" ")" [roll h, roll j]
    Dtls h   -> fixed ".+" "+("  ")" [roll h]
    Dttr h j -> fixed ".*" ".*(" ")" [roll h, roll j]
    Dtts h j -> fixed ".=" "=("  ")" [roll h, roll j]
    Dtwt h   -> fixed ".?" ".?(" ")" [roll h]
    --
    Ktls h j -> Huge $ Palm "^+" [tank $ roll h, tank $ roll j]
    Kthp s h -> Huge $ Palm "^-" [tank $ roll s, tank $ roll h]
    Ktfs h s -> binary "^/" "/" (roll h) (roll s)
    Ktwt h -> fixed "^?" "^?(" ")" [roll h]
    Ktts s h -> binary "^=" "=" (roll s) (roll h)
    Ktcl s -> fixed "^:" "^:(" ")" [roll s]
    Ktzp s h -> case (roll s, roll h) of
      (Smol _ _ ss, Smol _ _ hh) -> Smol Long tnk $ "`" <> ss <> "`" <> hh
      _ -> Huge tnk
     where
      tnk = Palm "^!" [tank $ roll s, tank $ roll h]
    --
    Sgfs t h -> Huge $ Palm "~/" [Leaf ("%" <> t), tank $ roll h]
    --
    --
    -- XX should print like
    -- =/  a  blah
    -- b
    Tsfs s h j -> Huge $ Palm "=/" [tank $ roll s, tank $ roll h, tank $ roll j]
    Tsmc s h j -> Huge $ Palm "=;" [tank $ roll s, tank $ roll h, tank $ roll j]
    Tsdt w h j -> Huge $ Palm "=." [tank $ roll w, tank $ roll h, tank $ roll j]
    Tswt w h j k -> Huge $ Palm "=?" $ tank (roll w) : map (tank.roll) [h, j, k]
    Tsgl h j -> binary "=<" ":" (roll h) (roll j)
    Tsgr h j -> Huge $ Palm "=>" [tank $ roll h, tank $ roll j]
    Tshp h j -> Huge $ Palm "=-" [tank $ roll h, tank $ roll j]
    Tskt s w h j -> Huge $ Palm "=^"
      [tank $ roll s, tank $ roll w, tank $ roll h, tank $ roll j]
    Tsls h j -> Huge $ Palm "=+" [tank $ roll h, tank $ roll j]
    Tssg hs -> Huge $ Rose "=~" "==" $ map (tank . roll) hs
    --
    Wtbr hs -> running "?|" "==" "|(" ")" $ map roll hs
    Wthp w shs -> jogging "?-" "==" "?-(" ")" [roll w] (map (roll *** roll) shs)
    Wtcl h j k -> Huge $ Palm "?:" [tank $ roll h, tank $ roll j, tank $ roll k]
    Wtdt h j k -> Huge $ Palm "?." [tank $ roll j, tank $ roll j, tank $ roll k]
    Wtkt w h j -> Huge $ Palm "?^" [tank $ roll w, tank $ roll h, tank $ roll j]
    Wtgl h j -> fixed "?<" "?<(" ")" [roll h, roll j]
    Wtgr h j -> fixed "?>" "?>(" ")" [roll h, roll j]
    Wtpm hs -> running "?&" "==" "&(" ")" $ map roll hs
    Wtpt w h j -> Huge $ Palm "?@" [tank $ roll w, tank $ roll h, tank $ roll j]
    Wtts s h -> fixed "?=" "?=(" ")" [roll s, roll h]
    Wtzp h -> case roll h of
      Smol _ t x -> Smol Long (Palm "?!" [t]) ("!" <> x)
      Huge t -> Huge $ Palm "?!" [t]
    --
    Zpzp -> leaf "!!"
   where
    arms ms = mapToList ms <&> \(arm, typ) -> ("++", Leaf arm, tank $ roll typ)
