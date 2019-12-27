module Deppy.Demo where

import ClassyPrelude hiding (putStrLn)
import Prelude (putStrLn)

import Data.Function    ((&))
-- ort Text.Show.Pretty (ppShow)

import Deppy.Core
import Deppy.Parser hiding (Wide)
import Deppy.Showings
import Deppy.ToUntyped
import Untyped.Core    (copy)

import qualified Deppy.CST  as C
import qualified Deppy.Hoon as H
import qualified Noun       as N

demo :: Text -> IO ()
demo prog = parseCst prog & \case
  Left err -> putStrLn ("parse error: " <> unpack err)
  Right c -> do
    putStrLn ("parsed: " <> display c)
    let h = C.abstractify c
    putStrLn ("ast: " <> display h)
    let e = H.desugar h
    putStrLn ("core: " <> display e)
    let t = infer env e
    case t of
      Right t -> putStrLn ("type: " <> display (H.resugar' t))
      Left er -> putStrLn ("<type error>: " <> show er)
    let n = copy $ toUntyped e
    putStrLn ("nock: " <> show n)
  where
    env v = error ("error: free variable: " <> show v)

filo :: FilePath -> Text -> IO ()
filo fn expr = do
  decls <- readFileUtf8 fn
  demo (decls <> "\n" <> expr)

demoCST :: Text -> IO ()
demoCST prog = parseCst prog & \case
  Left err -> putStrLn ("parse error: " <> unpack err)
  Right c  -> do r <- pure (toRunic c)
                 -- StrLn (ppShow r)
                 putStrLn (runic r)

--------------------------------------------------------------------------------

data Runic
    = Leaf Text
    | RunC Text [Runic]
    | RunN Text [Runic]
    | Jog0 Text [(Runic, Runic)]
    | Jog1 Text Runic [(Runic, Runic)]
    | IFix Text Text [Runic]
    | JFix Text Text [(Runic, Runic)]
    | Bind Text Runic
    | Pair Text Runic Runic
    | Wide Runic
    | Pref Text Runic
    | Tied Runic Runic
    | Mode Runic Runic
  deriving (Show)

wide ∷ Runic → Text
wide = go
  where
    go = \case
        Leaf t      → t
        RunC t xs   → mconcat [t, "(", intercalate " " (go <$> xs), ")"]
        RunN t xs   → mconcat [t, "(", intercalate " " (go <$> xs), ")"]
        IFix h t xs → mconcat [h, intercalate " " (go <$> xs), t]
        JFix h t xs → mconcat [h, intercalate ", " (pair go <$> xs), t]
        Bind t v    → mconcat [t, "/", go v]
        Pair i h t  → mconcat [go h, i, go t]
        Jog0 i xs   → i <> "(" <> bod <> ")"
          where bod = intercalate " " (xs <&> (\(h,t) → go h <> " " <> go t))
        Jog1 i x [] → i <> "(" <> go x <> ")"
        Jog1 i x xs → i <> "(" <> go x <> "; " <> bod <> ")"
          where bod = intercalate ", " $ xs <&> (\(h,t) → go h <> " " <> go t)
        Wide x      → go x
        Pref t x    → t <> go x
        Tied x y    → go x <> go y
        Mode w _    → go w

    pair f (x, y) = f x <> " " <> f y

tall ∷ Runic → Text
tall = go 0
  where
    go d (wide -> t) | length t < 40 = line d t
    go d v                           = ta d v

    indent d t = replicate d ' ' <> t

    line d t = indent d t <> "\n"

    ta d = \case
        Leaf t → line d t

        RunC t xs → case xs of
                      []   -> line d t <> bod (length xs - 1) xs
                      x:xs -> indent d t <> "  " <> wide x <> "\n"
                           <> bod (length xs - 1) xs
          where bod n []     = ""
                bod n (x:xs) = go (d + n*2) x <> bod (pred n) xs

        RunN t xs → mconcat ([line d t] <> bod <> [line d "=="])
          where bod = go (d+2) <$> xs

        Jog0 t xs   → mconcat ([line d t] <> bod <> [line d "=="])
          where bod ∷ [Text]
                bod = (xs <&> (\(h,t) → go (d+2) h <> go (d+4) t))

        Jog1 t x xs → mconcat ([line d (t<>hed)] <> bod <> [line d "=="])
          where bod = xs <&> (\(h,t) → go (d+2) h <> go (d+4) t)
                hed = "  " <> wide x

        Mode _ t → go d t

        IFix h t xs → line d $ wide $ IFix h t xs
        JFix h t xs → line d $ wide $ JFix h t xs
        Bind t v    → line d $ wide $ Bind t v
        Pair i h t  → line d $ wide $ Pair i h t
        Wide x      → line d $ wide x
        Pref t x    → line d $ wide $ Pref t x
        Tied x y    → line d $ wide $ Tied x y

runic = unpack . tall

{-
      C.Var v -> Var v
      --
      C.Typ             -> Hax
      C.Fun (C.Abs t b) -> Fun (go t) (hoist go b)
      C.Cel (C.Abs t b) -> Cel (go t) (hoist go b)
      C.Wut as          -> Wut as
      --
      C.Lam (C.Abs t b)  -> Lam (go t) (hoist go b)
      C.Cns e f (Just t) -> The (go t) (Cns (go e) (go f))
      C.Cns e f Nothing  -> Cns (go e) (go f)
      C.Tag a            -> Tag a
      --
      C.App e f  -> App (go e) (go f)
      C.Hed e    -> Hed (go e)
      C.Tal e    -> Tal (go e)
      C.Cas e cs -> WutCen (go e) (go <$> cs)
      --
      C.Let e b         -> TisFas (go e) (hoist go b)
      C.Rec (C.Abs t b) -> DotDot (go t) (hoist go b)
-}

toRunic ∷ C.CST → Runic
toRunic = go
  where
    go = \case
        C.Hax          -> Leaf "#"
        C.Var t        -> Leaf t
        C.Tag a        -> tagLit a
        C.Col a x      -> appTag a x
        C.Hed x        -> hed x
        C.DotGal x     -> hed x
        C.Tal x        -> tal x
        C.DotGar x     -> tal x
        C.HaxBuc xs    -> tagUnion xs
        C.Obj cs       -> recLit cs
        C.BarCen cs    -> recLit cs
        C.HaxCen xs    -> recTy xs
        C.Cls xs       -> recTy xs
        C.Lam bs x     -> lambda bs x
        C.BarTis bs x  -> lambda bs x
        C.Fun bs x     -> pie bs x
        C.HaxHep bs x  -> pie bs x
        C.Cel bs x     -> cellTy bs x
        C.HaxCol bs x  -> cellTy bs x
        C.Wut w        -> wut w
        C.Cns xs       -> cellLit xs
        C.ColHep x y   -> cellLit [x, y]
        C.ColTar xs    -> cellLit xs
        C.App xs       -> apply (go <$> xs)
        C.CenDot x y   -> apply [go y, go x]
        C.CenHep x y   -> apply [go x, go y]
        C.The x y      -> the x y
        C.KetFas x y   -> the y x
        C.KetHep x y   -> the x y
        C.Fas x y      -> the y x
        C.TisFas t x y -> let_ t x y
        C.DotDot x y   -> fix x y
        C.WutCen x cs  -> switch x cs

    tagLit a = tag "%" "" a

    appTag a x = Mode wide tall
      where wide = Pair ":" (tag "" "" a) (go x)
            tall = apply [go x, tagLit a]

    hed x = Mode wide tall
      where wide = Pref "-." (go x)
            tall = RunC ".<" [go x]

    tal x = Mode wide tall
      where wide = Pref "+." (go x)
            tall = RunC ".>" [go x]

    tagUnion xs = Jog0 "$%" $ jog (tag "" "") go xs

    recTy xs = Mode wide tall
      where wide = JFix "{|" "|}" $ entJog $ mapToList xs
            tall = Jog0 "$=" $ jog (tag "" "") go xs

    pie bs x = Mode wide tall
      where wide = IFix "<|" "|>" $ fmap binder bs <> [go x]
            tall = RunN "$-" $ fmap binder bs <> [go x]

    switch x cs = Jog1 "?%" (go x) (jog (tag "%" "") go cs)

    recLit cs = Mode wide tall
      where wide = JFix "{" "}" $ entJog $ mapToList cs
            tall = Jog0 "|%" (entJog $ mapToList cs)

    fix x y = RunC ".." [binder x, go y]

    the x y = Mode wide tall
      where wide = Tied (IFix "`" "`" [go x]) (go y)
            tall = RunC "^-" [go x, go y]

    let_ t x y = RunC "=/" [Leaf t, go x, go y]

    apply xs = Mode wide tall
      where wide = IFix "(" ")" xs
            tall = RunN "%-" xs

    lambda bs x = Mode wide tall
      where wide = IFix "<" ">" (fmap binder bs <> [go x])
            tall = RunN "|=" (fmap binder bs <> [go x])

    cellTy bs x = Mode wide tall
      where
        wide = IFix "[|" "|]" (fmap binder bs <> [go x])
        tall = RunN "$:" (fmap binder bs <> [go x])

    cellLit xs = Mode wide tall
      where wide = IFix "[" "]" (go <$> xs)
            tall = xs & \case [x,y] → RunC ":-" [go x, go y]
                              _     → RunN ":*" (go <$> xs)

    jog ∷ Ord k => (k → Runic) → (v → Runic) → Map k v → [(Runic, Runic)]
    jog x y = fmap (\(k,v) -> (x k, y v)) . mapToList

    wut w = setToList w & \case
        [x] -> tag "$" "$" x
        xs  -> Wide $ RunN "?" (tag "" "" <$> xs)

    entJog ∷ [(N.Atom, C.CST)] → [(Runic, Runic)]
    entJog xs = xs <&> \(h,t) → (tag "" "" h, go t)

    binder ( Nothing, x ) = go x
    binder ( Just t,  x ) = Bind t (go x)

    tag t n 0 = Leaf (n <> "0")
    tag t n x = N.fromNoun (N.A x) & \case
        Just (N.Cord c) | okay c -> Leaf (t <> c)
        _                        -> Leaf (n <> tshow x)
      where
        okay = all (flip elem ['a'..'z'])

env v = error ("error: free variable: " <> show v)
-- Abstract Syntax Tree --------------------------------------------------------
