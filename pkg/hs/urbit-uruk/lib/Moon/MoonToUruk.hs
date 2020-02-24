module Moon.MoonToUruk where

import Bound
import ClassyPrelude
import GHC.Natural
import Moon.AST

import Control.Arrow    ((>>>))
import Data.Function    ((&))
import System.IO.Unsafe (unsafePerformIO)
import Uruk.OptToFast   (optToFast)

import qualified Moon.Parser      as Parser
import qualified Urbit.Atom       as Atom
import qualified Uruk.JetComp     as Uruk
import qualified Uruk.JetDemo     as Ur
import qualified Uruk.JetOptimize as Opt
import qualified Uruk.Fast        as F

--------------------------------------------------------------------------------

getGlobal :: Text -> Ur.Ur
getGlobal = \case
  "s"    -> Ur.S
  "k"    -> Ur.K
  "j"    -> Ur.J 1
  "d"    -> Ur.D
  "id"   -> Ur.I
  "dot"  -> Ur.B
  "flip" -> Ur.C
  "cas"  -> Ur.Cas
  "lef"  -> Ur.Lef
  "rit"  -> Ur.Rit
  "yea"  -> Ur.Bol True
  "nah"  -> Ur.Bol False
  "iff"  -> Ur.Iff
  "seq"  -> Ur.Seq
  "pak"  -> Ur.Pak
  "zer"  -> Ur.Zer
  "eql"  -> Ur.Eql
  "inc"  -> Ur.Inc
  "dec"  -> Ur.Dec
  "fec"  -> Ur.Fec
  "add"  -> Ur.Add
  "sub"  -> Ur.Sub
  "mul"  -> Ur.Mul
  "fix"  -> Ur.Fix
  "ded"  -> Ur.Ded
  "uni"  -> Ur.Uni
  "con"  -> Ur.Con
  "car"  -> Ur.Car
  "cdr"  -> Ur.Cdr
  str    -> error ("undefined variable: " <> unpack str)
  where p = Uruk.Prim

{-
    | Sn !Positive
    | Bn !Positive
    | Cn !Positive
    | Yet !Natural
-}

toUruk :: Exp Text -> Ur.Ur
toUruk = Uruk.moonStrict . toLC getGlobal

forceParse :: Text -> AST
forceParse = Parser.parseAST >>> \case
  Left  err -> error (show err)
  Right ex  -> ex

forceParseExp ∷ Text → Exp Text
forceParseExp = bind . forceParse

gogogo :: Text -> Ur.Ur
gogogo text = Ur.simp complex
 where
  ast     = traceShowId (forceParse text)
  exp     = bind ast
  lam     = traceShowId (toLC getGlobal exp)
  complex = traceShowId (Uruk.moonStrict lam)

toLC :: (Text -> Ur.Ur) -> Exp Text -> Uruk.Exp
toLC getGlobal = go (Left . getGlobal)
 where
  go :: (a -> Either Ur.Ur Nat) -> Exp a -> Uruk.Exp
  go f = \case
    Var a     -> var f a
    Lam b     -> lam f b
    App x y   -> Uruk.Go (go f x) (go f y)
    Jet r n b -> Uruk.Jet (fromIntegral r) (Atom.utf8Atom n) (go f b)
    Fix b     -> Uruk.Loop (enter f b)
    Sig       -> Uruk.Prim Ur.Uni
    Con x y   -> con f x y
    Cas x l r -> cas f x l r
    Iff c t e -> Uruk.If (go f c) (go f t) (go f e)
    Lit n     -> Uruk.Prim $ Ur.Nat n
    Str n     -> Uruk.Prim $ Ur.Nat $ Atom.utf8Atom n

  enter f b = go f' (fromScope b) where f' = wrap f

  lam f b = Uruk.Lam (enter f b)

  var f a = f a & \case
    Left  e -> Uruk.Prim e
    Right v -> Uruk.Var v

  cas f x l r = Uruk.Case (go f x) (enter f l) (enter f r)

  con f x y = Uruk.cons `Uruk.Go` go f x `Uruk.Go` go f y

  wrap f = \case
    B () -> Right 0
    F x  -> succ <$> f x

-- Ackermann -------------------------------------------------------------------

ackerSrc ∷ Text
ackerSrc = unlines
    [  "~/  2  acker"
    ,  "..  recur"
    ,  "|=  (x y)"
    ,  "?:  (zer x)"
    ,  "  (inc y)"
    ,  "?:  (zer y)"
    ,  "  (recur (fec x) 1)"
    ,  "(recur (fec x) (recur x (fec y)))"
    ]

tryAckerSrc ∷ Nat → Nat → Text
tryAckerSrc x y = unlines
  [ "=/  acker"
  , indentBlock ackerSrc
  , "(acker " <> tshow x <> " " <> tshow y <> ")"
  ]

tryAcker ∷ Nat → Nat → Ur.Ur
tryAcker x y = gogogo (tryAckerSrc x y)

ackerUr ∷ Ur.Ur
ackerUr = gogogo ackerSrc

ackerOpt :: Opt.Code
ackerOpt =
  unsafePerformIO
    $ Opt.compile
    $ traceShowId
    $ Uruk.moonStrict
    $ traceShowId
    $ toLC getGlobal
    $ bind
    $ traceShowId
    $ forceParse ackerSrc

ackerJet :: F.Jet
ackerJet = optToFast ackerOpt

runFastAcker :: Nat -> Nat -> IO F.Val
runFastAcker x y = F.execJet2 ackerJet (F.VNat x) (F.VNat y)


-- Fib -------------------------------------------------------------------------

fibSrc ∷ Text
fibSrc = unlines
    [  "~/  1  fib"
    ,  "..  fib"
    ,  "|=  x"
    ,  "?:  (zer x)"
    ,  "  1"
    ,  "?:  (eql x 1)"
    ,  "  1"
    ,  "(add (fib (fec x)) (fib (fec (fec x))))"
    ]

tryFibSrc ∷ Nat → Text
tryFibSrc x = unlines
  [ "=/  fib"
  , indentBlock fibSrc
  , "(fib " <> tshow x <> ")"
  ]

indentBlock = unlines . fmap ("  " <>) . lines

tryFib ∷ Nat → Ur.Ur
tryFib = gogogo . tryFibSrc

fibUr ∷ Ur.Ur
fibUr = gogogo fibSrc

fibOpt :: Opt.Code
fibOpt =
  unsafePerformIO
    $ Opt.compile
    $ traceShowId
    $ Uruk.moonStrict
    $ traceShowId
    $ toLC getGlobal
    $ bind
    $ traceShowId
    $ forceParse fibSrc

fibJet :: F.Jet
fibJet = optToFast fibOpt

runFastFib :: Nat -> IO F.Val
runFastFib x = F.execJet1 fibJet (F.VNat x)
