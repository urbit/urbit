module Urbit.Moon.MoonToUruk where

import Bound
import ClassyPrelude
import GHC.Natural
import Urbit.Moon.AST
import Control.Monad.Except
import Urbit.Uruk.Class

import Control.Arrow             ((>>>))
import Data.Function             ((&))
import System.IO.Unsafe          (unsafePerformIO)
import Text.Show.Pretty          (ppShow)
import Urbit.Uruk.Fast.OptToFast (optToFast)

import qualified Urbit.Atom              as Atom
import qualified Urbit.Moon.LambdaToUruk as Lamb
import qualified Urbit.Moon.Parser       as Parser
import qualified Urbit.Uruk.Fast         as F
import qualified Urbit.Uruk.Refr.Jetted  as Ur

--------------------------------------------------------------------------------

getGlobal :: Uruk p => Text -> p
getGlobal = \case
  "S"     -> uEss
  "K"     -> uKay
  "J"     -> uJay 1
  "const" -> uKay
  "D"     -> uDee
  "I"     -> uEye
  "id"    -> uEye
  "B"     -> uBee
  "dot"   -> uBee
  "C"     -> uSea
  "flip"  -> uSea
  "cas"   -> uCas
  "lef"   -> uLef
  "rit"   -> uRit
  "iff"   -> uIff
  "seq"   -> uSeq
  "pak"   -> uPak
  "zer"   -> uZer
  "eql"   -> uEql
  "inc"   -> uInc
  "dec"   -> uDec
  "fec"   -> uFec
  "add"   -> uAdd
  "sub"   -> uSub
  "mul"   -> uMul
  "fix"   -> uFix
  "ded"   -> uDed
  "uni"   -> uUni
  "con"   -> uCon
  "car"   -> uCar
  "cdr"   -> uCdr
  str     -> error ("undefined variable: " <> unpack str)
  where p = Lamb.Prim

{-
    | Sn !Positive
    | Bn !Positive
    | Cn !Positive
    | Yet !Natural
-}

toUruk :: Exp Text -> IO Ur.Ur
toUruk = Lamb.moonStrict . toLC getGlobal

forceParse :: Text -> AST
forceParse = Parser.parseAST >>> \case
  Left  err -> error (show err)
  Right ex  -> ex

forceParseExp ∷ Text → Exp Text
forceParseExp = bind . forceParse

gogogo :: Text -> IO Ur.Ur
gogogo text = Ur.simp <$> complex
 where
  ast     = traceShowId (forceParse text)
  exp     = bind ast
  lam     = traceShowId (toLC getGlobal exp)
  complex = traceShowId <$> Lamb.moonStrict lam

gogogo' :: Text -> ExceptT Text IO Ur.Ur
gogogo' text = do
  ast <- ExceptT $ pure $ Parser.parseAST text

  traceM ""
  traceM (show ast)
  traceM ""

  let !expr = bind ast
      !lamb = toLC getGlobal expr

  cplx <- liftIO (Lamb.moonStrict lamb)

  traceM ""
  traceM (show lamb)
  traceM ""

  traceM ""
  traceM (ppShow cplx)
  traceM ""

  pure (Ur.simp cplx)

gogogoFast :: (Eq p, Uruk p) => Text -> ExceptT Text IO p
gogogoFast text = do
  ast <- ExceptT $ pure $ Parser.parseAST text

  let expr = bind ast
      lamb = toLC getGlobal expr

  cplx <- liftIO $ Lamb.moonStrict lamb

  pure cplx


toLC :: forall p. Uruk p => (Text -> p) -> Exp Text -> Lamb.Exp p
toLC getGlobal = go (Left . getGlobal)
 where
  go :: (a -> Either p Nat) -> Exp a -> Lamb.Exp p
  go f = \case
    Var a     -> var f a
    Lam b     -> lam f b
    App x y   -> Lamb.Go (go f x) (go f y)
    Jet r n b -> Lamb.Jet (fromIntegral r) (Atom.utf8Atom n) (go f b)
    Fix b     -> Lamb.Loop (enter f b)
    Sig       -> Lamb.Prim uUni
    Con x y   -> con f x y
    Cas x l r -> cas f x l r
    Iff c t e -> Lamb.If (go f c) (go f t) (go f e)
    Lit n     -> Lamb.Prim $ uNat n
    Bol b     -> Lamb.Prim $ uBol b
    Str n     -> Lamb.Prim $ uNat $ Atom.utf8Atom n

  enter :: (a -> Either p Nat) -> Scope () Exp a -> Lamb.Exp p
  enter f b = go f' (fromScope b) where f' = wrap f

  lam :: (a -> Either p Nat) -> Scope () Exp a -> Lamb.Exp p
  lam f b = Lamb.Lam (enter f b)

  var f a = f a & \case
    Left  e -> Lamb.Prim e
    Right v -> Lamb.Var v

  cas :: (a -> Either p Nat) -> Exp a -> Scope () Exp a -> Scope () Exp a -> Lamb.Exp p
  cas f x l r = Lamb.Case (go f x) (enter f l) (enter f r)

  con :: (a -> Either p Nat) -> Exp a -> Exp a -> Lamb.Exp p
  con f x y = Lamb.Prim uCon `Lamb.Go` go f x `Lamb.Go` go f y

  wrap :: (a -> Either p Nat) -> Var () a -> Either p Nat
  wrap f = \case
    B () -> Right 0
    F x  -> succ <$> f x
