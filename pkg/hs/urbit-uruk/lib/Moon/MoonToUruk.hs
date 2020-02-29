module Moon.MoonToUruk where

import Bound
import ClassyPrelude
import GHC.Natural
import Moon.AST
import Control.Monad.Except

import Control.Arrow    ((>>>))
import Data.Function    ((&))
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (ppShow)
import Uruk.OptToFast   (optToFast)

import qualified Moon.Parser      as Parser
import qualified Urbit.Atom       as Atom
import qualified Uruk.JetComp     as Uruk
import qualified Uruk.JetDemo     as Ur
import qualified Uruk.JetOptimize as Opt
import qualified Uruk.Fast        as F

--------------------------------------------------------------------------------

getGlobal :: Uruk.Uruk p => Text -> p
getGlobal = \case
  "S"     -> Uruk.uEss
  "K"     -> Uruk.uKay
  "J"     -> Uruk.uJay 1
  "const" -> Uruk.uKay
  "D"     -> Uruk.uDee
  "I"     -> Uruk.uEye
  "id"    -> Uruk.uEye
  "B"     -> Uruk.uBee
  "dot"   -> Uruk.uBee
  "C"     -> Uruk.uSea
  "flip"  -> Uruk.uSea
  "cas"   -> Uruk.uCas
  "lef"   -> Uruk.uLef
  "rit"   -> Uruk.uRit
  "iff"   -> Uruk.uIff
  "seq"   -> Uruk.uSeq
  "pak"   -> Uruk.uPak
  "zer"   -> Uruk.uZer
  "eql"   -> Uruk.uEql
  "inc"   -> Uruk.uInc
  "dec"   -> Uruk.uDec
  "fec"   -> Uruk.uFec
  "add"   -> Uruk.uAdd
  "sub"   -> Uruk.uSub
  "mul"   -> Uruk.uMul
  "fix"   -> Uruk.uFix
  "ded"   -> Uruk.uDed
  "uni"   -> Uruk.uUni
  "con"   -> Uruk.uCon
  "car"   -> Uruk.uCar
  "cdr"   -> Uruk.uCdr
  str     -> error ("undefined variable: " <> unpack str)
  where p = Uruk.Prim

{-
    | Sn !Positive
    | Bn !Positive
    | Cn !Positive
    | Yet !Natural
-}

toUruk :: Exp Text -> IO Ur.Ur
toUruk = Uruk.moonStrict . toLC getGlobal

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
  complex = traceShowId <$> Uruk.moonStrict lam

gogogo' :: Text -> ExceptT Text IO Ur.Ur
gogogo' text = do
  ast <- ExceptT $ pure $ Parser.parseAST text

  traceM ""
  traceM (show ast)
  traceM ""

  let !expr = bind ast
      !lamb = toLC getGlobal expr

  cplx <- liftIO (Uruk.moonStrict lamb)

  traceM ""
  traceM (show lamb)
  traceM ""

  traceM ""
  traceM (ppShow cplx)
  traceM ""

  pure (Ur.simp cplx)

gogogoFast :: Text -> ExceptT Text IO F.Val
gogogoFast text = do
    ast <- ExceptT $ pure $ Parser.parseAST text

    let expr = bind ast
        lamb = toLC getGlobal expr

    cplx <- liftIO $ Uruk.moonStrict lamb

    pure cplx


toLC :: forall p. Uruk.Uruk p => (Text -> p) -> Exp Text -> Uruk.Exp p
toLC getGlobal = go (Left . getGlobal)
 where
  go :: (a -> Either p Nat) -> Exp a -> Uruk.Exp p
  go f = \case
    Var a     -> var f a
    Lam b     -> lam f b
    App x y   -> Uruk.Go (go f x) (go f y)
    Jet r n b -> Uruk.Jet (fromIntegral r) (Atom.utf8Atom n) (go f b)
    Fix b     -> Uruk.Loop (enter f b)
    Sig       -> Uruk.Prim Uruk.uUni
    Con x y   -> con f x y
    Cas x l r -> cas f x l r
    Iff c t e -> Uruk.If (go f c) (go f t) (go f e)
    Lit n     -> Uruk.Prim $ Uruk.uNat n
    Bol b     -> Uruk.Prim $ Uruk.uBol b
    Str n     -> Uruk.Prim $ Uruk.uNat $ Atom.utf8Atom n

  enter :: (a -> Either p Nat) -> Scope () Exp a -> Uruk.Exp p
  enter f b = go f' (fromScope b) where f' = wrap f

  lam :: (a -> Either p Nat) -> Scope () Exp a -> Uruk.Exp p
  lam f b = Uruk.Lam (enter f b)

  var f a = f a & \case
    Left  e -> Uruk.Prim e
    Right v -> Uruk.Var v

  cas :: (a -> Either p Nat) -> Exp a -> Scope () Exp a -> Scope () Exp a -> Uruk.Exp p
  cas f x l r = Uruk.Case (go f x) (enter f l) (enter f r)

  con :: (a -> Either p Nat) -> Exp a -> Exp a -> Uruk.Exp p
  con f x y = Uruk.Prim Uruk.uCon `Uruk.Go` go f x `Uruk.Go` go f y

  wrap :: (a -> Either p Nat) -> Var () a -> Either p Nat
  wrap f = \case
    B () -> Right 0
    F x  -> succ <$> f x
