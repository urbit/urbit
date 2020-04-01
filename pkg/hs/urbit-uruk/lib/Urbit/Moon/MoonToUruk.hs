module Urbit.Moon.MoonToUruk where

import Prelude ()
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
import Urbit.Moon.MakeStrict     (makeStrict)
import Urbit.Moon.MoonToLambda   (moonToLambda)
import Urbit.Uruk.Fast.OptToFast (optToFast)

import qualified Urbit.Atom              as Atom
import qualified Urbit.Moon.LambdaToUruk as Lamb
import qualified Urbit.Moon.Parser       as Parser
import qualified Urbit.Uruk.Bracket      as B
import qualified Urbit.Uruk.Fast         as F
import qualified Urbit.Uruk.JetEval      as JetEval
import qualified Urbit.Uruk.Refr.Jetted  as Ur

--------------------------------------------------------------------------------

getGlobal :: Uruk p => Text -> Either Text p
getGlobal = \case
  "S"     -> Right uEss
  "K"     -> Right uKay
  "J"     -> Right (uJay 1)
  "const" -> Right uKay
  "D"     -> Right uDee
  "I"     -> Right uEye
  "id"    -> Right uEye
  "B"     -> Right uBee
  "dot"   -> Right uBee
  "C"     -> Right uSea
  "flip"  -> Right uSea
  "cas"   -> Right uCas
  "iff"   -> Right uIff
  "seq"   -> Right uSeq
  "fix"   -> Right uFix
  "uni"   -> Right uUni
  "con"   -> Right uCon
  str     -> may str (uGlobal str)
 where
  may str Nothing  = Left ("Error: undefined variable:\n\n  " <> str <> "\n")
  may _   (Just x) = Right x

toUruk :: Exp Text -> IO (Either Text Ur.Ur)
toUruk = sequence . fmap Lamb.moonStrict . bindLC . toLC

forceParse :: Text -> AST
forceParse = Parser.parseAST >>> \case
  Left  err -> error (show err)
  Right ex  -> ex

forceParseExp ∷ Text → Exp Text
forceParseExp = bind . forceParse

gogogo' :: Text -> ExceptT Text IO Ur.Ur
gogogo' text = do
  ast <- ExceptT $ pure $ Parser.parseAST text

  traceM ""
  traceM (show ast)
  traceM ""

  let !expr = bind ast
      !lamb = toLC expr

  bound <- ExceptT $ pure (bindLC lamb)

  cplx <- liftIO (Lamb.moonStrict bound)

  traceM ""
  traceM (show lamb)
  traceM ""

  traceM ""
  traceM (ppShow cplx)
  traceM ""

  pure (Ur.simp cplx)


gogogo'new :: Text -> ExceptT Text IO JetEval.Exp
gogogo'new text = do
  ast <- ExceptT $ pure $ Parser.parseAST text

  traceM ""
  traceM (show ast)
  traceM ""

  let !expr = bind ast
      !lamb = toLC expr

  bound <- ExceptT $ pure (bindLC lamb)

  cplx <- liftIO (Lamb.moonStrict bound)

  traceM ""
  traceM (show lamb)
  traceM ""

  traceM ""
  traceM (ppShow cplx)
  traceM ""

  pure (JetEval.eval cplx)


gogogoFast :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoFast text = do
  ast <- ExceptT $ pure $ Parser.parseAST text

  let expr = bind ast
      lamb = toLC expr

  bound <- ExceptT $ pure (bindLC lamb)
  cplx  <- liftIO $ Lamb.moonStrict bound

  pure cplx

gogogoOleg :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoOleg text = do
  ast <- ExceptT $ pure $ Parser.parseAST text
  let expr = bind ast
  let lamb = toLC expr
  resu <- liftIO $ Lamb.moonStrict lamb
  ExceptT (pure resu)


gogogoLazyOleg :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoLazyOleg text = do
  ast <- ExceptT $ pure $ Parser.parseAST text
  let expr = bind ast
  let lamb = toLC expr
  resu <- liftIO $ Lamb.moonLazy lamb
  ExceptT (pure resu)

gogogoTromp :: forall p. (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoTromp text = do
  ast <- ExceptT $ pure $ Parser.parseAST text
  let expr = bind ast
  let lamb = moonToLambda expr :: B.Exp () (Either Text p)
  ExceptT $ B.outToUruk $ B.johnTrompBracket $ makeStrict Right lamb

gogogoLazyTromp :: forall p. (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoLazyTromp text = do
  ast <- ExceptT $ pure $ Parser.parseAST text
  let expr = bind ast
  let lamb = moonToLambda expr :: B.Exp () (Either Text p)
  ExceptT $ B.outToUruk $ B.johnTrompBracket lamb

bindLC :: Uruk p => Lamb.Exp (Either Text p) -> Either Text (Lamb.Exp p)
bindLC = traverse (either getGlobal Right)

toLC :: forall p. Uruk p => Exp Text -> Lamb.Exp (Either Text p)
toLC = go Left
 where
  go :: (a -> Either Text Nat) -> Exp a -> Lamb.Exp (Either Text p)
  go f = \case
    Var a     -> var f a
    Lam b     -> lam f b
    App x y   -> Lamb.Go (go f x) (go f y)
    Jet r n b -> Lamb.Jet (fromIntegral r) (Atom.utf8Atom n) (go f b)
    Fix b     -> Lamb.Loop (enter f b)
    Sig       -> Lamb.Prim (Right uUni)
    Con x y   -> con f x y
    Cas x l r -> cas f x l r
    Iff c t e -> Lamb.If (go f c) (go f t) (go f e)
    Lit n     -> Lamb.Prim $ Right $ uNat n
    Bol b     -> Lamb.Prim $ Right $ uBol b
    Str n     -> Lamb.Prim $ Right $ uNat $ Atom.utf8Atom n

  enter :: (a -> Either Text Nat) -> Scope () Exp a -> Lamb.Exp (Either Text p)
  enter f b = go f' (fromScope b) where f' = wrap f

  lam :: (a -> Either Text Nat) -> Scope () Exp a -> Lamb.Exp (Either Text p)
  lam f b = Lamb.Lam (enter f b)

  var f a = f a & \case
    Left  e -> Lamb.Prim (Left e)
    Right v -> Lamb.Var v

  cas :: (a -> Either Text Nat) -> Exp a -> Scope () Exp a -> Scope () Exp a -> Lamb.Exp (Either Text p)
  cas f x l r = Lamb.Case (go f x) (enter f l) (enter f r)

  con :: (a -> Either Text Nat) -> Exp a -> Exp a -> Lamb.Exp (Either Text p)
  con f x y = Lamb.Prim (Right uCon) `Lamb.Go` go f x `Lamb.Go` go f y

  wrap :: (a -> Either Text Nat) -> Var () a -> Either Text Nat
  wrap f = \case
    B () -> Right 0
    F x  -> succ <$> f x
