module Urbit.Moon.MoonToUruk where

import Bound
import ClassyPrelude
import Control.Monad.Except
import GHC.Natural
import Urbit.Moon.AST
import Urbit.Uruk.Class

import Control.Arrow             ((>>>))
import Data.Function             ((&))
import Data.Void                 (Void)
import System.IO.Unsafe          (unsafePerformIO)
import Text.Show.Pretty          (ppShow)
import Urbit.Moon.Arity          (Arity)
import Urbit.Moon.MakeStrict     (makeStrict)
import Urbit.Moon.MoonToLambda   (moonToLambda)
import Urbit.Moon.Oleg           (oleg)
import Urbit.Uruk.Fast.OptToFast (optToFast)

import qualified Urbit.Atom              as Atom
import qualified Urbit.Moon.AST          as AST
import qualified Urbit.Moon.Bracket      as B
import qualified Urbit.Moon.LambdaToUruk as Lamb
import qualified Urbit.Moon.MakeStrict   as B
import qualified Urbit.Moon.Parser       as Parser
import qualified Urbit.Uruk.Fast         as F
import qualified Urbit.Uruk.JetEval      as JetEval
import qualified Urbit.Uruk.Refr.Jetted  as Ur

--------------------------------------------------------------------------------

data CompileTrace p = CompileTrace
  { ctInpu :: Text
  , ctTree :: AST
  , ctBind :: AST.Exp Text
  , ctLamb :: B.Exp p () Text
  , ctMuck :: B.Exp p () Void
  , ctStik :: B.Exp p () Void
  , ctDone :: p
  }

resolve
  :: (Text -> Either Text p) -> B.Exp p () Text -> Either Text (B.Exp p () Void)
resolve f = fmap mergePrim . traverse f

mergePrim :: forall p b . B.Exp p b p -> B.Exp p b Void
mergePrim = go B.Pri
 where
  go :: (a -> B.Exp p b c) -> B.Exp p b a -> B.Exp p b c
  go f = \case
    B.Pri p      -> B.Pri p
    B.Var v      -> f v
    x     B.:@ y -> go f x B.:@ go f y
    B.Lam b    x -> B.Lam b $ toScope $ go (wrap f) $ fromScope x

  wrap :: (a -> B.Exp p b c) -> Var b a -> B.Exp p b (Var b c)
  wrap f (B b) = B.Var (B b)
  wrap f (F v) = F <$> f v

compile
  :: Uruk p
  => (Text -> Either Text p)
  -> (B.Exp p () Void -> B.Exp p () Void)
  -> (B.Exp p () Void -> p)
  -> Text
  -> Either Text (CompileTrace p)
compile lkup strict comp ctInpu = do
  ctTree <- Parser.parseAST ctInpu
  let ctBind = AST.bind ctTree
  let ctLamb = moonToLambda ctBind
  ctMuck <- resolve lkup ctLamb
  let ctStik = strict ctMuck
  let ctDone = comp ctStik
  pure (CompileTrace{..})

usApp :: Uruk p => p -> p -> p
usApp x y = unsafePerformIO (uApp x y)

urukPrim :: Uruk p => B.Prim p
urukPrim = B.Prim
  { pSeq = uSeq
  , pYet = uYet . fromIntegral
  , pKay = uKay
  , pApp = usApp
  , pArg = uArity
  }

urukOut :: Uruk p => (p, p, p -> p -> p)
urukOut = (uEss, uKay, usApp)


-- Examples --------------------------------------------------------------------

strictBracket :: (Uruk p, Eq p) => Text -> Either Text (CompileTrace p)
strictBracket =
  compile getGlobal (makeStrict urukPrim) (B.outToUruk urukOut . B.naiveBracket)

lazyBracket :: (Uruk p, Eq p) => Text -> Either Text (CompileTrace p)
lazyBracket = compile getGlobal id (B.outToUruk urukOut . B.naiveBracket)

strictTromp :: (Uruk p, Eq p) => Text -> Either Text (CompileTrace p)
strictTromp = compile getGlobal
                      (makeStrict urukPrim)
                      (B.outToUruk urukOut . B.johnTrompBracket)

lazyTromp :: (Uruk p, Eq p) => Text -> Either Text (CompileTrace p)
lazyTromp = compile getGlobal id (B.outToUruk urukOut . B.johnTrompBracket)

strictOleg :: (Uruk p, Eq p) => Text -> Either Text (CompileTrace p)
strictOleg = compile getGlobal (makeStrict urukPrim) oleg

lazyOleg :: (Uruk p, Eq p) => Text -> Either Text (CompileTrace p)
lazyOleg = compile getGlobal id oleg


-- Entry Points ----------------------------------------------------------------

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

-- Entry Points ----------------------------------------------------------------

gogogoOleg :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoOleg = fmap ctDone . ExceptT . pure . strictOleg

gogogoLazyOleg :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoLazyOleg = fmap ctDone . ExceptT . pure . lazyOleg

gogogoTromp :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoTromp = fmap ctDone . ExceptT . pure . strictTromp

gogogoLazyTromp :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoLazyTromp = fmap ctDone . ExceptT . pure . lazyTromp

gogogoNaive :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoNaive = fmap ctDone . ExceptT . pure . strictBracket

gogogoLazyNaive :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoLazyNaive =  fmap ctDone . ExceptT . pure . lazyBracket

gogogo' :: Text -> ExceptT Text IO Ur.Ur
gogogo' = ExceptT . pure . fmap (Ur.simp . ctDone) . strictOleg

gogogo'new :: Text -> ExceptT Text IO JetEval.Exp
gogogo'new = ExceptT . pure . fmap (JetEval.eval . ctDone) . strictOleg

gogogoFast :: (Eq p, Show p, Uruk p) => Text -> ExceptT Text IO p
gogogoFast = gogogoOleg
