{-- OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.MoonToUruk where

import Bound
import ClassyPrelude
import Control.Monad.Except
import Urbit.Moon.AST
import Urbit.Uruk.Class

import Bound.Var                 (unvar)
import Data.Void                 (Void)
import System.IO.Unsafe          (unsafePerformIO)
import Urbit.Moon.MakeStrict     (makeStrict)
import Urbit.Moon.MoonToLambda   (moonToLambda)
import Urbit.Moon.Oleg           (oleg)
import Text.Show.Pretty (ppShow)

import qualified Urbit.Moon.AST          as AST
import qualified Urbit.Moon.Bracket      as B
import qualified Urbit.Moon.MakeStrict   as B
import qualified Urbit.Moon.Parser       as Parser
import qualified Urbit.Uruk.JetEval      as JetEval


--------------------------------------------------------------------------------

data CompileTrace p = CompileTrace
  { ctTree :: AST
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
  wrap g = unvar (B.Var . B) (fmap F . g)

compileAST
  :: Uruk p
  => (Text -> Either Text p)
  -> (B.Exp p () Void -> B.Exp p () Void)
  -> (B.Exp p () Void -> p)
  -> AST
  -> Either Text (CompileTrace p)
compileAST lkup strict comp ctTree = do
  let ctBind = AST.bind ctTree
  let ctLamb = moonToLambda ctBind
  ctMuck <- resolve lkup ctLamb
  let ctStik = strict ctMuck
  let ctDone = comp ctStik
  pure (CompileTrace{..})


-- Compile Expression ----------------------------------------------------------

compile
  :: Uruk p
  => (Text -> Either Text p)
  -> (B.Exp p () Void -> B.Exp p () Void)
  -> (B.Exp p () Void -> p)
  -> Text
  -> Either Text (CompileTrace p)
compile lkup strict comp inpu = do
  ctTree <- Parser.parseAST inpu
  compileAST lkup strict comp ctTree


-- Compile File ----------------------------------------------------------------

compileFile
  :: forall p
   . Uruk p
  => (Text -> Either Text p)
  -> (B.Exp p () Void -> B.Exp p () Void)
  -> (B.Exp p () Void -> p)
  -> (p -> p)
  -> Text
  -> Either Text p
compileFile lkup strict comp eval inpu = do
  tree <- Parser.parseAST inpu
  let AST.File ds e = AST.astFile tree
  go mempty ds e
 where
  go :: Map Text p -> [AST.Decl] -> AST -> Either Text p
  go env []              e = doOne env e
  go env (Decl n v : ds) e = do
    traceM ("[" <> unpack n <> "]")
    val <- doOne env v
    traceM (ppShow val)
    traceM ""
    go (insertMap n val env) ds e

  doOne :: Map Text p -> AST -> Either Text p
  doOne env = fmap (eval . ctDone) . compileAST (get env) strict comp

  get :: Map Text p -> Text -> Either Text p
  get env k = case lookup k env of
    Just xv -> pure xv
    Nothing -> lkup k




usApp :: Uruk p => p -> p -> p
usApp x y = unsafePerformIO (uApp x y)

urukPrim :: Uruk p => B.Prim p
urukPrim = B.Prim
  { pSeq = uSeq
  , pEye = uEye . fromIntegral
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

strictBracketFile :: (Uruk p, Eq p) => (p -> p) -> Text -> Either Text p
strictBracketFile = compileFile getGlobal
                                (makeStrict urukPrim)
                                (B.outToUruk urukOut . B.naiveBracket)

lazyBracketFile :: (Uruk p, Eq p) => (p -> p) -> Text -> Either Text p
lazyBracketFile =
  compileFile getGlobal id (B.outToUruk urukOut . B.naiveBracket)

strictTrompFile :: (Uruk p, Eq p) => (p -> p) -> Text -> Either Text p
strictTrompFile = compileFile getGlobal
                              (makeStrict urukPrim)
                              (B.outToUruk urukOut . B.johnTrompBracket)

lazyTrompFile :: (Uruk p, Eq p) => (p -> p) -> Text -> Either Text p
lazyTrompFile =
  compileFile getGlobal id (B.outToUruk urukOut . B.johnTrompBracket)

strictOlegFile :: (Uruk p, Eq p) => (p -> p) -> Text -> Either Text p
strictOlegFile = compileFile getGlobal (makeStrict urukPrim) oleg

lazyOlegFile :: (Uruk p, Eq p) => (p -> p) -> Text -> Either Text p
lazyOlegFile = compileFile getGlobal id oleg



-- Entry Points ----------------------------------------------------------------

getGlobal :: Uruk p => Text -> Either Text p
getGlobal = \case
  "S"     -> Right uEss
  "K"     -> Right uKay
  "J"     -> Right (uJay 1)
  "const" -> Right uKay
  "D"     -> Right uDee
  "I"     -> Right (uEye 1)
  "id"    -> Right (uEye 1)
  "B"     -> Right (uBee 1)
  "dot"   -> Right (uBee 1)
  "C"     -> Right (uSea 1)
  "flip"  -> Right (uSea 1)
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

gogogoOleg :: (Eq p, Uruk p) => Text -> ExceptT Text IO p
gogogoOleg = ExceptT . pure . strictOlegFile id

gogogoLazyOleg :: (Eq p, Uruk p) => Text -> ExceptT Text IO p
gogogoLazyOleg = ExceptT . pure . lazyOlegFile id

gogogoTromp :: (Eq p, Uruk p) => Text -> ExceptT Text IO p
gogogoTromp = ExceptT . pure . strictTrompFile id

gogogoLazyTromp :: (Eq p, Uruk p) => Text -> ExceptT Text IO p
gogogoLazyTromp = ExceptT . pure . lazyTrompFile id

gogogoNaive :: (Eq p, Uruk p) => Text -> ExceptT Text IO p
gogogoNaive = ExceptT . pure . strictBracketFile id

gogogoLazyNaive :: (Eq p, Uruk p) => Text -> ExceptT Text IO p
gogogoLazyNaive = ExceptT . pure . lazyBracketFile id

gogogo'new :: Text -> ExceptT Text IO JetEval.Exp
gogogo'new = ExceptT . pure . strictOlegFile JetEval.eval
