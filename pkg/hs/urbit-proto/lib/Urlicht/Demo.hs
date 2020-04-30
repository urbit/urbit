module Urlicht.Demo where

import ClassyPrelude --hiding (putStrLn)
--import Prelude (putStrLn)

import Control.Monad.State.Strict
import Data.Function ((&))

import Urlicht.CST as CST
--import Urlicht.Hoon as H
import Urlicht.Parser
import Urlicht.RunicShow
import qualified Urlicht.Core as C
import Urlicht.Elab
import Urlicht.Elaborate
import qualified Urlicht.HoonToSimple as H2S
--import qualified Urlicht.Simple as S
import qualified Urlicht.SimpleToCoreHack as S2C
import Urlicht.Unify

udemo :: Text -> IO ()
udemo prog = parseCst prog & \case
  Left err -> putStrLn ("parse error: " <> err)
  Right c -> do
    let s = H2S.down $ CST.abstractify c
    putStrLn ("SIMPLE:\n" <> runic s)
    let
      (t, c, ms) = runElab do
        (t, c) <- infer [] s
        (,,) <$> crank t <*> zonk c <*> get
    putStrLn ("ELABORATED:\n" <> runic c)
    putStrLn ("TYPE:\n" <> runic t)
    putStrLn ("ELAB STATE:\n" <> runic ms)

dunify :: Text -> Text -> IO ()
dunify p1 p2 = (,) <$> parseCst p1 <*> parseCst p2 & \case
  Left err -> putStrLn ("parse error: " <> err)
  Right (c1, c2) -> do
    let v1 = C.eval $ S2C.down $ H2S.down $ CST.abstractify c1
    let v2 = C.eval $ S2C.down $ H2S.down $ CST.abstractify c2
    let
      ms = runElab do
        unify v1 v2
        get
    putStrLn ("METAS\n" <> runic ms)


