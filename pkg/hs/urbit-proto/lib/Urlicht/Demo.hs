module Urlicht.Demo where

import ClassyPrelude hiding (putStrLn)
import Prelude (putStrLn)

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
--import Urlicht.Unify

udemo :: Text -> IO ()
udemo prog = parseCst prog & \case
  Left err -> putStrLn ("parse error: " <> unpack err)
  Right c -> do
    let s = H2S.down $ CST.abstractify c
    putStrLn ("SIMPLE:\n" <> runic s)
    let
      (t, c) = runElab do
        (t, c) <- infer [] s
        t <- crank t
        c <- zonk c
        ms <- gets _metas
        pure $ trace (show $ keysSet ms) (t, c)
    putStrLn ("ELABORATED:\n" <> show c)
    putStrLn ("TYPE:\n" <> show t)

delab :: Text -> Text -> IO ()
delab p1 p2 = (,) <$> parseCst p1 <*> parseCst p2 & \case
  Left err -> putStrLn ("parse error: " <> unpack err)
  Right (c1, c2) -> do
    let v1 = C.eval $ S2C.down $ H2S.down $ CST.abstractify c1
    let v2 = C.eval $ S2C.down $ H2S.down $ CST.abstractify c2
    undefined

