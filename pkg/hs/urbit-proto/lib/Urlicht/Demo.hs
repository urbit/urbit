module Urlicht.Demo where

import ClassyPrelude hiding (putStrLn)
import Prelude (putStrLn)

import Control.Monad.State.Strict
import Data.Function ((&))

import Deppy.CST as CST
import Deppy.Hoon as H
import Deppy.Parser
import Deppy.RunicShow
import qualified Urlicht.Core as C
import Urlicht.DisplayOrphans()
import Urlicht.Elab
import Urlicht.Elaborate
import qualified Urlicht.HoonToSimple as H2S
import qualified Urlicht.Simple as S
import Urlicht.Unify

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

