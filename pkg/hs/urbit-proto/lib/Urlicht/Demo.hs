module Urlicht.Demo where

import ClassyPrelude hiding (putStrLn)
import Prelude (putStrLn)

import Data.Function ((&))

import Deppy.CST as CST
import Deppy.Hoon as H
import Deppy.Parser
import Deppy.RunicShow
import qualified Urlicht.Core as C
import Urlicht.DisplayOrphans()
import qualified Urlicht.HoonToSimple as H2S
import qualified Urlicht.Simple as S

udemo :: Text -> IO ()
udemo prog = parseCst prog & \case
  Left err -> putStrLn ("parse error: " <> unpack err)
  Right c -> do
    let s = H2S.down $ CST.abstractify c
    putStrLn ("SIMPLE:\n" <> runic s)
