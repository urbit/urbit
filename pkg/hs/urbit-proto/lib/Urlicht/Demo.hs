module Urlicht.Demo where

import ClassyPrelude

import Control.Arrow ((>>>))

import Urlicht.CST as CST
--import Urlicht.Hoon as H
import Urlicht.Parser
import Urlicht.RunicShow
import qualified Urlicht.Core as C
import Urlicht.Elab
import Urlicht.Elaborate
import Urlicht.Env
import qualified Urlicht.HoonToSimple as H2S
--import qualified Urlicht.Simple as S
import qualified Urlicht.SimpleToCoreHack as S2C
import Urlicht.Unify

udemo :: Text -> IO ()
udemo prog = runElabIO do
  s <- H2S.down . CST.abstractify <$> parse prog
  putStrLn ("SIMPLE:\n" <> runic s)
  (t, c) <- infer emptyEnv s
  t <- crank t
  c <- zonk c
  putStrLn ("ELABORATED:\n" <> runic c)
  putStrLn ("TYPE:\n" <> runic t)
  tell

dunify :: Text -> Text -> IO ()
dunify p1 p2 = runElabIO do
  v1 <- C.eval . S2C.down . H2S.down . CST.abstractify <$> parse p1
  v2 <- C.eval . S2C.down . H2S.down . CST.abstractify <$> parse p2
  unify v1 v2
  tell

parse :: Text -> ElabT IO CST
parse = parseCst >>> \case
  Left err -> terror ("parser: " <> err)
  Right c -> pure c
