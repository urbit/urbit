module Deppy.Demo where

import ClassyPrelude hiding (putStrLn)
import Prelude (putStrLn)

import Data.Function    ((&))
-- ort Text.Show.Pretty (ppShow)

import Deppy.Core
import Deppy.Parser hiding (Wide)
import Deppy.Showings
import Deppy.RunicShow
import Deppy.ToUntyped
import Untyped.Core    (copy)

import qualified Deppy.CST  as C
import qualified Deppy.Hoon as H

demo :: Text -> IO ()
demo prog = parseCst prog & \case
  Left err -> putStrLn ("parse error: " <> unpack err)
  Right c -> do
    let h = C.abstractify c
    let e = H.desugar h
    putStrLn ("CORE:\n" <> runic e)
    let t = infer env e
    case t of
      Right t -> putStrLn ("TYPE:\n" <> runic t)
      Left er -> putStrLn ("<type error>: " <> runic er)
    let w = whnf e
    putStrLn ("WHNF:\n" <> runic (H.resugar' w))
    let n = copy $ toUntyped e
    putStrLn ("nock: " <> show n)
  where
    env v = error ("error: free variable: " <> show v)

demo' :: Text -> IO ()
demo' prog = parseCst prog & \case
  Left err -> putStrLn ("parse error: " <> unpack err)
  Right c -> do
    putStrLn ("parsed: " <> display c)
    let h = C.abstractify c
    putStrLn ("ast: " <> display h)
    let e = H.desugar h
    putStrLn ("core: " <> display e)
    let t = infer env e
    case t of
      Right t -> putStrLn ("type: " <> display t)
      Left er -> putStrLn ("<type error>: " <> show er)
    let n = copy $ toUntyped e
    putStrLn ("nock: " <> show n)
  where
    env v = error ("error: free variable: " <> show v)

filo :: FilePath -> Text -> IO ()
filo fn expr = do
  decls <- readFileUtf8 fn
  demo (decls <> "\n" <> expr)

filo' :: FilePath -> Text -> IO ()
filo' fn expr = do
  decls <- readFileUtf8 fn
  demo' (decls <> "\n" <> expr)
