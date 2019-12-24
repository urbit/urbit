module Deppy.Demo where

import ClassyPrelude hiding (putStrLn)
import Prelude (putStrLn)

import Data.Function ((&))
import Text.Show.Pretty

import Deppy.Parser
import Deppy.Showings
import Deppy.CST as C
import Deppy.Hoon as H
import Deppy.Core
import Deppy.ToUntyped
import Untyped.Core (copy)

demo :: Text -> IO ()
demo prog = parseCst prog & \case
  Left err -> putStrLn ("parse error: " <> unpack err)
  Right c -> do
    putStrLn ("parsed: " <> ppShow c)
    let h = C.abstractify c
    putStrLn ("ast: " <> display h)
    let e = H.desugar h
    putStrLn ("core: " <> display e)
    let t = infer env e
    case t of
      Just t -> putStrLn ("type: " <> display (H.resugar' t))
      Nothing -> putStrLn "<type error>"
    let n = copy $ toUntyped e
    putStrLn ("nock: " <> show n)

env v = error ("error: free variable: " <> show v)
