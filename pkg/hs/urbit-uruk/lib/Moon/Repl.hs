module Moon.Repl (repl, main) where

import Bound
import ClassyPrelude
import GHC.Natural
import Moon.AST

import Control.Arrow    ((>>>))
import Data.Function    ((&))
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (ppShow)
import Uruk.OptToFast   (optToFast)

import qualified Moon.MoonToUruk          as MU
import qualified Moon.Parser              as Parser
import qualified System.Console.Haskeline as HL
import qualified Urbit.Atom               as Atom
import qualified Uruk.Fast                as F
import qualified Uruk.JetComp             as Uruk
import qualified Uruk.JetDemo             as Ur
import qualified Uruk.JetOptimize         as Opt

--------------------------------------------------------------------------------

type Ctx = Map Text F.Val

main :: IO ()
main = do
  getArgs >>= \case
    []   -> repl

    [fn] -> do
      txt <- readFile (unpack fn)
      _   <- runText mempty (decodeUtf8 txt)
      pure ()

    _ -> do
      putStrLn "usage: moon [file]"

runText :: Ctx -> Text -> IO Ctx
runText ctx txt = do
  let !res = MU.gogogo txt
  print res
  pure ctx

repl :: IO ()
repl = HL.runInputT HL.defaultSettings go
 where
  go :: HL.InputT IO ()
  go = do
    minput <- HL.getInputLine "moon> "
    case minput of
      Nothing    -> return ()
      Just "!!"  -> return ()
      Just input -> do
        let !resStr = MU.gogogo' (pack input) & \case
              Left err -> unpack err
              Right res -> ppShow (Ur.unClose res)
        HL.outputStrLn resStr
        go
