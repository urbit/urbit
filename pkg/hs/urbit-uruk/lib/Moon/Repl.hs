module Moon.Repl (repl, runFile, runText, main) where

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
    [fn] -> runFile (unpack fn)
    _    -> putStrLn "usage: moon [file]"

runFile :: FilePath -> IO ()
runFile fp = do
  txt <- readFile fp
  _   <- runText mempty (decodeUtf8 txt)
  pure ()

runText :: Ctx -> Text -> IO Ctx
runText ctx txt = do
  MU.gogogo' txt & \case
    Left err -> do
      putStrLn err
      pure ctx
    Right res -> do
      putStrLn $ pack $ ppShow (Ur.unClose res)
      pure ctx

runText_ :: Text -> IO ()
runText_ = void . runText mempty

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
              Left  err -> unpack err
              Right res -> ppShow (Ur.unClose res)
        HL.outputStrLn resStr
        go
