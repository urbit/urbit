module Moon.Repl
  ( repl
  , replFast
  , runFile
  , runFileFast
  , runText
  )
where

import Bound
import ClassyPrelude
import Control.Monad.Except
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

runFile' :: Show a => (Text -> IO (Either Text a)) -> FilePath -> IO ()
runFile' go fp = do
  txt <- readFile fp
  _   <- runText' go (decodeUtf8 txt)
  pure ()

runText' :: Show a => (Text -> IO (Either Text a)) -> Text -> IO ()
runText' go txt = do
  res <- go txt
  res & \case
    Left  err -> putStrLn err
    Right res -> putStrLn $ pack $ show res

repl' :: Show a => (Text -> IO (Either Text a)) -> IO ()
repl' go = HL.runInputT HL.defaultSettings loop
 where
  loop :: HL.InputT IO ()
  loop = do
    minput <- HL.getInputLine "moon> "
    case minput of
      Nothing    -> return ()
      Just "!!"  -> return ()
      Just input -> do
        res <- liftIO $ go (pack input)
        let !resStr = res & \case
              Left  err -> unpack err
              Right res -> show res
        HL.outputStrLn resStr
        loop

--------------------------------------------------------------------------------

goSlow :: Text -> IO (Either Text Ur.Ur)
goSlow = fmap (fmap Ur.unClose) . runExceptT . MU.gogogo'

goFast :: Text -> IO (Either Text F.Val)
goFast = runExceptT . MU.gogogoFast

--------------------------------------------------------------------------------

runText :: Text -> IO ()
runText = runText' goSlow

replFast :: IO ()
replFast = repl' goFast

repl :: IO ()
repl = repl' goSlow

runFile :: FilePath -> IO ()
runFile = runFile' goSlow

runFileFast :: FilePath -> IO ()
runFileFast = runFile
