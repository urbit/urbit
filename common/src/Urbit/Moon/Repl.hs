module Urbit.Moon.Repl
  ( runFile
  , runFileFast
  , evalText
  , evalTextFast
  , runText
  )
where

import Prelude ()
import Bound
import ClassyPrelude
import Control.Monad.Except
import GHC.Natural
import Urbit.Moon.AST

import Control.Arrow             ((>>>))
import Data.Function             ((&))
import System.IO.Unsafe          (unsafePerformIO)
import Text.Show.Pretty          (ppShow)
import Urbit.Uruk.Fast.OptToFast (optToFast)

import qualified Urbit.Atom                  as Atom
import qualified Urbit.Moon.LambdaToUruk     as Lamb
import qualified Urbit.Moon.MoonToUruk       as MU
import qualified Urbit.Moon.Parser           as Parser
import qualified Urbit.Uruk.Fast             as F
import qualified Urbit.Uruk.Fast.Types       as F
import qualified Urbit.Uruk.Refr.Jetted      as Ur

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

evalText' :: Show a => (Text -> IO (Either Text a)) -> Text -> IO Text
evalText' go txt = do
  res <- go txt
  res & \case
    Left  err -> pure err
    Right res -> pure (pack (show res))

--------------------------------------------------------------------------------

goSlow :: Text -> IO (Either Text Ur.Ur)
goSlow = fmap (fmap Ur.unClose) . runExceptT . MU.gogogo'

goFast :: Text -> IO (Either Text F.Val)
goFast = runExceptT . MU.gogogoFast

--------------------------------------------------------------------------------

runText :: Text -> IO ()
runText = runText' goSlow

runTextFast :: Text -> IO ()
runTextFast = runText' goFast

evalText :: Text -> IO Text
evalText = evalText' goSlow

evalTextFast :: Text -> IO Text
evalTextFast = evalText' goFast

runFile :: FilePath -> IO ()
runFile = runFile' goSlow

runFileFast :: FilePath -> IO ()
runFileFast = runFile' goFast
