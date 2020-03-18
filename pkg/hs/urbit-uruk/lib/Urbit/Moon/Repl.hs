{-# LANGUAGE CPP #-}

module Urbit.Moon.Repl
  ( runFile
  , runFileFast
  , runText
#if !defined(__GHCJS__)
  , repl
  , replFast
#endif
  , evalText
  , evalTextFast
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

#if !defined(__GHCJS__)
import qualified System.Console.Haskeline    as HL
#endif
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

#if !defined(__GHCJS__)
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
#endif

--------------------------------------------------------------------------------

goSlow :: Text -> IO (Either Text Ur.Ur)
goSlow = fmap (fmap Ur.unClose) . runExceptT . MU.gogogo'

goFast :: Text -> IO (Either Text F.Val)
goFast = runExceptT . MU.gogogoFast

--------------------------------------------------------------------------------

runText :: Text -> IO ()
runText = runText' goSlow

#if !defined(__GHCJS__)
replFast :: IO ()
replFast = repl' goFast

repl :: IO ()
repl = repl' goSlow
#endif

runFile :: FilePath -> IO ()
runFile = runFile' goSlow

runFileFast :: FilePath -> IO ()
runFileFast = runFile' goFast

evalText :: Text -> IO Text
evalText = evalText' goSlow

evalTextFast :: Text -> IO Text
evalTextFast = evalText' goFast
