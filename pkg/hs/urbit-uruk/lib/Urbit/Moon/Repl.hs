{-# LANGUAGE CPP #-}

module Urbit.Moon.Repl
  ( runFile
  , runFileFast
  , runText
#if !defined(__GHCJS__)
  , replRefr
  , replSlow
  , replFast
#endif
  , evalText
  , evalTextFast
  , evalTextUruk
  )
where

import Prelude ()
import Bound
import ClassyPrelude
import Control.Monad.Except
import GHC.Natural
import Urbit.Moon.AST
import Codec.Serialise

import Control.Arrow             ((>>>))
import Data.Function             ((&))
import System.Directory          (createDirectoryIfMissing, getHomeDirectory,
                                  listDirectory, removeFile)
import System.IO.Unsafe          (unsafePerformIO)
import Text.Show.Pretty          (ppShow)
import Urbit.Uruk.Fast.OptToFast (optToFast)
import Urbit.Uruk.UrukDemo       (Env, EvalResult(..), Inp(..), InpResult(..))
import Urbit.Uruk.UrukDemo       (execInp, execText, parseInps, printInpResult)

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

import qualified Urbit.Uruk.UrukDemo as Dem

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

goInp :: Text -> IO (Either Text [Inp])
goInp = pure . parseInps

--------------------------------------------------------------------------------

runText :: Text -> IO ()
runText = runText' goSlow

#if !defined(__GHCJS__)
replFast :: IO ()
replFast = repl' goFast

replSlow :: IO ()
replSlow = repl' goSlow

replRefr :: IO ()
replRefr = do
  vEnv <- readUrukDemoEnv >>= newIORef
  HL.runInputT HL.defaultSettings (loop vEnv)
 where
  loop :: IORef Env -> HL.InputT IO ()
  loop vEnv = do
    minput <- HL.getInputLine "uruk> "
    case minput of
      Nothing    -> return ()
      Just "!!"  -> return ()
      Just input -> do
        parseInps (pack input) & \case
          Left err -> do
            liftIO $ putStrLn err
            loop vEnv
          Right inps -> do
            liftIO $ doInps vEnv inps
            loop vEnv

  doInps :: IORef Env -> [Inp] -> IO ()
  doInps vEnv []     = pure ()
  doInps vEnv (i:is) = do
    env <- readIORef vEnv
    case execInp env i of
      Left err -> putStrLn err
      Right (env', res) -> do
        writeIORef vEnv env'
        printInpResult res
        persistInpResu res
        doInps vEnv is
#endif

runFile :: FilePath -> IO ()
runFile = runFile' goSlow

runFileFast :: FilePath -> IO ()
runFileFast = runFile' goFast

evalTextUruk :: Text -> IO Text
evalTextUruk = evalText' goInp

evalText :: Text -> IO Text
evalText = evalText' goSlow

evalTextFast :: Text -> IO Text
evalTextFast = evalText' goFast


-- Types -----------------------------------------------------------------------

-- | Identifier, should match /[a-zA-Z0-9$-]+/
type Ident = Text


-- Read/Write + Serialise/Deserialise ------------------------------------------

persistInpResu :: InpResult -> IO ()
persistInpResu = \case
  InpExpr _ _                  -> pure ()
  InpWipe n                    -> writeUrukDemoBind n Nothing
  InpDecl n _ (EvalResult r _) -> writeUrukDemoBind n (Just r)

writeUrukDemoBind :: Ident -> Maybe Dem.Exp -> IO ()
writeUrukDemoBind iden mExp =
  writeUrukDemoBindBytes iden (toStrict . serialise <$> mExp)

--  Throws `DeserialiseFailure` exception on decode failure.
readUrukDemoEnv :: IO (Map Ident Dem.Exp)
readUrukDemoEnv = readUrukDemoEnvBytes >>= traverse cvt
 where
  cvt :: ByteString -> IO Dem.Exp
  cvt byt = evaluate $ force $ deserialise $ fromStrict byt


-- Reading and writing bind files ----------------------------------------------

readUrukDemoEnvBytes :: IO (Map Ident ByteString)
readUrukDemoEnvBytes = do
  hom <- getHomeDirectory
  let dir = hom <> "/.uruk/uruk-demo/"
  createDirectoryIfMissing True dir
  fmap mapFromList $ listDirectory dir >>= traverse (loadEnt dir)
 where
  loadEnt :: FilePath -> String -> IO (Ident, ByteString)
  loadEnt dir ent = do
    let pax = dir <> "/" <> ent
    byt <- readFile pax
    pure (pack ent, byt)

writeUrukDemoBindBytes :: Ident -> Maybe ByteString -> IO ()
writeUrukDemoBindBytes iden mByt = do
  hom <- getHomeDirectory
  let dir = hom <> "/.uruk/uruk-demo/"
  let pax = dir <> unpack iden
  createDirectoryIfMissing True dir
  case mByt of
    Just bytz -> writeFile pax bytz
    Nothing   -> removeFile pax
