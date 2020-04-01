{-# LANGUAGE CPP #-}

module Urbit.Moon.Repl where

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
import qualified System.Console.Haskeline as HL
#endif
import qualified Urbit.Atom               as Atom
import qualified Urbit.Moon.LambdaToUruk  as Lamb
import qualified Urbit.Moon.MoonToUruk    as MU
import qualified Urbit.Moon.Parser        as Parser
import qualified Urbit.Uruk.Fast          as F
import qualified Urbit.Uruk.Fast.Types    as F
import qualified Urbit.Uruk.JetEval       as JetEval
import qualified Urbit.Uruk.Refr.Jetted   as Ur

import qualified Urbit.Uruk.UrukDemo as Dem


--------------------------------------------------------------------------------

runFile' :: Show a => (Text -> IO (Either Text a)) -> FilePath -> IO ()
runFile' go fp = do
  txt <- readFile fp
  _   <- runText' go (decodeUtf8 txt)
  pure ()

runFile'' :: (Text -> IO (Either Text Text)) -> FilePath -> IO ()
runFile'' go fp = do
  txt <- readFile fp
  _   <- runText'' go (decodeUtf8 txt)
  pure ()

runText' :: Show a => (Text -> IO (Either Text a)) -> Text -> IO ()
runText' go txt = do
  res <- go txt
  res & \case
    Left  err -> putStrLn err
    Right res -> putStrLn $ pack $ show res

runText'' :: (Text -> IO (Either Text Text)) -> Text -> IO ()
runText'' go txt = go txt >>= either putStrLn putStrLn

evalText' :: Show a => (Text -> IO (Either Text a)) -> Text -> IO Text
evalText' go txt = do
  res <- go txt
  res & \case
    Left  err -> pure err
    Right res -> pure (pack (show res))

#if !defined(__GHCJS__)
repl' :: Show a => Text -> (Text -> IO (Either Text a)) -> IO ()
repl' prompt go = repl'' prompt ((fmap . fmap . fmap $ tshow) go)

repl'' :: Text -> (Text -> IO (Either Text Text)) -> IO ()
repl'' prompt go = HL.runInputT HL.defaultSettings loop
 where
  loop :: HL.InputT IO ()
  loop = do
    minput <- HL.getInputLine ps1
    case minput of
      Nothing    -> return ()
      Just "!!"  -> return ()
      Just input -> do
        res <- liftIO $ go (pack input)
        let !resStr = res & \case
              Left  err -> unpack err
              Right res -> unpack res
        HL.outputStrLn resStr
        loop
  ps1 = unpack prompt <> "> "
#endif

--------------------------------------------------------------------------------

goSlow :: Text -> IO (Either Text Ur.Ur)
goSlow = fmap (fmap Ur.unClose) . runExceptT . MU.gogogo'

goNew :: Text -> IO (Either Text JetEval.Exp)
goNew = runExceptT . MU.gogogo'new

goFast :: Text -> IO (Either Text F.Val)
goFast = runExceptT . MU.gogogoFast

goOleg :: Text -> IO (Either Text Ur.Ur)
goOleg = runExceptT . MU.gogogoOleg

goLazyOleg :: Text -> IO (Either Text Ur.Ur)
goLazyOleg = runExceptT . MU.gogogoLazyOleg

goInp :: Text -> IO (Either Text [Inp])
goInp = pure . parseInps

--------------------------------------------------------------------------------

runText :: Text -> IO ()
runText = runText' goSlow

#if !defined(__GHCJS__)
replFast :: IO ()
replFast = repl' "fast" goFast

replSlow :: IO ()
replSlow = repl' "slow" goSlow

replNew :: IO ()
replNew = repl' "slow" goNew

replOleg :: IO ()
replOleg = repl'' "oleg" goAll

replTromp = replOleg
replLazyTromp = replLazyOleg

replLazyOleg :: IO ()
replLazyOleg = repl' "compile" goLazyOleg

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

runFileTromp = runFileOleg
runFileLazyTromp = runFileLazyOleg
goTromp = goOleg
goLazyTromp = goLazyOleg

goAll :: Text -> IO (Either Text Text)
goAll = runExceptT . bar
 where
  bar :: Text -> ExceptT Text IO Text
  bar txt = do
    oleg      <- pack . ppShow <$> liftIO (goOleg txt)
    lazyoleg  <- pack . ppShow <$> liftIO (goLazyOleg txt)
    tromp     <- pack . ppShow <$> liftIO (goTromp txt)
    lazyTromp <- pack . ppShow <$> liftIO (goLazyTromp txt)

    pure $ unlines
      [ "", "[oleg]", oleg
      , "", "[lazyoleg]", lazyoleg
      , "", "[tromp]", tromp
      , "", "[lazytromp]", lazyTromp
      ]

runFileSlow :: FilePath -> IO ()
runFileSlow = runFile' goSlow

runFileNew :: FilePath -> IO ()
runFileNew = runFile' goNew

runFileFast :: FilePath -> IO ()
runFileFast = runFile' goFast

runFileOleg :: FilePath -> IO ()
runFileOleg = runFile'' goAll

runFileLazyOleg :: FilePath -> IO ()
runFileLazyOleg = runFile' goLazyOleg

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
