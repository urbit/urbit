{-# LANGUAGE CPP #-}

module Urbit.Moon.Repl where

import Prelude ()
import Bound
import ClassyPrelude
import Control.Monad.Except
import GHC.Natural
import Urbit.Moon.AST
import Codec.Serialise

import Control.Arrow       ((>>>))
import Data.Function       ((&))
import System.Directory    (createDirectoryIfMissing, getHomeDirectory,
                            listDirectory, removeFile)
import System.IO.Unsafe    (unsafePerformIO)
import Text.Show.Pretty    (ppShow)
import Urbit.Skew.SkewDemo (Env, EvalResult(..), Inp(..), InpResult(..))
import Urbit.Skew.SkewDemo (execInp, execText, parseInps, printInpResult)

#if !defined(__GHCJS__)
import qualified System.Console.Haskeline as HL
#endif
import qualified Urbit.Atom               as Atom
import qualified Urbit.Moon.MoonToSkew    as MU
import qualified Urbit.Moon.Parser        as Parser
import qualified Urbit.Skew.JetEval       as JetEval
import qualified Urbit.Skew.SkewDemo      as Dem


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

goNew :: Text -> IO (Either Text JetEval.Exp)
goNew = runExceptT . MU.gogogo'new

goOleg :: Text -> IO (Either Text JetEval.Exp)
goOleg = runExceptT . ExceptT . fmap join . unExceptT . MU.gogogoOleg

goLazyOleg :: Text -> IO (Either Text JetEval.Exp)
goLazyOleg = runExceptT . ExceptT . fmap join . unExceptT . MU.gogogoLazyOleg

goTromp :: Text -> IO (Either Text JetEval.Exp)
goTromp = runExceptT . ExceptT . fmap join . unExceptT . MU.gogogoTromp

goNaive :: Text -> IO (Either Text JetEval.Exp)
goNaive = runExceptT . ExceptT . fmap join . unExceptT . MU.gogogoNaive

goLazyTromp :: Text -> IO (Either Text JetEval.Exp)
goLazyTromp = runExceptT . ExceptT . fmap join . unExceptT . MU.gogogoLazyTromp

goLazyNaive :: Text -> IO (Either Text JetEval.Exp)
goLazyNaive = runExceptT . ExceptT . fmap join . unExceptT . MU.gogogoLazyNaive

unExceptT :: ExceptT t m a -> m (Either t a)
unExceptT (ExceptT a) = a

goInp :: Text -> IO (Either Text [Inp])
goInp = pure . parseInps

--------------------------------------------------------------------------------

runText :: Text -> IO ()
runText = runText' goNew

#if !defined(__GHCJS__)
replNew :: IO ()
replNew = repl' "slow" goNew

replOleg :: IO ()
replOleg = repl'' "oleg" goAll -- TODO XX NASTY HACK

replTromp :: IO ()
replTromp = repl' "oleg" goTromp

replLazyTromp :: IO ()
replLazyTromp = repl' "lazytromp" goLazyTromp

replLazyOleg :: IO ()
replLazyOleg = repl' "compile" goLazyOleg

replRefr :: IO ()
replRefr = do
  vEnv <- readSkewDemoEnv >>= newIORef
  HL.runInputT HL.defaultSettings (loop vEnv)
 where
  loop :: IORef Env -> HL.InputT IO ()
  loop vEnv = do
    minput <- HL.getInputLine "skew> "
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

goAll :: Text -> IO (Either Text Text)
goAll = runExceptT . bar
 where
  bar :: Text -> ExceptT Text IO Text
  bar txt = do
    naive     <- ExceptT (goNaive txt)
--  lazyNaive <- ExceptT (goLazyNaive txt)
    tromp     <- ExceptT (goTromp txt)
--  lazyTromp <- ExceptT (goLazyTromp txt)
    oleg      <- ExceptT (goOleg txt)
--  lazyoleg  <- ExceptT (goLazyOleg txt)

    pure $ unlines
      [ "", "[naive]", "", pack (ppShow naive), ""
--    , "", "[lazynaive]", "", pack (ppShow lazyNaive), ""
      , "", "[tromp]", "", pack (ppShow tromp), ""
--    , "", "[lazytromp]", "", pack (ppShow lazyTromp), ""
      , "", "[oleg]", "", pack (ppShow oleg), ""
--    , "", "[lazyoleg]", "", pack (ppShow lazyoleg), ""
      ]

runFileSlow :: FilePath -> IO ()
runFileSlow = runFile' goNew

runFileNew :: FilePath -> IO ()
runFileNew = runFile' goNew

runFileOleg :: FilePath -> IO ()
runFileOleg = runFile'' goAll

runFileLazyOleg :: FilePath -> IO ()
runFileLazyOleg = runFile' goLazyOleg

evalTextSkew :: Text -> IO Text
evalTextSkew = evalText' goInp

evalText :: Text -> IO Text
evalText = evalText' goNew


-- Types -----------------------------------------------------------------------

-- | Identifier, should match /[a-zA-Z0-9$-]+/
type Ident = Text


-- Read/Write + Serialise/Deserialise ------------------------------------------

persistInpResu :: InpResult -> IO ()
persistInpResu = \case
  InpExpr _ _                  -> pure ()
  InpWipe n                    -> writeSkewDemoBind n Nothing
  InpDecl n _ (EvalResult r _) -> writeSkewDemoBind n (Just r)

writeSkewDemoBind :: Ident -> Maybe Dem.Exp -> IO ()
writeSkewDemoBind iden mExp =
  writeSkewDemoBindBytes iden (toStrict . serialise <$> mExp)

--  Throws `DeserialiseFailure` exception on decode failure.
readSkewDemoEnv :: IO (Map Ident Dem.Exp)
readSkewDemoEnv = readSkewDemoEnvBytes >>= traverse cvt
 where
  cvt :: ByteString -> IO Dem.Exp
  cvt byt = evaluate $ force $ deserialise $ fromStrict byt


-- Reading and writing bind files ----------------------------------------------

readSkewDemoEnvBytes :: IO (Map Ident ByteString)
readSkewDemoEnvBytes = do
  hom <- getHomeDirectory
  let dir = hom <> "/.skew/skew-demo/"
  createDirectoryIfMissing True dir
  fmap mapFromList $ listDirectory dir >>= traverse (loadEnt dir)
 where
  loadEnt :: FilePath -> String -> IO (Ident, ByteString)
  loadEnt dir ent = do
    let pax = dir <> "/" <> ent
    byt <- readFile pax
    pure (pack ent, byt)

writeSkewDemoBindBytes :: Ident -> Maybe ByteString -> IO ()
writeSkewDemoBindBytes iden mByt = do
  hom <- getHomeDirectory
  let dir = hom <> "/.skew/skew-demo/"
  let pax = dir <> unpack iden
  createDirectoryIfMissing True dir
  case mByt of
    Just bytz -> writeFile pax bytz
    Nothing   -> removeFile pax
