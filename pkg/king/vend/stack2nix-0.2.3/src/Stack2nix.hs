{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack2nix
  ( Args(..)
  , stack2nix
  , version
  ) where

import           Control.Monad              (unless, void, when)
import           Data.Maybe                 (isJust)
import           Data.Monoid                ((<>))
import           Paths_stack2nix            (version)
import           Stack2nix.External.Stack
import           Stack2nix.External.Util    (runCmdFrom, failHard)
import           Stack2nix.External.VCS.Git (Command (..), ExternalCmd (..),
                                             InternalCmd (..), git)
import           Stack2nix.Types            (Args (..))
import           Stack2nix.Util
import           System.Directory           (doesFileExist,
                                             getCurrentDirectory, withCurrentDirectory)
import           System.Environment         (getEnv, setEnv)
import           System.FilePath            ((</>))
import           System.IO.Temp             (withSystemTempDirectory)

stack2nix :: Args -> IO ()
stack2nix args@Args{..} = do
  when argEnsureExecutables $ do
    ensureExecutableExists "cabal" "cabal-install"
    ensureExecutableExists "git" "git"
    ensureExecutableExists "nix-prefetch-git" "nix-prefetch-scripts"
  assertMinVer "git" "2"
  assertMinVer "cabal" "2"
  setEnv "GIT_QUIET" "y"
  updateCabalPackageIndex
  -- cwd <- getCurrentDirectory
  -- let projRoot = if isAbsolute argUri then argUri else cwd </> argUri
  let projRoot = argUri
  isLocalRepo <- doesFileExist $ projRoot </> argStackYaml
  logDebug args $ "stack2nix (isLocalRepo): " ++ show isLocalRepo
  logDebug args $ "stack2nix (projRoot): " ++ show projRoot
  logDebug args $ "stack2nix (argUri): " ++ show argUri
  if isLocalRepo
  then handleStackConfig Nothing projRoot
  else withSystemTempDirectory "s2n-" $ \tmpDir ->
    tryGit tmpDir >> handleStackConfig (Just argUri) tmpDir
  where
    updateCabalPackageIndex :: IO ()
    updateCabalPackageIndex = do
      home <- getEnv "HOME"
      out <- runCmdFrom home "cabal" ["update"]
      void $ failHard out

    tryGit :: FilePath -> IO ()
    tryGit tmpDir = do
      void $ git $ OutsideRepo $ Clone argUri tmpDir
      case argRev of
        Just r  -> void $ git $ InsideRepo tmpDir (Checkout r)
        Nothing -> return mempty

    handleStackConfig :: Maybe String -> FilePath -> IO ()
    handleStackConfig remoteUri localDir = do
      cwd <- getCurrentDirectory
      logDebug args $ "handleStackConfig (cwd): " ++ cwd
      logDebug args $ "handleStackConfig (localDir): " ++ localDir
      logDebug args $ "handleStackConfig (remoteUri): " ++ show remoteUri
      let stackFile = localDir </> argStackYaml
      alreadyExists <- doesFileExist stackFile
      unless alreadyExists $ error $ stackFile <> " does not exist. Use 'stack init' to create it."
      logDebug args $ "handleStackConfig (alreadyExists): " ++ show alreadyExists
      let go = if isJust remoteUri
               then withCurrentDirectory localDir
               else id
      go $ runPlan localDir remoteUri args
