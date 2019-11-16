{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack2nix.External.Cabal2nix (
  cabal2nix
  ) where

import           Cabal2nix                   (cabal2nixWithDB, parseArgs, optNixpkgsIdentifier, Options)
import           Control.Lens
import           Data.Bool                   (bool)
import           Data.Maybe                  (fromMaybe, maybeToList)
import           Data.Text                   (Text, unpack)
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import           Distribution.Nixpkgs.Haskell.Derivation (Derivation)
import           Distribution.PackageDescription (unFlagName)
import           Distribution.System         (Platform(..), Arch(..), OS(..))
import           Language.Nix
import           System.IO                   (hPutStrLn, stderr)
import           Stack.Types.Version         (Version)
import           Stack2nix.Types             (Args (..), Flags)

import           Text.PrettyPrint.HughesPJClass (Doc)

cabal2nix :: Args -> Version -> FilePath -> Maybe Text -> Maybe FilePath -> Flags -> DB.HackageDB -> IO (Either Doc Derivation)
cabal2nix Args{..} ghcVersion uri commit subpath flags hackageDB = do
  let runCmdArgs = args $ fromMaybe "." subpath
  hPutStrLn stderr $ unwords ("+ cabal2nix":runCmdArgs)
  options <- parseArgs runCmdArgs
  cabal2nixWithDB hackageDB $ cabalOptions options

  where
    args :: FilePath -> [String]
    args dir = concat
      [ maybe [] (\c -> ["--revision", unpack c]) commit
      , maybeToList argCabal2nixArgs
      , ["--subpath", dir]
      , ["--system", fromCabalPlatform argPlatform]
      , ["--compiler", "ghc-" ++ show ghcVersion]
      , ["-f" ++ bool "-" "" enable ++ unFlagName f | (f, enable) <- flags]
      , [uri]
      ]

-- Override default nixpkgs resolver to do pkgs.attr instead of attr
cabalOptions :: Options -> Options
cabalOptions options =
  options {
    optNixpkgsIdentifier = \i -> Just (binding # (i, path # ["pkgs", i]))
  }

-- | Copied (and modified) from src/Distribution/Nixpkgs/Meta.hs
fromCabalPlatform :: Platform -> String
fromCabalPlatform (Platform I386 Linux)   = "i686-linux"
fromCabalPlatform (Platform X86_64 Linux) = "x86_64-linux"
fromCabalPlatform (Platform X86_64 OSX)   = "x86_64-darwin"
fromCabalPlatform p                       = error ("fromCabalPlatform: invalid Nix platform" ++ show p)
