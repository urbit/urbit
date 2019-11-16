module Stack2nix.Util
  ( assertMinVer
  , extractVersion
  , mapPool
  , logDebug
  , ensureExecutableExists
  , ensureExecutable
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.MSem
import           Control.Exception            (onException)
import           Control.Monad                (unless)
import           Data.Maybe                   (listToMaybe)
import qualified Data.Traversable             as T
import           Data.Version                 (Version (..), parseVersion,
                                               showVersion)
import           Data.Text                    (pack, strip, unpack)
import           GHC.Exts                     (sortWith)
import           Stack2nix.External.Util      (runCmd)
import           Stack2nix.Types              (Args, argVerbose)
import           System.Directory             (findExecutable)
import           System.Environment           (getEnv, setEnv)
import           System.Exit                  (ExitCode (..))
import           System.IO                    (hPutStrLn, stderr)
import           System.Process               (readProcessWithExitCode)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Regex.PCRE              (AllTextMatches (..),
                                               getAllTextMatches, (=~))

-- Credit: https://stackoverflow.com/a/18898822/204305
mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max' f xs = do
  sem <- new max'
  mapConcurrently (with sem . f) xs

-- heuristic for parsing version from stdout
extractVersion :: String -> Maybe Version
extractVersion str = ver
  where
    firstLine = head . lines $ str
    candidateVers = getAllTextMatches (firstLine =~ "[\\d\\.]+" :: AllTextMatches [] String)
    bestMatch = head . reverse . sortWith length $ candidateVers
    ver = fmap fst . listToMaybe . reverse . readP_to_S parseVersion $ bestMatch

assertMinVer :: String -> String -> IO ()
assertMinVer prog minVer = do
  hPutStrLn stderr $ unwords ["Ensuring", prog, "version is >=", minVer, "..."]
  result <- runCmd prog ["--version"] `onException` error ("Failed to run " ++ prog ++ ". Not found in PATH.")
  case result of
    (ExitSuccess, out, _) ->
      let ver = extractVersion out in
        unless (ver >= extractVersion minVer) $ error $ unwords ["ERROR:", prog, "version must be", minVer, "or higher. Current version:", maybe "[parse failure]" showVersion ver]
    (ExitFailure _, _, err)  -> error err

logDebug :: Args -> String -> IO ()
logDebug args msg
  | argVerbose args = hPutStrLn stderr msg
  | otherwise = return ()


-- check if executable is present, if not provision it with nix
ensureExecutableExists :: String -> String -> IO ()
ensureExecutableExists executable nixAttr = do
  exec <- findExecutable executable
  case exec of
    Just _ -> return ()
    Nothing -> ensureExecutable nixAttr

-- given nixAttr, build it and add $out/bin to $PATH
ensureExecutable :: String -> IO ()
ensureExecutable nixAttr = do
  (exitcode2, stdout, err2) <- readProcessWithExitCode "nix-build" ["-A", nixAttr, "<nixpkgs>", "--no-build-output", "--no-out-link"] mempty
  case exitcode2 of
    ExitSuccess -> do
      hPutStrLn stderr $ err2
      path <- getEnv "PATH"
      setEnv "PATH" (unpack (strip (pack stdout)) ++ "/bin" ++ ":" ++ path)
    ExitFailure _ -> error $ nixAttr ++ " failed to build via nix"
