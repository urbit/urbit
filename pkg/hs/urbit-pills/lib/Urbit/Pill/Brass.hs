module Urbit.Pill.Brass where

import ClassyPrelude hiding (readFile)

import Urbit.Arvo.Common
import Urbit.Arvo.Event
import Urbit.Arvo.Pill
import Urbit.Noun
import Urbit.Noun.Time
import System.FilePattern.Directory
import Data.Text.IO (readFile)

import qualified Data.ByteString        as BS

-- This is an implementation of turning a "zinc" pill into a brass pill.
--
-- A zinc(?) pill is just the compiled boot-ova event list, and a brass pill is
-- a 3-tuple of the event lists [boot-ova module-ova userspace]. We want to
-- build brass pills out of a checked in zinc pill and the actual arvo source
-- directory on CI without doing massive rebuilds of the kernel every time.

-- Turn an arvo directory into a list of %veer events for the arvo kernel.
moduleOva :: FilePath -> IO [Ev]
moduleOva arvoDir = do
  -- TODO: The resolution on these haskell times is significantly higher than
  -- the ones which go in paths inside Urbit:
  --
  -- Urbit:   ~2020.7.30..23.16.05..3b85
  -- Haskell: ~2020.9.14..15.11.41..6d93.e5fb.71fb.c5de
  --
  -- This might not be a problem, but it may surprise some people?
  buildTime <- MkDate <$> now
  mapM (loadPath buildTime) modulesToLoad
 where
  loadPath buildTime (vaneName, term, filePath) = do
    txt <- (BigCord . Cord) <$>
      readFile (arvoDir </> "sys" </> filePath <.> "hoon")
    let path = filePathToPath filePath
    let pax = Path $
          [MkKnot "~zod", MkKnot "home", MkKnot $ pack $ show buildTime] ++
          unPath path ++
          [MkKnot "hoon"]

    pure $ EvVane $ case vaneName of
      Nothing   -> VaneZuse $ ZEVeer () term pax txt
      Just name -> VaneVane $ VEVeer (name, ()) term pax txt

  modulesToLoad :: [(Maybe VaneName, Cord, FilePath)]
  modulesToLoad = [
    (Nothing,   Cord "",  "zuse"),
    (Just Ames, Cord "a", "vane/ames"),
    (Just Behn, Cord "b", "vane/behn"),
    (Just Clay, Cord "c", "vane/clay"),
    (Just Dill, Cord "d", "vane/dill"),
    (Just Eyre, Cord "e", "vane/eyre"),
    (Just Gall, Cord "g", "vane/gall"),
    (Just Iris, Cord "i", "vane/iris"),
    (Just Jael, Cord "j", "vane/jael")
    ]

-- fileOvum is a list of events where the event is the initial filesystem
-- event.
fileOvum :: FilePath -> IO [Ev]
fileOvum arvoDir = allFiles >>= mapM fileToEntry <&> buildSyncEvent
  where
    -- Subdirectories under arvoDir that we want to pay attention to
    srcSubdirectories = ["app", "gen", "lib", "mar", "sur", "sys", "ted",
                         "tests"]

    -- Whitelisted filetype extensions to put in the filesystem
    srcExtensions = ["css", "hoon", "html", "js", "json", "md", "png", "txt",
                     "udon", "umd"]

    allFiles :: IO [FilePath]
    allFiles = concat <$> mapM getInDir srcSubdirectories
      where
        getInDir dir = map ((++) dir . (++) "/") <$>
                       getDirectoryFiles (arvoDir </> dir) filePatterns

        filePatterns = map ((++) "**/*.") srcExtensions

    textPlain = filePathToPath "text/plain"

    fileToEntry :: FilePath -> IO (Path, Maybe Mime)
    fileToEntry file = do
      bs <- BS.readFile (arvoDir </> file)
      pure (filePathToPath file, Just $ Mime textPlain (File $ Octs bs))

    buildSyncEvent items = [
      EvBlip $ BlipEvSync $ SyncEvInto None (Desk "") True items
      ]

-- Given a zinc pill with the Nock formulas for the boot sequence, build a
-- brass pill.
buildBrassPill :: FilePath -> FilePath -> IO Pill
buildBrassPill zincPillPath arvoDirectory = do
  putStrLn $ "Loading zinc pill from " <> (tshow zincPillPath) <> "..."
  (Pill pBootFormulas _ _) <- (loadFile zincPillPath >>= either throwIO pure)

  putStrLn $ "Loading kernel modules from " <> (tshow arvoDirectory) <> "/sys/..."
  pKernelOvums <- moduleOva arvoDirectory

  putStrLn $ "Loading userspace from " <> (tshow arvoDirectory) <> "..."
  pUserspaceOvums <- fileOvum arvoDirectory

  pure Pill{..}
