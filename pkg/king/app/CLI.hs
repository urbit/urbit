{-# OPTIONS_GHC -Werror -Wall #-}
{-# LANGUAGE CPP #-}

module CLI (parseArgs, Cmd(..), New(..), Run(..), Opts(..)) where

import ClassyPrelude
import Options.Applicative
import Options.Applicative.Help.Pretty

import Data.Word          (Word16)
import System.Environment (getProgName)

--------------------------------------------------------------------------------

data Opts = Opts
    { oQuiet     :: Bool
    , oHashless  :: Bool
    , oExit      :: Bool
    , oDryRun    :: Bool
    , oVerbose   :: Bool
    , oAmesPort  :: Maybe Word16
    , oProf      :: Bool
    , oCollectFx :: Bool
    , oLocalhost :: Bool
    , oOffline   :: Bool
    }
  deriving (Show)

data New = New
    { nPillPath  :: FilePath
    , nShipAddr  :: Text
    , nPierPath  :: Maybe FilePath -- Derived from ship name if not specified.
    , nArvoDir   :: Maybe FilePath
    , nBootFake  :: Bool
    }
  deriving (Show)

data Run = Run
    { rPierPath :: FilePath
    }
  deriving (Show)

data Cmd
    = CmdNew New Opts
    | CmdRun Run Opts
    | CmdVal FilePath -- Validate Pill
  deriving (Show)

--------------------------------------------------------------------------------

headNote :: String -> Doc
headNote _version = string $ intercalate "\n"
  [ "Urbit: a personal server operating function"
  , "https://urbit.org"
  , "Version " <> VERSION_king
  ]

footNote :: String -> Doc
footNote exe = string $ intercalate "\n"
  [ "Development Usage:"
  , "  To create a development ship, use a fakezod:"
  , "    $ " <>exe<> " new zod /path/to/pill -F zod -A arvo/folder"
  , ""
  , "Simple Usage: "
  , "  $ " <>exe<> " new pier <my-comet> to create a comet (anonymous urbit)"
  , "  $ " <>exe<> " new pier <my-planet> -k <my-key-file> if you own a planet"
  , "  $ " <>exe<> " run <myplanet or mycomet> to restart an existing urbit"
  , ""
  , "For more information about developing on urbit, see:"
  , "  https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md"
  ]

--------------------------------------------------------------------------------

parseArgs :: IO Cmd
parseArgs = do
    nm <- getProgName

    let p = prefs $ showHelpOnError
                 <> showHelpOnEmpty
                 <> columns 80

    let o = info (cmd <**> helper)
              $ progDesc "Start an existing Urbit or boot a new one."
             <> headerDoc (Just $ headNote "0.9001.0")
             <> footerDoc (Just $ footNote nm)
             <> fullDesc

    customExecParser p o

--------------------------------------------------------------------------------

new :: Parser New
new = do
    nShipAddr <- strArgument
                     $ metavar "SHIP"
                    <> help "Ship address"

    nPierPath <- argument auto
                     $ metavar "PIER"
                    <> help "Path to pier"
                    <> value Nothing

    nPillPath <- strOption
                     $ short 'B'
                    <> long "pill"
                    <> metavar "PILL"
                    <> help "Path to pill file"

    nBootFake <- switch
                     $ short 'F'
                    <> long "fake"
                    <> help "Create a fake ship"

    nArvoDir <- option auto
                    $ metavar "PATH"
                   <> short 'A'
                   <> long "arvo"
                   <> value Nothing
                   <> help "Replace initial clay filesys with contents of PATH"

    pure New{..}

opts :: Parser Opts
opts = do
    oAmesPort  <- option auto $ metavar "PORT"
                             <> short 'p'
                             <> long "ames"
                             <> help "Ames port number"
                             <> value Nothing
                             <> hidden

    oHashless  <- switch $ short 'S'
                        <> long "hashless"
                        <> help "Disable battery hashing"
                        <> hidden

    oQuiet     <- switch $ short 'q'
                        <> long "quiet"
                        <> help "Quiet"
                        <> hidden

    oVerbose   <- switch $ short 'v'
                        <> long "verbose"
                        <> help "Verbose"
                        <> hidden

    oExit      <- switch $ short 'x'
                        <> long "exit"
                        <> help "Exit immediatly"
                        <> hidden

    oDryRun    <- switch $ short 'N'
                        <> long "dry-run"
                        <> help "Dry run -- Don't persist"
                        <> hidden

    oProf      <- switch $ short 'p'
                       <> long "profile"
                       <> help "Enable profiling"
                       <> hidden

    oLocalhost <- switch $ short 'L'
                        <> long "local"
                        <> help "Localhost-only networking"
                        <> hidden

    oOffline   <- switch $ short 'O'
                        <> long "offline"
                        <> help "Run without any networking"
                        <> hidden

    oCollectFx <- switch $ short 'f'
                        <> long "collect-fx"
                        <> help "Write effects to disk for debugging"
                        <> hidden

    pure (Opts{..})

runShip :: Parser Cmd
runShip = do
    rPierPath <- strArgument (metavar "PIER" <> help "Path to pier")
    o         <- opts
    pure (CmdRun (Run{..}) o)

valPill :: Parser Cmd
valPill = do
    pillPath <- strArgument (metavar "PILL" <> help "Path to pill")
    pure (CmdVal pillPath)

cmd :: Parser Cmd
cmd = subparser
        $ command "new" ( info (newShip <**> helper)
                        $ progDesc "Boot a new ship."
                        )
       <> command "run" ( info (runShip <**> helper)
                        $ progDesc "Run an existing ship."
                        )
       <> command "val" ( info (valPill <**> helper)
                        $ progDesc "Validate a pill file."
                        )
  where
    newShip = CmdNew <$> new <*> opts
