{-# OPTIONS_GHC -Werror -Wall #-}
{-# LANGUAGE CPP #-}

module CLI (parseArgs, Cmd(..), BootType(..), PillSource(..), New(..), Run(..),
            Bug(..), Opts(..)) where

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
    , oTrace     :: Bool
    , oCollectFx :: Bool
    , oLocalhost :: Bool
    , oOffline   :: Bool
    }
  deriving (Show)

data BootType
  = BootComet
  | BootFake Text
  | BootFromKeyfile FilePath
  deriving (Show)

data PillSource
  = PillSourceFile FilePath
  | PillSourceURL String
  deriving (Show)

data New = New
    { nPillSource :: PillSource
    , nPierPath   :: Maybe FilePath -- Derived from ship name if not specified.
    , nArvoDir    :: Maybe FilePath
    , nBootType   :: BootType
    , nLite       :: Bool
    }
  deriving (Show)

data Run = Run
    { rPierPath :: FilePath
    }
  deriving (Show)

data Bug
    = ValidatePill
        { bPillPath :: FilePath
        , bPrintPil :: Bool
        , bPrintSeq :: Bool
        }
    | CollectAllFX
        { bPierPath :: FilePath
        }
    | EventBrowser
        { bPierPath :: FilePath
        }
    | ValidateEvents
        { bPierPath :: FilePath
        , bFirstEvt :: Word64
        , bFinalEvt :: Word64
        }
    | ValidateFX
        { bPierPath :: FilePath
        , bFirstEvt :: Word64
        , bFinalEvt :: Word64
        }
    | CheckDawn
        { bKeyfilePath :: FilePath
        }
    | CheckComet
  deriving (Show)

data Cmd
    = CmdNew New Opts
    | CmdRun Run Opts
    | CmdBug Bug
    | CmdCon Word16
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

defaultPillURL :: String
defaultPillURL = "https://bootstrap.urbit.org/urbit-" <> VERSION_king <> ".pill"

--------------------------------------------------------------------------------

newComet :: Parser BootType
newComet = flag' BootComet
  (  long "comet"
  <> help "Boot a new comet")

newFakeship :: Parser BootType
newFakeship = BootFake <$> strOption
                         (short 'F'
                        <> long "fake"
                        <> metavar "SHIP"
                        <> help "Boot a fakeship")

newFromKeyfile :: Parser BootType
newFromKeyfile = BootFromKeyfile <$> strOption
                             (short 'k'
                            <> long "keyfile"
                            <> metavar "KEYFILE"
                            <> help "Boot from a keyfile")

pillFromPath :: Parser PillSource
pillFromPath = PillSourceFile <$> strOption
                     ( short 'B'
                    <> long "pill"
                    <> metavar "PILL"
                    <> help "Path to pill file")

pillFromURL :: Parser PillSource
pillFromURL = PillSourceURL <$> strOption
                     ( short 'u'
                    <> long "pill-url"
                    <> metavar "URL"
                    <> value defaultPillURL
                    <> help "URL to pill file")

new :: Parser New
new = do
    nPierPath <- optional
               $ strArgument
                     $ metavar "PIER"
                    <> help "Path to pier"

    nBootType <- newComet <|> newFakeship <|> newFromKeyfile

    nPillSource <- pillFromPath <|> pillFromURL

    nLite <- switch
               $ short 'l'
              <> long "lite"
              <> help "Boots ship in lite mode"

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

    -- Always disable hashboard. Right now, urbit is almost unusable with this
    -- flag enabled and it is disabled in vere.
    let oHashless = True
    -- oHashless  <- switch $ short 'S'
    --                     <> long "hashless"
    --                     <> help "Disable battery hashing"
    --                     <> hidden

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

    oTrace      <- switch $ short 't'
                         <> long "trace"
                         <> help "Enable tracing"
                         <> hidden

    oLocalhost <- switch $ short 'L'
                        <> long "local"
                        <> help "Localhost-only networking"
                        <> hidden

    oCollectFx <- switch $ short 'f'
                        <> long "collect-fx"
                        <> help "Write effects to disk for debugging"
                        <> hidden

    oOffline   <- switch $ short 'O'
                        <> long "offline"
                        <> help "Run without any networking"
                        <> hidden

    pure (Opts{..})

newShip :: Parser Cmd
newShip = CmdNew <$> new <*> opts

runShip :: Parser Cmd
runShip = do
    rPierPath <- strArgument (metavar "PIER" <> help "Path to pier")
    o         <- opts
    pure (CmdRun (Run{..}) o)

valPill :: Parser Bug
valPill = do
    bPillPath <- strArgument (metavar "PILL" <> help "Path to pill")

    bPrintPil <- switch $ long "print-pill"
                       <> help "Print pill"

    bPrintSeq <- switch $ long "print-boot"
                       <> help "Print boot sequence"

    pure ValidatePill{..}

pierPath :: Parser FilePath
pierPath = strArgument (metavar "PIER" <> help "Path to pier")

keyfilePath :: Parser FilePath
keyfilePath = strArgument (metavar "KEYFILE" <> help "Path to key file")

firstEv :: Parser Word64
firstEv = option auto $ long "first"
                     <> metavar "FST"
                     <> help "starting from event FST"
                     <> value 1

lastEv :: Parser Word64
lastEv = option auto $ long "last"
                    <> metavar "LAS"
                    <> help "anding with event LAS"
                    <> value maxBound

checkEvs :: Parser Bug
checkEvs = ValidateEvents <$> pierPath <*> firstEv <*> lastEv

checkFx :: Parser Bug
checkFx = ValidateFX <$> pierPath <*> firstEv <*> lastEv

browseEvs :: Parser Bug
browseEvs = EventBrowser <$> pierPath

checkDawn :: Parser Bug
checkDawn = CheckDawn <$> keyfilePath

bugCmd :: Parser Cmd
bugCmd = fmap CmdBug
        $ subparser
        $ command "validate-pill"
            ( info (valPill <**> helper)
            $ progDesc "Validate a pill file."
            )
       <> command "collect-all-fx"
            ( info (allFx <**> helper)
            $ progDesc "Replay entire event log, collecting all effects"
            )
       <> command "validate-events"
            ( info (checkEvs <**> helper)
            $ progDesc "Parse all data in event log"
            )
       <> command "event-browser"
            ( info (browseEvs <**> helper)
            $ progDesc "Interactively view (and prune) event log"
            )
       <> command "validate-effects"
            ( info (checkFx <**> helper)
            $ progDesc "Parse all data in event log"
            )
       <> command "dawn"
            ( info (checkDawn <**> helper)
            $ progDesc "Test run dawn"
            )
       <> command "comet"
            ( info (pure CheckComet)
            $ progDesc "Shows the list of stars accepting comets"
            )

conCmd :: Parser Cmd
conCmd = do
    port <- argument auto ( metavar "PORT"
                         <> help "Port of terminal server"
                          )
    pure (CmdCon port)

allFx :: Parser Bug
allFx = do
    bPierPath <- strArgument (metavar "PIER" <> help "Path to pier")
    pure CollectAllFX{..}

cmd :: Parser Cmd
cmd = subparser
        $ command "new" ( info (newShip <**> helper)
                        $ progDesc "Boot a new ship."
                        )
       <> command "run" ( info (runShip <**> helper)
                        $ progDesc "Run an existing ship."
                        )
       <> command "bug" ( info (bugCmd <**> helper)
                        $ progDesc "Run a debugging sub-command."
                        )
       <> command "con" ( info (conCmd <**> helper)
                        $ progDesc "Connect a terminal to a running urbit."
                        )
