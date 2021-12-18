{-# OPTIONS_GHC -Werror -Wall #-}
{-# LANGUAGE CPP #-}

{-|
    Command line parsing.
-}
module Urbit.King.CLI where

import ClassyPrelude                   hiding (log)
import Options.Applicative
import Options.Applicative.Help.Pretty

import Data.Word          (Word16)
import RIO                (LogLevel(..))
import System.Environment (getProgName)

--------------------------------------------------------------------------------

data Host = Host
  { hSharedHttpPort  :: Maybe Word16
  , hSharedHttpsPort :: Maybe Word16
  , hUseNatPmp       :: Nat
  , hSerfExe         :: Maybe Text
  }
 deriving (Show)

-- | Options for each running pier.
data Opts = Opts
    { oQuiet        :: Bool
    , oHashless     :: Bool
    , oExit         :: Bool
    , oDryRun       :: Bool
    , oVerbose      :: Bool
    , oAmesPort     :: Maybe Word16
    , oNoAmes       :: Bool
    , oNoHttp       :: Bool
    , oNoHttps      :: Bool
    , oTrace        :: Bool
    , oCollectFx    :: Bool
    , oLocalhost    :: Bool
    , oOffline      :: Bool
    , oHttpPort     :: Maybe Word16
    , oHttpsPort    :: Maybe Word16
    , oLoopbackPort :: Maybe Word16
    , oInjectEvents :: [Injection]
    }
  deriving (Show)

-- | Options for the logging subsystem.
data Log = Log
    { lTarget  :: Maybe (LogTarget FilePath)
    , lLevel   :: LogLevel
    }
  deriving (Show)

data LogTarget a
  = LogOff
  | LogStderr
  | LogFile a
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

data Nat
  = NatAlways
  | NatWhenPrivateNetwork
  | NatNever
  deriving (Show)

data Injection
  = InjectOneEvent   FilePath
  | InjectManyEvents FilePath
  deriving (Show)

data New = New
    { nPillSource :: PillSource
    , nPierPath   :: Maybe FilePath -- Derived from ship name if not specified.
    , nArvoDir    :: Maybe FilePath
    , nBootType   :: BootType
    , nLite       :: Bool
    , nEthNode    :: String
    , nSerfExe    :: Maybe Text
    }
  deriving (Show)

data Run = Run
    { rPierPath :: FilePath
    }
  deriving (Show)

data Bug
    = CheckDawn
        { bEthNode     :: String
        , bKeyfilePath :: FilePath
        }
    | CheckComet
  deriving (Show)

data Cmd
    = CmdNew New  Opts
    | CmdRun Host [(Run, Opts, Bool)]
    | CmdBug Bug
    | CmdCon FilePath
  deriving (Show)

--------------------------------------------------------------------------------

headNote :: String -> Doc
headNote _version = string $ intercalate "\n"
  [ "Urbit: a personal server operating function"
  , "https://urbit.org"
  , "Version " <> VERSION_urbit_king
  ]

-- | TODO This needs to be updated.
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

parseArgs :: IO (Cmd, Log)
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
defaultPillURL = "https://bootstrap.urbit.org/urbit-v" <> ver <> ".pill"
  where
    ver = VERSION_urbit_king

--------------------------------------------------------------------------------

newComet :: Parser BootType
newComet = flag' BootComet
    ( long "comet"
   <> help "Boot a new comet"
    )

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

pierPath :: Parser FilePath
pierPath = strArgument (metavar "PIER" <> help "Path to pier")

injectEvents :: Parser [Injection]
injectEvents = many $ InjectOneEvent <$> strOption
                        ( short 'I'
                       <> long "inject-event"
                       <> metavar "JAM"
                       <> help "Path to a jammed event"
                       <> hidden)
                  <|> InjectManyEvents <$> strOption
                        ( long "inject-event-list"
                       <> metavar "JAM_LIST"
                       <> help "Path to a jammed list of events"
                       <> hidden)

serfExe :: Parser (Maybe Text)
serfExe =  optional
    $  strOption
    $  metavar "PATH"
    <> long "serf"
    <> help "Path to serf binary to run ships in"
    <> hidden

ethNode :: Parser String
ethNode = strOption
     $ short 'e'
    <> long "eth-node"
    <> value "http://eth-mainnet.urbit.org:8545"
    <> help "Ethereum gateway URL"
    <> hidden

new :: Parser New
new = do
    nPierPath <- optional pierPath

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

    nEthNode <- ethNode

    nSerfExe <- serfExe

    pure New{..}

opts :: Parser Opts
opts = do
    oAmesPort <-
      optional
      $  option auto
      $  metavar "PORT"
      <> short 'p'
      <> long "ames"
      <> help "Ames port"
      <> hidden

    oNoAmes <-
     switch
     $  long "no-ames"
     <> help "Run with Ames disabled."
     <> hidden

    oNoHttp <-
     switch
     $  long "no-http"
     <> help "Run with HTTP disabled."
     <> hidden

    oNoHttps <-
     switch
     $  long "no-https"
     <> help "Run with HTTPS disabled."
     <> hidden

    oHttpPort <-
      optional
      $  option auto
      $  metavar "PORT"
      <> long "http-port"
      <> help "HTTP port"
      <> hidden

    oHttpsPort <-
      optional
      $  option auto
      $  metavar "PORT"
      <> long "https-port"
      <> help "HTTPS port"
      <> hidden

    oLoopbackPort <-
      optional
      $  option auto
      $  metavar "PORT"
      <> long "loopback-port"
      <> help "Localhost-only HTTP port"
      <> hidden

    oInjectEvents <- injectEvents

    oHashless  <- switch $ short 'S'
                        <> long "hashless"
                        <> help "Disable battery hashing (Ignored for now)"
                        <> hidden

    oQuiet     <- switch $ short 'q'
                        <> long "quiet"
                        <> help "Quiet"
                        <> hidden

    oVerbose   <- switch $ short 'v'
                        <> long "verbose"
                        <> help "Puts the serf and king into verbose mode"
                        <> hidden

    oExit      <- switch $ short 'x'
                        <> long "exit"
                        <> help "Exit immediately"
                        <> hidden

    oDryRun    <- switch $ long "dry-run"
                        <> help "Turn off persistence [UNSUPPORTED] and Ames"
                        <> hidden

    oTrace     <- switch $ short 't'
                        <> long "trace"
                        <> help "Enable tracing"
                        <> hidden

    oLocalhost <- switch $ short 'L'
                        <> long "local"
                        <> help "Localhost-only Ames networking"
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

log :: Parser Log
log = do
  lTarget <-
    optional
      $ ( flag' LogStderr
        $ long "log-to-stderr"
       <> long "stderr"
       <> help "Display logs on stderr"
       <> hidden
        )
    <|> ( fmap LogFile . strOption
        $ long "log-to"
       <> metavar "LOG_FILE"
       <> help "Append logs to the given file"
       <> hidden
        )
    <|> ( flag' LogOff
        $ long "no-logging"
       <> help "Disable logging entirely"
       <> hidden
        )

  lLevel <-
        ( flag' LevelDebug
        $ long "log-debug"
       <> help "Log errors, warnings, info, and debug messages"
       <> hidden
        )
    <|> ( flag' LevelInfo
        $ long "log-info"
       <> help "Log errors, warnings, and info"
       <> hidden
        )
    <|> ( flag' LevelWarn
        $ long "log-warn"
       <> help "Log errors and warnings (default)"
       <> hidden
        )
    <|> ( flag' LevelError
        $ long "log-error"
       <> help "Log errors only"
       <> hidden
        )
    <|> pure LevelWarn

  pure (Log{..})

newShip :: Parser (Cmd, Log)
newShip = (,) <$> (CmdNew <$> new <*> opts) <*> log

runOneShip :: Parser (Run, Opts, Bool)
runOneShip = (,,) <$> fmap Run pierPath <*> opts <*> df
 where
  df = switch (short 'd' <> long "daemon" <> help "Daemon mode" <> hidden)

host :: Parser Host
host = do
  hSharedHttpPort <-
    optional
    $  option auto
    $  metavar "PORT"
    <> long "shared-http-port"
    <> help "HTTP port"
    <> hidden

  hSharedHttpsPort <-
    optional
    $  option auto
    $  metavar "PORT"
    <> long "shared-https-port"
    <> help "HTTPS port"
    <> hidden

  hUseNatPmp <-
     ( flag' NatAlways
     $ long "port-forwarding"
    <> help "Always try to search for a router to forward ames ports"
    <> hidden
     ) <|>
     ( flag' NatNever
     $ long "no-port-forwarding"
    <> help "Disable trying to ask the router to forward ames ports"
    <> hidden
     ) <|>
     ( flag' NatWhenPrivateNetwork
     $ long "port-forwarding-when-internal"
    <> help ("Try asking the router to forward when ip is 192.168.0.0/16, " <>
             "172.16.0.0/12 or 10.0.0.0/8 (default).")
    <> hidden
     ) <|>
     (pure $ NatWhenPrivateNetwork)

  hSerfExe <- serfExe

  pure (Host{..})

runShip :: Parser (Cmd, Log)
runShip = (,) <$> (CmdRun <$> host <*> some runOneShip) <*> log

keyfilePath :: Parser FilePath
keyfilePath = strArgument (metavar "KEYFILE" <> help "Path to key file")

checkDawn :: Parser Bug
checkDawn = CheckDawn <$> ethNode <*> keyfilePath

bugCmd :: Parser (Cmd, Log)
bugCmd = (flip (,) <$> log <*>) $ fmap CmdBug
        $ subparser
        $ command "dawn"
            ( info (checkDawn <**> helper)
            $ progDesc "Test run dawn"
            )
       <> command "comet"
            ( info (pure CheckComet)
            $ progDesc "Shows the list of stars accepting comets"
            )

conCmd :: Parser (Cmd, Log)
conCmd = (,) <$> (CmdCon <$> pierPath) <*> log

cmd :: Parser (Cmd, Log)
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
