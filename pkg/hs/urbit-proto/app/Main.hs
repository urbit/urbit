
module Main where

import ClassyPrelude
import qualified Prelude as P

import Control.Lens ((&))
import qualified Data.Text.IO as T
import Data.Void
import Options.Applicative
import System.Exit (die)
import Text.Show.Pretty (pPrint)

import Practice.TopLevelDH4
import Practice.DependentHoon4 (Con(..), Type, Semi(..))
import Practice.Nock hiding (nock)
import Practice.Render


-- Entrypoint ------------------------------------------------------------------

data Opts = Opts
  { imports :: [FilePath]
  , tracery :: Maybe FilePath
  , typeful :: Bool
  , nockful :: Bool
  }
  deriving Show

data Command
  = Eval Text
  | Exec
  | Nock FilePath
  | Gall
  | Repl
  deriving Show

main :: IO ()
main = do
  args@(opts@Opts{..}, command) <- execParser arg'
  pPrint args
  case command of
    Eval hon -> eval hon opts
    Exec -> exec opts
    Nock out -> nock out opts
    Gall -> undefined
    Repl -> undefined

 where
  arg' = info (arg <**> helper)
    ( fullDesc
   <> progDesc "Demonstrate a draft implementation of Dependent Hoon"
   <> header "urbit-proto -- a demo of Dependent Hoon"
    )


-- Common utilities ------------------------------------------------------------

topLevel :: [(FilePath, Text)]
         -> Maybe FilePath
         -> IO (Type Void, Semi Void, Nock)
topLevel impr trac = stop <$> foldlM step (Noun', Atom' 0, []) impr
 where
  step :: (Type Void, Semi Void, [Nock])
       -> (FilePath, Text)
       -> IO (Type Void, Semi Void, [Nock])
  step (typ, ken, nks) (file, txt) = do
    let con = Con 0 typ
    let res = ride file con txt
    case res of
      ResNone{err} -> die $ unpack err
      ResRead{ero} -> die $ unpack ero
      ResOpen{ert} -> die $ unpack $ render ert
      ResType{typ, ken, nok} -> pure (typ, ken, nok:nks)

  stop (typ, ken, nks) =
    ( typ
    , ken
    , case nks of
        [] -> error "nock: need at least one hoon file to compile"
        _ -> P.foldr1 N7 nks
    )

readFiles :: [FilePath] -> IO [(FilePath, Text)]
readFiles = traverse \file -> (file,) <$> T.readFile file


-- Non-interactive commands ----------------------------------------------------

eval :: Text -> Opts -> IO ()
eval hoon Opts{..} = do
  impr <- (++ [("<input>", hoon)]) <$> readFiles imports
  (typ, ken, nok) <- topLevel impr tracery
  when typeful do
    putStrLn $ renderTank $ Palm "TYPE" [tank $ roll typ]
  when nockful do
    putStrLn $ "NOCK " <> tshow nok
  putStrLn $ render ken

exec :: Opts -> IO ()
exec Opts{..} = do
  impr <- readFiles imports
  (typ, ken, nok) <- topLevel impr tracery
  when typeful do
    putStrLn $ renderTank $ Palm "TYPE" [tank $ roll typ]
  when nockful do
    putStrLn $ "NOCK " <> tshow nok
  putStrLn $ render ken

nock :: FilePath -> Opts -> IO ()
nock out Opts{..} = do
  putStrLn
    "My friend, jammed nock output is not implemented yet. Try again later?"


-- Command line argument parsing -----------------------------------------------

arg :: Parser (Opts, Command)
arg = (,) <$> opt <*> com

opt :: Parser Opts
opt = Opts <$> imp
           <*> tac
           <*> wit
           <*> win

imp :: Parser [FilePath]
imp = many $ strArgument
  ( metavar "HOON_FILE"
 <> action "file"
 <> help "Hoon file to be added to the stack of tisgars for execution."
  )

tac :: Parser (Maybe FilePath)
tac = optional $ strOption
  ( long "trace"
 <> short 't'
 <> metavar "TRACE_OUTFILE"
 <> action "file"
 <> help "Write full compilation traces to the given output file for debugging"
 <> hidden
  )

wit :: Parser Bool
wit = switch
  ( long "with-type"
 <> short 't'
 <> help "Print type information"
  )

win :: Parser Bool
win = switch
  ( long "with-nock"
 <> short 'n'
 <> help "Print generated nock"
  )

com :: Parser Command
com = eva <|> exe <|> noc <|> gal <|> rep

eva :: Parser Command
eva = Eval . pack <$> strOption
  ( long "eval"
 <> short 'e'
 <> metavar "HOON_EXPR"
 <> help "Execute the given string as a hoon program against the imported files"
  )

exe :: Parser Command
exe = flag' Exec
  ( long "exec"
 <> short 'x'
 <> help "Execute the tisgar-stack of hoon files and print the result"
  )

noc :: Parser Command
noc = Nock <$> strOption
  ( long "nock"
 <> short 'o'
 <> metavar "NOCK_OUTFILE"
 <> action "file"
 <> help "Compile the tisgar-stack to nock and write its jam to the given file"
  )

gal :: Parser Command
gal = flag' Gall
  ( long "gall"
 <> short 'g'
 <> help "Interpret the tisgar stack as a 'gall' application and run it"
  )

rep :: Parser Command
rep = flag Repl Repl
  ( long "repl"
 <> short 'r'
 <> help "Enter hoon repl, treating the tisgar-stack as libraries (default)"
  )
