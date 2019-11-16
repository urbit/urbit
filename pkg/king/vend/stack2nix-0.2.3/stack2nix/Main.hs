module Main ( main ) where

import           Data.Semigroup            ((<>))
import           Data.Time                 (UTCTime, defaultTimeLocale,
                                            parseTimeM)
import qualified Distribution.Compat.ReadP as P
import           Distribution.System       (Arch (..), OS (..), Platform (..),
                                            buildPlatform)
import           Distribution.Text         (display)
import           Options.Applicative
import           Stack2nix
import           System.IO                 (BufferMode (..), hSetBuffering,
                                            stderr, stdout)

args :: Parser Args
args = Args
       <$> optional (strOption $ long "revision" <> help "revision to use when fetching from VCS")
       <*> optional (strOption $ short 'o' <> help "output file for generated nix expression" <> metavar "PATH")
       <*> strOption (long "stack-yaml" <> help "Override project stack.yaml file" <> showDefault <> value "stack.yaml")
       <*> option auto (short 'j' <> help "number of threads for subprocesses" <> showDefault <> value 4 <> metavar "INT")
       <*> switch (long "test" <> help "enable tests")
       <*> switch (long "bench" <> help "enable benchmarks")
       <*> switch (long "haddock" <> help "enable documentation generation")
       <*> optional (option utcTimeReader (long "hackage-snapshot" <> help "hackage snapshot time, ISO format"))
       <*> option (readP platformReader) (long "platform" <> help "target platform to use when invoking stack or cabal2nix" <> value buildPlatform <> showDefaultWith display)
       <*> strArgument (metavar "URI")
       <*> flag True False (long "no-indent" <> help "disable indentation and place one item per line")
       <*> switch (long "verbose" <> help "verbose output")
       <*> optional (strOption $ long "cabal2nix-args" <> help "extra arguments for cabal2nix")
       <*> flag True False (long "no-ensure-executables" <> help "do not ensure required executables are installed")
  where
    -- | A parser for the date. Hackage updates happen maybe once or twice a month.
    -- Example: parseTime defaultTimeLocale "%FT%T%QZ" "2017-11-20T12:18:35Z" :: Maybe UTCTime
    utcTimeReader :: ReadM UTCTime
    utcTimeReader = eitherReader $ \arg ->
        case parseTimeM True defaultTimeLocale "%FT%T%QZ" arg of
            Nothing      -> Left $ "Cannot parse date, ISO format used ('2017-11-20T12:18:35Z'): " ++ arg
            Just utcTime -> Right utcTime

    -- | A String parser for Distribution.System.Platform
    -- | Copied from cabal2nix/src/Cabal2nix.hs
    platformReader :: P.ReadP r Platform
    platformReader = do
      arch <- P.choice
          [ P.string "i686" >> return I386
          , P.string "x86_64" >> return X86_64
          ]
      _ <- P.char '-'
      os <- P.choice
          [ P.string "linux" >> return Linux
          , P.string "osx" >> return OSX
          , P.string "darwin" >> return OSX
          ]
      return (Platform arch os)

    readP :: P.ReadP a a -> ReadM a
    readP p = eitherReader $ \s ->
      case [ r' | (r',"") <- P.readP_to_S p s ] of
        (r:_) -> Right r
        _     -> Left ("invalid value " ++ show s)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  stack2nix =<< execParser opts
  where
    opts = info
      (helper
       <*> infoOption ("stack2nix " ++ display version) (long "version" <> help "Show version number")
       <*> args) $
      fullDesc
      <> progDesc "Generate a nix expression for a Haskell package using stack"
