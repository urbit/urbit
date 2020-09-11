module Main where

import ClassyPrelude                   hiding (log)

import Urbit.Noun (saveFile)
import Urbit.Pill.Brass

import Options.Applicative
import Options.Applicative.Help.Pretty

--------------------------------------------------------------------------------

data Options = Options
  { oSrcZincPill  :: FilePath
  , oSrcArvoDir   :: FilePath
  , oDstBrassPill :: FilePath
  }
  deriving (Show)

srcZincPill :: Parser FilePath
srcZincPill = strArgument (metavar "ZINC"
                        <> help "Path to the input zinc pill")

srcArvoDirectory :: Parser FilePath
srcArvoDirectory = strArgument (metavar "ARVO"
                             <> help "Path to the arvo source directory")

dstBrassPill :: Parser FilePath
dstBrassPill = strArgument (metavar "BRASS"
                         <> help "Path to the output brass pill")

optionParser :: Parser Options
optionParser = do
  oSrcZincPill  <- srcZincPill
  oSrcArvoDir   <- srcArvoDirectory
  oDstBrassPill <- dstBrassPill
  pure Options{..}

optionInfo = info (optionParser <**> helper)
      ( fullDesc
     <> progDesc "Combines the ZINC pill and the ARVO directory into BRASS."
     <> header "urbit-alloy: changes zinc pills into brass pills" )

--------------------------------------------------------------------------------

main = do
  Options{..} <- execParser optionInfo

  brassPill <- buildBrassPill oSrcZincPill oSrcArvoDir

  putStrLn $ "Writing brass pill to " <> (tshow oDstBrassPill)
  saveFile oDstBrassPill brassPill
