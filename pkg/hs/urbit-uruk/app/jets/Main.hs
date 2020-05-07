module Main where

import ClassyPrelude
import Urbit.Uruk.Dash.Parser

import Data.Text        (stripEnd)
import Text.Show.Pretty (pPrint, ppShow)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "|%"
  for_ (mapToList jetsMap) $ \(_,(arg,nam,bod)) -> do
    let namb = pack $ filter (/= '\'') $ show nam
    putStrLn ("++  " <> namb)
    putStrLn ("  ~/  " <> tshow arg <> "  %" <> namb)
    putStr $ pack $ indent $ ppShow bod
  putStrLn "--"

indent :: String -> String
indent = unlines . fmap ("  " <>) . lines
