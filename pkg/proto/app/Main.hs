module Main where

import ClassyPrelude

import Control.Lens ((&))

import Untyped.Parser hiding (main)
import Untyped.CST
import Untyped.Hoon
import Untyped.Core
import Nock
import SimpleNoun
import Dashboard

import Text.Show.Pretty (pPrint)

import qualified Prelude as P

-------------------------------------------------------------------------------

main :: IO ()
main = (P.head <$> getArgs) >>= compileHoonTest

compileHoonTest :: Text -> IO ()
compileHoonTest ln = do
    cst <- parse ln & \case
              Left x  -> error (unpack x)
              Right x -> pure x
    pPrint cst
    hon <- pure $ hone cst
    pPrint hon
    exp <- pure $ desugar hon
    pPrint exp
    nok <- pure $ copy exp
    pPrint nok
    res <- runCare $ nock (A 140) nok
    pPrint res
