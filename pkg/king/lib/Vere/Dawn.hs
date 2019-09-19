{-# OPTIONS_GHC -Wwarn #-}
module Vere.Dawn where

import Arvo.Common
import Arvo.Event
import Azimuth.Azimuth
import UrbitPrelude    hiding (Call, to)

import Network.Ethereum.Api.Eth
import Network.Ethereum.Api.Provider
import Network.Ethereum.Api.Types    hiding (blockNumber)
import Network.Ethereum.Web3

import Data.Text (splitOn)

import qualified Data.Map.Strict as M

{-TODOs:

  - Dawn takes a NounMap instead of a Map. Need a conversion function.

-}


-- During boot, use the infura provider
provider = HttpProvider
  "https://mainnet.infura.io/v3/196a7f37c7d54211b4a07904ec73ad87"

azimuthContract = "0x223c067F8CF28ae173EE5CafEa60cA44C335fecB"


-- Reads the
--
-- TODO: I don't know how to change a BytesN 32 to an Atom.
retrieveGalaxyTable :: Quantity -> Web3 (Map Ship (Rift, Life, Pass))
retrieveGalaxyTable bloq =
    withAccount () $
      withParam (to .~ azimuthContract) $
        withParam (block .~ (BlockWithNumber bloq)) $
          M.fromList <$> mapM getRow [0..5]
  where
    getRow idx = do
      (pubKey, _, _, _, _, _, _, _, keyRev, continuity) <- points idx
      -- pubKey is a sort of ByteArray.
      pure (fromIntegral idx, (fromIntegral continuity,
                               fromIntegral keyRev,
                               fromIntegral 0))
                               -- pubKey ^. from atomBytes))

-- Reads the Turf domains off the blockchain at block height `bloq`.
readAmesDomains :: Quantity -> Web3 ([Turf])
readAmesDomains bloq =
    withAccount () $
      withParam (to .~ azimuthContract) $
        withParam (block .~ (BlockWithNumber bloq)) $
          mapM getTurf [0..2]
  where
    getTurf idx = do
      str <- dnsDomains idx
      pure $ Turf $ fmap Cord $ reverse $ splitOn "." str




{-
  [%dawn seed sponsor galaxies domains block eth-url snap]
-}

-- Produces either an error or a validated boot event structure.
dawnVent :: Seed -> RIO e (Either Text Dawn)
dawnVent (Seed (Ship ship) life ring oaf) = do
  -- TODO: The dawn code tries to switch which ethereum provider it uses based
  -- on a ships' rank, but then doesn't do anything with it other than passing
  -- it into the ship and just uses the hardcoded infura node?

  hs <- runWeb3' provider $ do
    -- Block number (dBloq)
    dBloq <- blockNumber

    -- TODO: Do the entire point:...:dawn flow. This now should work in theory
    --
    -- (x ...) <- withAccount () $
    --   withParam (to .~ azimuthContract) $
    --     points 15

    -- Retrieve the galaxy table [MUST FIX s/5/255/ AND PUBKEY TO ATOM]
    -- galaxyTable <- retrieveGalaxyTable dBloq
    -- print $ show galaxyTable

    -- Read Ames domains [DONE]
    -- dTurf <- readAmesDomains dBloq
    -- print $ show dTurf

    pure (dBloq)

  print $ show hs

  pure (Left "bad")

