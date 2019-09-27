{-# OPTIONS_GHC -Wwarn #-}
module Vere.Dawn where

import Arvo.Common
import Arvo.Event      hiding (Address)
import Azimuth.Azimuth
import UrbitPrelude    hiding (Call, rights, to)

import Data.Maybe
import Data.Solidity.Abi.Codec       (encode)
import Data.Text                     (splitOn)
import Network.Ethereum.Account
import Network.Ethereum.Api.Eth
import Network.Ethereum.Api.Provider
import Network.Ethereum.Api.Types    hiding (blockNumber)
import Network.Ethereum.Web3

import qualified Crypto.Sign.Ed25519  as Ed
import qualified Data.ByteArray       as BA
import qualified Data.Map.Strict      as M
import qualified Network.Ethereum.Ens as Ens
import qualified Urbit.Ob             as Ob

-- During boot, use the infura provider
provider = HttpProvider
  "https://mainnet.infura.io/v3/196a7f37c7d54211b4a07904ec73ad87"

-- Conversion Utilities --------------------------------------------------------

bsToAtom :: ByteString -> Atom
bsToAtom x = x ^. from atomBytes

-- Takes the web3's bytes representation and changes the endianness.
bytes32ToBS :: BytesN 32 -> ByteString
bytes32ToBS = reverse . BA.pack . BA.unpack

bytes32ToAtom :: BytesN 32 -> Atom
bytes32ToAtom = bsToAtom . bytes32ToBS

-- web3 doesn't export unAddress.
addressToBS :: Address -> ByteString
addressToBS = reverse . encode

addressToAtom = bsToAtom . addressToBS

toBloq :: Quantity -> Bloq
toBloq = fromIntegral . unQuantity

passFromEth :: BytesN 32 -> BytesN 32 -> UIntN 32 -> Pass
passFromEth enc aut sut | sut /= 1 =
      Pass (Ed.PublicKey mempty) (Ed.PublicKey mempty)
passFromEth enc aut sut =
      Pass (decode aut) (decode enc)
    where
      decode = Ed.PublicKey . bytes32ToBS

clanFromShip :: Ship -> Ob.Class
clanFromShip = Ob.clan . Ob.patp . fromIntegral

shipSein :: Ship -> Ship
shipSein = Ship . fromIntegral . Ob.fromPatp . Ob.sein . Ob.patp . fromIntegral

renderShip :: Ship -> Text
renderShip = Ob.renderPatp . Ob.patp . fromIntegral

-- Data Validation -------------------------------------------------------------

-- for =(who.seed `@`fix:ex:cub)
-- getFingerprintFromKey :: Ring -> Atom
-- getFingerprintFromKey = undefined

-- Derive public key structure from the key derivation seed structure
getPassFromRing :: Ring -> Pass
getPassFromRing Ring{..} = Pass{..}
  where
    passCrypt = decode ringCrypt
    passSign = decode ringSign
    decode = fst . fromJust . Ed.createKeypairFromSeed_


-- Azimuth Functions -----------------------------------------------------------

-- Perform a request to azimuth at a certain block number
withAzimuth :: Quantity
            -> Address
            -> DefaultAccount Web3 a
            -> Web3 a
withAzimuth bloq azimuth action =
    withAccount () $
      withParam (to .~ azimuth) $
        withParam (block .~ (BlockWithNumber bloq))
          action

-- In the Hoon implementation, the EthPoint structure has space for the deed
-- information, but it immediately punts on this by bunting the deed structure
-- instead of making the correct calls. We just do the right thing.
--
retrievePoint :: Quantity -> Address -> Ship -> Web3 (EthPoint)
retrievePoint bloq azimuth ship =
  withAzimuth bloq azimuth $ do
    (owner, managementProxy, spawnProxy, votingProxy, transferProxy)
      <- rights (fromIntegral ship)

    (encryptionKey,
     authenticationKey,
     hasSponsor,
     active,
     escapeRequested,
     sponsor,
     escapeTo,
     cryptoSuite,
     keyRevision, continuityNum) <- points (fromIntegral ship)

    let escapeState = if escapeRequested
                      then Just $ Ship $ fromIntegral escapeTo
                      else Nothing

    let epOwn = (addressToAtom owner,
                 addressToAtom managementProxy,
                 addressToAtom votingProxy,
                 addressToAtom transferProxy)

    let epNet = if (not active)
        then Nothing
        else Just (
          (fromIntegral keyRevision),
          (passFromEth encryptionKey authenticationKey cryptoSuite),
          (fromIntegral continuityNum),
          (hasSponsor, Ship (fromIntegral sponsor)),
          escapeState
          )

    let epKid = case clanFromShip ship of
           Ob.Galaxy -> Just (addressToAtom spawnProxy, setToHoonSet mempty)
           Ob.Star   -> Just (addressToAtom spawnProxy, setToHoonSet mempty)
           _         -> Nothing

    pure EthPoint{..}

-- Retrieves information about all the galaxies from Ethereum.
retrieveGalaxyTable :: Quantity -> Address -> Web3 (Map Ship (Rift, Life, Pass))
retrieveGalaxyTable bloq azimuth =
    withAzimuth bloq azimuth $ M.fromList <$> mapM getRow [0..5]
  where
    getRow idx = do
      (encryptionKey, authenticationKey, _, _, _, _, _, cryptoSuite,
       keyRev, continuity) <- points idx
      pure (fromIntegral idx,
            (fromIntegral continuity,
              fromIntegral keyRev,
              (passFromEth encryptionKey authenticationKey cryptoSuite)))

-- Reads the three Ames domains from Ethereum.
readAmesDomains :: Quantity -> Address -> Web3 ([Turf])
readAmesDomains bloq azimuth =
    withAzimuth bloq azimuth $ mapM getTurf [0..2]
  where
    getTurf idx = do
      str <- dnsDomains idx
      pure $ Turf $ fmap Cord $ reverse $ splitOn "." str

-- Returns the sponsor of the current ship or fails on invalid state.
getSponsorShipAndValidate :: Quantity -> Address -> Seed -> Web3 (Ship)
getSponsorShipAndValidate block azimuth (Seed ship life ring oaf) =
  do
    if clan == Ob.Comet then
      validateComet
    else do
      who <- pointToRetrieve

      -- TODO: We don't need the entire EthPoint structure to do this; this
      -- is just copying what the old hoon code did.
      whoP <- retrievePoint block azimuth ship
      case clan of
        Ob.Moon -> validateMoon (epNet whoP)
        _       -> validateRest (epNet whoP)
  where
    clan = clanFromShip ship

    -- When we're booting a moon, we need to retrieve our planet's
    -- keys. Otherwise retrieve keys for the ship passed in.
    pointToRetrieve =
      if clan == Ob.Moon then do
        let parent = shipSein ship
        print ("boot: retrieving " ++ (renderShip parent) ++
               "'s public keys (for " ++ (renderShip ship) ++ ")")
        pure parent
      else do
        print ("boot: retrieving " ++ (renderShip ship) ++
               "'s public keys")
        pure ship

    validateComet = do
      -- TODO: All validation of the comet.
      -- A comet address is the fingerprint of the keypair
      -- when (ship /= (x ring.seed)) (Left "todo: key mismatch")
      -- A comet can never be breached
      -- when live Left "comet already booted"
      -- TODO: the parent must be launched check?
      --pure $ shipSein ship
      fail "dealing with comets is a giant todo i'm punting on for now"

    validateMoon = \case
      Nothing -> fail "sponsoring planet not keyed"
      Just _  -> pure $ shipSein ship

    validateRest = \case
      Nothing -> fail "ship not keyed"
      Just (netLife, pass, contNum, (hasSponsor, who), _) -> do
        when (netLife /= life) $
            fail $ pack
                 ("keyfile life mismatch; keyfile claims life " ++
                  (show life) ++ ", but Azimuth claims life " ++ (show netLife))
        when ((getPassFromRing ring) /= pass) $
            fail "keyfile does not match blockchain"
        -- TODO: The hoon code does a breach check, but the C code never
        -- supplies the data necessary for it to function.
        pure who



-- Produces either an error or a validated boot event structure.
dawnVent :: Seed -> RIO e (Either Text Dawn)
dawnVent dSeed@(Seed ship life ring oaf) = do
  ret <- runWeb3' provider $ do
    -- Block number (dBloq)
    block <- blockNumber
    print ("boot: eth block: " ++ (show block))

    print "boot: retrieving azimuth contract"
    azimuth <- withAccount () $ Ens.resolve "azimuth.eth"

    -- TODO: This is one of three cases: Validate data for a comet, get the
    -- moon's parent keys, or get your ship's keys.
    sponsorShip <- getSponsorShipAndValidate block azimuth dSeed

    -- Retrieve the whole EthPoint for our sponsor
    print $ "boot: retrieving sponsor " ++ (renderShip sponsorShip) ++
            "'s public keys"
    dSponsor <- retrievePoint block azimuth (fromIntegral sponsorShip)

    -- Retrieve the galaxy table [MUST FIX s/5/255/]
    print "boot: retrieving galaxy table"
    galaxyTable <- retrieveGalaxyTable block azimuth
    let dCzar = mapToHoonMap galaxyTable

    -- Read Ames domains
    print "boot: retrieving network domains"
    dTurf <- readAmesDomains block azimuth

    -- TODO: I need a Map -> NounMap conversion to turn the galaxyTable into
    -- dCzar.

    let dBloq = toBloq block

    -- dNode is supposed to be a PUrl to an Ethereum node. However, it looks
    -- like it's almost always Nothing. The jael side just has a default node
    -- that it goes and uses when null?
    let dNode = Nothing

    pure $ MkDawn{..}

  case ret of
    Left x  -> pure $ Left $ tshow x
    Right y -> pure $ Right y
