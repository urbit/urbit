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

{-TODOs:

  - Dawn takes a NounMap instead of a Map. Need a conversion function.

  - The Haskell Dawn structure as it exists right now isn't right? It can't
    parse a real %dawn event in the event browser.

-}


-- During boot, use the infura provider
provider = HttpProvider
  "https://mainnet.infura.io/v3/196a7f37c7d54211b4a07904ec73ad87"


--azimuthContract = "0x223c067F8CF28ae173EE5CafEa60cA44C335fecB"

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


-- A Pass is the encryptionKey and authenticationKey concatenated together.
--

passFromEth :: BytesN 32 -> BytesN 32 -> UIntN 32 -> Pass
passFromEth enc aut sut | sut /= 1 = error "Invalid crypto suite number"
passFromEth enc aut sut =
      Pass (decode aut) (decode enc)
    where
      decode = Ed.PublicKey . bytes32ToBS

clanFromShip :: Ship -> Ob.Class
clanFromShip = Ob.clan . Ob.patp . fromIntegral

shipSein :: Ship -> Ship
shipSein = Ship . fromIntegral . Ob.fromPatp . Ob.sein . Ob.patp . fromIntegral


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


-- Validates the keys, life, discontinuity, etc. If everything is ok, return
-- the sponsoring ship for Seed.
validateAndGetSponsor :: Seed -> EthPoint -> Either Text Ship
validateAndGetSponsor (Seed ship life ring mo) EthPoint{..} = do
  let clan = clanFromShip ship

  case clan of
    Ob.Comet  -> do
      -- A comet address is the fingerprint of the keypair
      -- when (ship /= (x ring.seed)) (Left "todo: key mismatch")
      -- A comet can never be breached
      -- when live Left "comet already booted"
      -- TODO: the parent must be launched check?
      Right $ shipSein ship

    -- When the ship is a moon, the only requirement is that the parent is
    -- launched.
    Ob.Moon   -> do
      Left "todo: moon's parent must be launched"

    -- For Galaxies, Stars and Planets, we do the full checks.
    _         -> case epNet of
      Nothing -> Left "ship not keyed"
      Just (netLife, pass, contNum, (hasSponsor, who), _) -> do
        when (netLife /= life) $
            Left $ pack
                 ("keyfile life mismatch; keyfile claims life " ++
                  (show life) ++ ", but Azimuth claims life " ++ (show netLife))
        when ((getPassFromRing ring) /= pass) $
            Left "keyfile does not match blockchain"
        -- TODO: The hoon code does a breach check, but the C code never
        -- supplies the data necessary for it to function.
        pure who



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
retrievePoint :: Quantity -> Address -> Int -> Web3 (EthPoint)
retrievePoint bloq azimuth p =
  withAzimuth bloq azimuth $ do
    (owner, managementProxy, spawnProxy, votingProxy, transferProxy)
      <- rights (fromIntegral p)

    (encryptionKey,
     authenticationKey,
     hasSponsor,
     active,
     escapeRequested,
     sponsor,
     escapeTo,
     cryptoSuite,
     keyRevision, continuityNum) <- points (fromIntegral p)

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

    let epKid = case (clanFromShip $ Ship $ fromIntegral p) of
           Ob.Galaxy -> Just (addressToAtom spawnProxy, None)
           Ob.Star   -> Just (addressToAtom spawnProxy, None)
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

{-
  [%dawn seed sponsor galaxies domains block eth-url]
-}


-- Produces either an error or a validated boot event structure.
dawnVent :: Seed -> RIO e (Either Text Dawn)
dawnVent dSeed@(Seed (Ship ship) life ring oaf) = do
  --
  -- Everyone needs the Ethereum node instead of just the galaxies.

  hs <- runWeb3' provider $ do
    -- Block number (dBloq)
    block <- blockNumber
    print ("boot: eth block: " ++ (show block))

    azimuth <- withAccount () $ Ens.resolve "azimuth.eth"
    print ("Azimuth: " ++ (show azimuth))

    -- TODO: Comets don't go through retrievePoint; this needs to be hoisted
    -- one level.
    p <- retrievePoint block azimuth (fromIntegral ship)
    print $ show p
    let validate = validateAndGetSponsor dSeed p
    sponsorShip <- case validate of
      Left x -> fail $ unpack x
      Right sponsor -> do
        print $ Ob.patp $ fromIntegral sponsor
        pure sponsor

    -- Retrieve the galaxy table [MUST FIX s/5/255/]
    -- galaxyTable <- retrieveGalaxyTable block azimuth
    -- print $ show galaxyTable

    -- Read Ames domains [DONE]
    dTurf <- readAmesDomains block azimuth
    print $ show dTurf

    dSponsor <- retrievePoint block azimuth (fromIntegral sponsorShip)

    -- TODO: I need a Map -> NounMap conversion to turn the galaxyTable into
    -- dCzar.
    let dCzar = None

    -- dNode.
    let dNode = Nothing
    let dBloq :: Bloq = fromIntegral $ unQuantity block

    pure $ MkDawn{..}

  print $ show hs

  pure (Left "bad")

