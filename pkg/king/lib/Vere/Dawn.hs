{-# OPTIONS_GHC -Wwarn #-}
module Vere.Dawn where

import Arvo.Common
import Arvo.Event      hiding (Address)
import Azimuth.Azimuth
import UrbitPrelude    hiding (Call, rights, to)

import Data.List                     (nub)
import Data.Maybe
import Data.Solidity.Abi.Codec       (encode)
import Data.Text                     (splitOn)
import Network.Ethereum.Account
import Network.Ethereum.Api.Eth
import Network.Ethereum.Api.Provider
import Network.Ethereum.Api.Types    hiding (blockNumber)
import Network.Ethereum.Web3
import Network.HTTP.Client.TLS

import qualified Crypto.Sign.Ed25519  as Ed
import qualified Data.ByteArray       as BA
import qualified Data.Map.Strict      as M
import qualified Network.Ethereum.Ens as Ens
import qualified Network.HTTP.Client  as C
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

-- Retrieves the EthPoint information for an individual point.
retrievePoint :: Quantity -> Address -> Ship -> Web3 (EthPoint)
retrievePoint bloq azimuth ship =
  withAzimuth bloq azimuth $ do
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

    -- The hoon version also sets this to all 0s and then does nothing with it.
    let epOwn = (0, 0, 0, 0)

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
           Ob.Galaxy -> Just (0, setToHoonSet mempty)
           Ob.Star   -> Just (0, setToHoonSet mempty)
           _         -> Nothing

    pure EthPoint{..}

-- Retrieves information about all the galaxies from Ethereum.
retrieveGalaxyTable :: Quantity -> Address -> Web3 (Map Ship (Rift, Life, Pass))
retrieveGalaxyTable bloq azimuth =
    withAzimuth bloq azimuth $ M.fromList <$> mapM getRow [0..255]
  where
    getRow idx = do
      (encryptionKey, authenticationKey, _, _, _, _, _, cryptoSuite,
       keyRev, continuity) <- points idx
      pure (fromIntegral idx,
            (fromIntegral continuity,
              fromIntegral keyRev,
              (passFromEth encryptionKey authenticationKey cryptoSuite)))

-- Reads the three Ames domains from Ethereum, removing duplicates
readAmesDomains :: Quantity -> Address -> Web3 ([Turf])
readAmesDomains bloq azimuth =
    withAzimuth bloq azimuth $ nub <$> mapM getTurf [0..2]
  where
    getTurf idx = do
      str <- dnsDomains idx
      pure $ Turf $ fmap Cord $ reverse $ splitOn "." str

validateShipAndGetImmediateSponsor :: Quantity -> Address -> Seed -> Web3 (Ship)
validateShipAndGetImmediateSponsor block azimuth (Seed ship life ring oaf) =
  case clanFromShip ship of
    Ob.Comet -> validateComet
    Ob.Moon  -> validateMoon
    _        -> validateRest
  where
    validateComet = do
      -- TODO: All validation of the comet.
      -- A comet address is the fingerprint of the keypair
      -- when (ship /= (x ring.seed)) (Left "todo: key mismatch")
      -- A comet can never be breached
      -- when live Left "comet already booted"
      -- TODO: the parent must be launched check?
      pure (shipSein ship)

    validateMoon = do
      -- TODO: The current code in zuse does nothing, but we should be able to
      -- try to validate the oath against the current as exists planet on
      -- chain.
      pure $ shipSein ship

    validateRest = do
      print ("boot: retrieving " ++ (renderShip ship) ++ "'s public keys")

      whoP <- retrievePoint block azimuth ship
      case (epNet whoP) of
        Nothing -> fail "ship not keyed"
        Just (netLife, pass, contNum, (hasSponsor, who), _) -> do
          when (netLife /= life) $
              fail $ pack
                   ("keyfile life mismatch; keyfile claims life " ++
                    (show life) ++ ", but Azimuth claims life " ++
                    (show netLife))
          when ((getPassFromRing ring) /= pass) $
              fail "keyfile does not match blockchain"
          -- TODO: The hoon code does a breach check, but the C code never
          -- supplies the data necessary for it to function.
          pure who


-- Walk through the sponsorship chain retrieving the actual sponsorship chain
-- as it exists on Ethereum.
getSponsorshipChain :: Quantity -> Address -> Ship -> Web3 [(Ship,EthPoint)]
getSponsorshipChain block azimuth = loop
  where
    loop ship = do
      print ("boot: retrieving keys for sponsor " ++ (renderShip ship))
      ethPoint <- retrievePoint block azimuth ship

      case clanFromShip ship of
        Ob.Comet -> fail "Comets cannot be sponsors"
        Ob.Moon  -> fail "Moons cannot be sponsors"
        Ob.Galaxy -> do
          case (epNet ethPoint) of
            Nothing -> fail $ unpack ("Galaxy " ++ (renderShip ship) ++
                                      " not booted")
            Just _  -> pure [(ship, ethPoint)]
        _ -> do
          case (epNet ethPoint) of
            Nothing -> fail $ unpack ("Ship " ++ (renderShip ship) ++
                                      " not booted")
            Just (_, _, _, (has, sponsor), _) -> do
              case has of
                False -> fail $ unpack ("Ship " ++ (renderShip ship) ++
                                        " has no sponsor")
                True -> do
                  chain <- loop sponsor
                  pure $ chain ++ [(ship, ethPoint)]


-- Produces either an error or a validated boot event structure.
dawnVent :: Seed -> RIO e (Either Text Dawn)
dawnVent dSeed@(Seed ship life ring oaf) = do
  ret <- runWeb3' provider $ do
    block <- blockNumber
    print ("boot: ethereum block #" ++ (show block))

    print "boot: retrieving azimuth contract"
    azimuth <- withAccount () $ Ens.resolve "azimuth.eth"

    immediateSponsor <- validateShipAndGetImmediateSponsor block azimuth dSeed
    dSponsor <- getSponsorshipChain block azimuth immediateSponsor

    print "boot: retrieving galaxy table"
    dCzar <- mapToHoonMap <$> retrieveGalaxyTable block azimuth

    print "boot: retrieving network domains"
    dTurf <- readAmesDomains block azimuth

    let dBloq = toBloq block
    let dNode = Nothing
    pure $ MkDawn{..}

  case ret of
    Left x  -> pure $ Left $ tshow x
    Right y -> pure $ Right y


dawnCometList :: RIO e [Ship]
dawnCometList = do
  -- Get the jamfile with the list of stars accepting comets right now.
  manager <- io $ C.newManager tlsManagerSettings
  request <- io $ C.parseRequest "https://bootstrap.urbit.org/comet-stars.jam"
  response <- io $ C.httpLbs (C.setRequestCheckStatus request) manager
  let body = toStrict $ C.responseBody response

  noun <- cueBS body & either throwIO pure
  fromNounErr noun & either (throwIO . uncurry ParseErr) pure


mineComet :: Set Ship -> Word128 -> Seed
mineComet ships = loop
  where
    loop eny =
      loop (eny + 1)

-- dawnCome :: RIO e (Either Text Dawn)
-- dawnCome = do
