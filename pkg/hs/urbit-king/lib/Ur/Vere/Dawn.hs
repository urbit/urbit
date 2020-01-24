{-|
    Use etherium to access PKI information.
-}

module Ur.Vere.Dawn where

import Ur.Arvo.Common
import Ur.Arvo.Event  hiding (Address)
import Ur.Prelude     hiding (Call, rights, to)

import Data.Bits                     (xor)
import Data.List                     (nub)
import Data.Text                     (splitOn)
import Network.Ethereum.Account
import Network.Ethereum.Api.Eth
import Network.Ethereum.Api.Provider
import Network.Ethereum.Api.Types    hiding (blockNumber)
import Network.Ethereum.Web3
import Network.HTTP.Client.TLS

import qualified Crypto.Hash.SHA256    as SHA256
import qualified Crypto.Hash.SHA512    as SHA512
import qualified Crypto.Sign.Ed25519   as Ed
import qualified Data.Binary           as B
import qualified Data.ByteArray        as BA
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Network.Ethereum.Ens  as Ens
import qualified Network.HTTP.Client   as C
import qualified Urbit.Azimuth         as AZ
import qualified Urbit.Ob              as Ob

-- During boot, use the infura provider
provider = HttpProvider
  "https://mainnet.infura.io/v3/196a7f37c7d54211b4a07904ec73ad87"

-- Conversion Utilities --------------------------------------------------------

-- Takes the web3's bytes representation and changes the endianness.
bytes32ToBS :: BytesN 32 -> ByteString
bytes32ToBS = reverse . BA.pack . BA.unpack

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

-- Derive public key structure from the key derivation seed structure
ringToPass :: Ring -> Pass
ringToPass Ring{..} = Pass{..}
  where
    passCrypt = decode ringCrypt
    passSign = decode ringSign
    decode = fst . fromJust . Ed.createKeypairFromSeed_
    fromJust = \case
      Nothing -> error "Invalid seed passed to createKeypairFromSeed"
      Just x -> x

-- Azimuth Functions -----------------------------------------------------------

-- Perform a request to azimuth at a certain block number
withAzimuth :: Quantity
            -> Address
            -> DefaultAccount Web3 a
            -> Web3 a
withAzimuth bloq azimuth action =
    withAccount () $
      withParam (to .~ azimuth) $
        withParam (block .~ BlockWithNumber bloq)
          action

-- Retrieves the EthPoint information for an individual point.
retrievePoint :: Quantity -> Address -> Ship -> Web3 EthPoint
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
     keyRevision,
     continuityNum) <- AZ.points (fromIntegral ship)

    let escapeState = if escapeRequested
                      then Just $ Ship $ fromIntegral escapeTo
                      else Nothing

    -- The hoon version also sets this to all 0s and then does nothing with it.
    let epOwn = (0, 0, 0, 0)

    let epNet = if not active
        then Nothing
        else Just
          ( fromIntegral keyRevision
          , passFromEth encryptionKey authenticationKey cryptoSuite
          , fromIntegral continuityNum
          , (hasSponsor, Ship (fromIntegral sponsor))
          , escapeState
          )

    -- TODO: wtf?
    let epKid = case clanFromShip ship of
           Ob.Galaxy -> Just (0, setToHoonSet mempty)
           Ob.Star   -> Just (0, setToHoonSet mempty)
           _         -> Nothing

    pure EthPoint{..}

-- Retrieves information about all the galaxies from Ethereum.
retrieveGalaxyTable :: Quantity -> Address -> Web3 (Map Ship (Rift, Life, Pass))
retrieveGalaxyTable bloq azimuth =
    withAzimuth bloq azimuth $ mapFromList <$> mapM getRow [0..255]
  where
    getRow idx = do
      (encryptionKey, authenticationKey, _, _, _, _, _, cryptoSuite,
       keyRev, continuity) <- AZ.points idx
      pure ( fromIntegral idx
           , ( fromIntegral continuity
             , fromIntegral keyRev
             , passFromEth encryptionKey authenticationKey cryptoSuite
             )
          )

-- Reads the three Ames domains from Ethereum, removing duplicates
readAmesDomains :: Quantity -> Address -> Web3 [Turf]
readAmesDomains bloq azimuth =
    withAzimuth bloq azimuth $ nub <$> mapM getTurf [0..2]
  where
    getTurf idx =
      Turf . fmap Cord . reverse . splitOn "." <$> AZ.dnsDomains idx


validateShipAndGetImmediateSponsor :: Quantity -> Address -> Seed -> Web3 Ship
validateShipAndGetImmediateSponsor block azimuth (Seed ship life ring oaf) =
  case clanFromShip ship of
    Ob.Comet -> validateComet
    Ob.Moon  -> validateMoon
    _        -> validateRest
  where
    validateComet = do
      -- A comet address is the fingerprint of the keypair
      let shipFromPass = cometFingerprint $ ringToPass ring
      when (ship /= shipFromPass) $
        fail ("comet name doesn't match fingerprint " ++ show ship ++ " vs " ++
              show shipFromPass)
      when (life /= 1) $
        fail ("comet can never be re-keyed")
      pure (shipSein ship)

    validateMoon = do
      -- TODO: The current code in zuse does nothing, but we should be able to
      -- try to validate the oath against the current as exists planet on
      -- chain.
      pure $ shipSein ship

    validateRest = do
      putStrLn ("boot: retrieving " ++ renderShip ship ++ "'s public keys")

      whoP <- retrievePoint block azimuth ship
      case epNet whoP of
        Nothing -> fail "ship not keyed"
        Just (netLife, pass, contNum, (hasSponsor, who), _) -> do
          when (netLife /= life) $
              fail ("keyfile life mismatch; keyfile claims life " ++
                    show life ++ ", but Azimuth claims life " ++
                    show netLife)
          when ((ringToPass ring) /= pass) $
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
      putStrLn ("boot: retrieving keys for sponsor " ++ renderShip ship)
      ethPoint <- retrievePoint block azimuth ship

      case (clanFromShip ship, epNet ethPoint) of
        (Ob.Comet, _) -> fail "Comets cannot be sponsors"
        (Ob.Moon, _)  -> fail "Moons cannot be sponsors"

        (_, Nothing) ->
            fail $ unpack ("Ship " ++ renderShip ship ++ " not booted")

        (Ob.Galaxy, Just _) -> pure [(ship, ethPoint)]

        (_, Just (_, _, _, (False, _), _)) ->
            fail $ unpack ("Ship " ++ renderShip ship ++ " has no sponsor")

        (_, Just (_, _, _, (True, sponsor), _)) -> do
            chain <- loop sponsor
            pure $ chain ++ [(ship, ethPoint)]


-- Produces either an error or a validated boot event structure.
dawnVent :: Seed -> RIO e (Either Text Dawn)
dawnVent dSeed@(Seed ship life ring oaf) = do
  ret <- runWeb3' provider $ do
    block <- blockNumber
    putStrLn ("boot: ethereum block #" ++ tshow block)

    putStrLn "boot: retrieving azimuth contract"
    azimuth <- withAccount () $ Ens.resolve "azimuth.eth"

    immediateSponsor <- validateShipAndGetImmediateSponsor block azimuth dSeed
    dSponsor <- getSponsorshipChain block azimuth immediateSponsor

    putStrLn "boot: retrieving galaxy table"
    dCzar <- mapToHoonMap <$> retrieveGalaxyTable block azimuth

    putStrLn "boot: retrieving network domains"
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


-- Comet Mining ----------------------------------------------------------------

mix :: BS.ByteString -> BS.ByteString -> BS.ByteString
mix a b = BS.pack $ loop (BS.unpack a) (BS.unpack b)
  where
    loop [] []         = []
    loop a  []         = a
    loop [] b          = b
    loop (x:xs) (y:ys) = (xor x y) : loop xs ys

shas :: BS.ByteString -> BS.ByteString -> BS.ByteString
shas salt = SHA256.hash . mix salt . SHA256.hash

shaf :: BS.ByteString -> BS.ByteString -> BS.ByteString
shaf salt ruz = (mix a b)
  where
    haz = shas salt ruz
    a = (take 16 haz)
    b = (drop 16 haz)

cometFingerprintBS :: Pass -> ByteString
cometFingerprintBS = (shaf $ C.pack "bfig") . passToBS

cometFingerprint :: Pass -> Ship
cometFingerprint = Ship . B.decode . fromStrict . reverse . cometFingerprintBS

tryMineComet :: Set Ship -> Word64 -> Maybe Seed
tryMineComet ships seed =
  if member shipSponsor ships
  then Just $ Seed shipName 1 ring Nothing
  else Nothing
  where
    -- Hash the incoming seed into a 64 bytes.
    baseHash = SHA512.hash $ toStrict $ B.encode seed
    signSeed = (take 32 baseHash)
    ringSeed = (drop 32 baseHash)
    ring = Ring signSeed ringSeed
    pass = ringToPass ring
    shipName = cometFingerprint pass
    shipSponsor = shipSein shipName

mineComet :: Set Ship -> Word64 -> Seed
mineComet ships = loop
  where
    loop eny =
      case (tryMineComet ships eny) of
        Nothing -> loop (eny + 1)
        Just x  -> x
