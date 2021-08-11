{-|
    Use L2 to access PKI information.
-}

module Urbit.Vere.Dawn ( dawnVent
                       , dawnCometList
                       , renderShip
                       , mineComet
                       -- Used only in testing
                       , mix
                       , shas
                       , shaf
                       , deriveCode
                       , cometFingerprintBS
                       , cometFingerprint
                       ) where

import Urbit.Arvo.Common
import Urbit.Arvo.Event  hiding (Address)
import Urbit.Prelude     hiding (rights, to, (.=))

import Prelude (read)

import Data.Bits                     (xor)
import Data.List                     (nub)
import Data.Text                     (splitOn)
import Data.Aeson
import Data.HexString

import qualified Crypto.Hash.SHA256    as SHA256
import qualified Crypto.Hash.SHA512    as SHA512
import qualified Crypto.Sign.Ed25519   as Ed
import qualified Data.Binary           as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as L
import qualified Network.HTTP.Client   as C
import qualified Urbit.Ob              as Ob

import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types      as HT

-- Conversion Utilities --------------------------------------------------------

passFromText :: Text -> Text -> Int -> Pass
passFromText enc aut sut
  | sut /= 1  = Pass (Ed.PublicKey mempty) (Ed.PublicKey mempty)
  | otherwise = Pass (Ed.PublicKey $ grab aut) (Ed.PublicKey $ grab enc)
  where
    grab = reverse . toBytes . hexString . removePrefix . encodeUtf8

clanFromShip :: Ship -> Ob.Class
clanFromShip = Ob.clan . Ob.patp . fromIntegral

shipSein :: Ship -> Ship
shipSein = Ship . fromIntegral . Ob.fromPatp . Ob.sein . Ob.patp . fromIntegral

renderShip :: Ship -> Text
renderShip = Ob.renderPatp . Ob.patp . fromIntegral

onLeft :: (a -> b) -> Either a c -> Either b c
onLeft fun = bimap fun id

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

-- JSONRPC Functions -----------------------------------------------------------

-- Network.JSONRPC appeared to not like something about the JSON that Infura
-- returned; it just hung? Also no documentation.
--
-- Our use case here is simple enough.
-- So, like with Vere, we roll our own.

dawnSendHTTP :: String -> L.ByteString -> RIO e (Either Int L.ByteString)
dawnSendHTTP endpoint requestData = liftIO do
  manager <- C.newManager TLS.tlsManagerSettings

  initialRequest <- C.parseRequest endpoint
  let request = initialRequest
        { C.method = "POST"
        , C.requestBody = C.RequestBodyLBS $ requestData
        , C.requestHeaders = [("Accept", "application/json"),
                              ("Content-Type", "application/json"),
                              ("Charsets", "utf-8")]
        }

  response <- C.httpLbs request manager

  -- Return body if 200.
  let code = HT.statusCode $ C.responseStatus response
  case code of
    200 -> pure $ Right $ C.responseBody response
    _   -> pure $ Left code

class RequestMethod m where
  getRequestMethod :: m -> Text

data RawResponse r = RawResponse
  { rrId :: Int
  , rrResult :: r
  }
  deriving (Show)

instance FromJSON r => FromJSON (RawResponse r) where
  parseJSON = withObject "Response" $ \v -> do
    rawId <- v .: "id"
    rrResult <- v .: "result"
    let rrId = read rawId
    pure RawResponse{..}


-- Given a list of methods and parameters, return a list of decoded responses.
dawnPostRequests :: forall req e resp res
                  . (ToJSON req, RequestMethod req, FromJSON res)
                 => String
                 -> (req -> res -> resp)
                 -> [req]
                 -> RIO e [resp]
dawnPostRequests endpoint responseBuilder requests = do
  -- Encode our input requests
  let requestPayload =
        encode $ Array $ fromList $ fmap toFullRequest $ zip [0..] requests

  -- Send to the server
  responses <- dawnSendHTTP endpoint requestPayload >>= \case
    Left err -> error $ "error fetching " <> endpoint <> ": HTTP " <> (show err)
    Right x -> pure x

  -- Get a list of the result texts in the order of the submitted requests
  rawSorted <- case decode responses of
    Nothing -> error $ "couldn't decode json"
    Just x  -> pure $ map rrResult $ sortOn rrId x

  -- Build the final result structure by calling the passed in builder with the
  -- request (some outputs need data from the request structure, eitherwise,
  -- we'd lean on FromJSON).
  let results = map (uncurry responseBuilder) (zip requests rawSorted)
  pure results

 where
  toFullRequest :: (Int, req) -> Value
  toFullRequest (rid, req) = object [ "jsonrpc" .= ("2.0" :: Text)
                                    , "method"  .= getRequestMethod req
                                    , "params"  .= req
                                    , "id"      .= (show rid)
                                    ]

-- Azimuth JSON Requests -------------------------------------------------------

data PointResponse = PointResponse
  --NOTE  also contains dominion and ownership, but not actually used here
  { prNetwork :: PointNetwork
  } deriving (Show, Eq, Generic)

instance FromJSON PointResponse where
  parseJSON (Object o) = do
    prNetwork <- o .: "network"
    pure PointResponse{..}
  parseJSON _ = do error "failed to parse PointResponse json"

data PointNetwork = PointNetwork
  { pnKeys :: PointKeys
  , pnSponsor :: PointSponsor
  , pnRift :: ContNum
  } deriving (Show, Eq, Generic)

instance FromJSON PointNetwork where
  parseJSON (Object o) = do
    pnKeys <- o .: "keys"
    pnSponsor <- o .: "sponsor"
    pnRift <- o .: "rift"
    pure PointNetwork{..}
  parseJSON _ = do error "failed to parse PointNetwork json"

data PointKeys = PointKeys
  { pkLife :: Life
  , pkSuite :: Int
  , pkAuth :: Text  --NOTE  0xhex string
  , pkCrypt :: Text  --NOTE  0xhex string
  } deriving (Show, Eq, Generic)

instance FromJSON PointKeys where
  parseJSON (Object o) = do
    pkLife <- o .: "life"
    pkSuite <- o .: "suite"
    pkAuth <- o .: "auth"
    pkCrypt <- o .: "crypt"
    pure PointKeys{..}
  parseJSON _ = do error "failed to parse PointKeys json"

data PointSponsor = PointSponsor
  { psHas :: Bool
  , psWho :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON PointSponsor where
  parseJSON (Object o) = do
    psHas <- o .: "has"
    psWho <- o .: "who"
    pure PointSponsor{..}
  parseJSON _ = do error "failed to parse PointSponsor json"

data PointRequest = PointRequest Ship

instance RequestMethod PointRequest where
  getRequestMethod _ = "getPoint"

instance ToJSON PointRequest where
  toJSON (PointRequest point) = object [ "ship" .= renderShip point ]

parseAzimuthPoint :: PointRequest -> PointResponse -> EthPoint
parseAzimuthPoint (PointRequest point) response = EthPoint{..}
 where
  net = prNetwork response
  key = pnKeys net

  -- Vere doesn't set ownership information, neither did the old Dawn.hs
  -- implementation.
  epOwn = (0, 0, 0, 0)

  sponsorShip = Ob.parsePatp $ psWho $ pnSponsor net
  epNet = if (pkLife key) == 0
    then Nothing
    else case sponsorShip of
      Left _  -> Nothing
      Right s -> Just
        ( fromIntegral $ pkLife key
        , passFromText (pkCrypt key) (pkAuth key) (pkSuite key)
        , fromIntegral $ pnRift net
        , (psHas $ pnSponsor net, Ship $ fromIntegral $ Ob.fromPatp s)
        , Nothing  --NOTE  goes unused currently, so we simply put Nothing
        )

  -- I don't know what this is supposed to be, other than the old Dawn.hs and
  -- dawn.c do the same thing.
  -- zero-fill spawn data
  epKid = case clanFromShip (Ship $ fromIntegral point) of
    Ob.Galaxy -> Just (0, setToHoonSet mempty)
    Ob.Star   -> Just (0, setToHoonSet mempty)
    _         -> Nothing

-- Preprocess data from a point request into the form used in the galaxy table.
parseGalaxyTableEntry :: PointRequest -> PointResponse -> (Ship, (Rift, Life, Pass))
parseGalaxyTableEntry (PointRequest point) response = (ship, (rift, life, pass))
 where
  net  = prNetwork response
  keys = pnKeys net

  ship = Ship $ fromIntegral point
  rift = fromIntegral $ pnRift net
  life = fromIntegral $ pkLife keys
  pass = passFromText (pkCrypt keys) (pkAuth keys) (pkSuite keys)

removePrefix :: ByteString -> ByteString
removePrefix withOhEx
  | prefix == "0x" = suffix
  | otherwise      = error "not prefixed with 0x"
 where
  (prefix, suffix) = splitAt 2 withOhEx

data TurfRequest = TurfRequest ()

instance RequestMethod TurfRequest where
  getRequestMethod _ = "getDns"

instance ToJSON TurfRequest where
  toJSON _ = object []  --NOTE  getDns takes no parameters

parseTurfResponse :: TurfRequest -> [Text] -> [Turf]
parseTurfResponse _ = map turf
   where
    turf t = Turf $ fmap Cord $ reverse $ splitOn "." t

-- Azimuth Functions -----------------------------------------------------------

retrievePoint :: String -> Ship -> RIO e EthPoint
retrievePoint endpoint ship =
  dawnPostRequests endpoint parseAzimuthPoint [PointRequest ship]
    >>= \case
      [x] -> pure x
      _   -> error "JSON server returned multiple return values."

validateFeedAndGetSponsor :: String
                          -> Feed
                          -> RIO e (Seed, Ship)
validateFeedAndGetSponsor endpoint = \case
    Feed0 s -> do
      r <- validateSeed s
      case r of
        Left e  -> error e
        Right r -> pure (s, r)
    Feed1 s -> validateGerms s

  where
    validateGerms Germs{..} =
      case gFeed of
        []    -> error "no usable keys in keyfile"
        (Germ{..}:f) -> do
                  let seed = Seed gShip gLife gRing Nothing
                  r :: Either String Ship
                    <- validateSeed seed
                  case r of
                    Left _  -> validateGerms $ Germs gShip f
                    Right r -> pure (seed, r)

    validateSeed (Seed ship life ring oaf) =
      case clanFromShip ship of
        Ob.Comet -> validateComet
        Ob.Moon  -> validateMoon
        _        -> validateRest
      where
        validateComet = do
          -- A comet address is the fingerprint of the keypair
          let shipFromPass = cometFingerprint $ ringToPass ring
          if (ship /= shipFromPass) then
            pure $ Left ("comet name doesn't match fingerprint " <>
                         show ship <> " vs " <>
                         show shipFromPass)
          else if (life /= 1) then
            pure $ Left "comet can never be re-keyed"
          else
            pure $ Right (shipSein ship)

        validateMoon = do
          -- TODO: The current code in zuse does nothing, but we should be able
          -- to try to validate the oath against the current as exists planet
          -- on chain.
          pure $ Right $ shipSein ship

        validateRest = do
          putStrLn ("boot: retrieving " <> renderShip ship <> "'s public keys")

          --TODO  could cache this lookup
          whoP <- retrievePoint endpoint ship
          case epNet whoP of
            Nothing -> pure $ Left "ship not keyed"
            Just (netLife, pass, contNum, (hasSponsor, who), _) -> do
              if (netLife /= life) then
                pure $ Left ("keyfile life mismatch; keyfile claims life " <>
                             show life <> ", but Azimuth claims life " <>
                             show netLife)
              else if ((ringToPass ring) /= pass) then
                pure $ Left "keyfile does not match Azimuth"
              -- TODO: The hoon code does a breach check, but the C code never
              -- supplies the data necessary for it to function.
              else
                pure $ Right who


-- Walk through the sponsorship chain retrieving the actual sponsorship chain
-- as it exists on Azimuth.
getSponsorshipChain :: String -> Ship -> RIO e [(Ship,EthPoint)]
getSponsorshipChain endpoint = loop
  where
    loop ship = do
      putStrLn ("boot: retrieving keys for sponsor " <> renderShip ship)
      ethPoint <- retrievePoint endpoint ship

      case (clanFromShip ship, epNet ethPoint) of
        (Ob.Comet, _) -> error "Comets cannot be sponsors"
        (Ob.Moon, _)  -> error "Moons cannot be sponsors"

        (_, Nothing) ->
            error $ unpack ("Ship " <> renderShip ship <> " not booted")

        (Ob.Galaxy, Just _) -> pure [(ship, ethPoint)]

        (_, Just (_, _, _, (False, _), _)) ->
            error $ unpack ("Ship " <> renderShip ship <> " has no sponsor")

        (_, Just (_, _, _, (True, sponsor), _)) -> do
            chain <- loop sponsor
            pure $ chain <> [(ship, ethPoint)]

-- Produces either an error or a validated boot event structure.
dawnVent :: HasLogFunc e => String -> Feed -> RIO e (Either Text Dawn)
dawnVent provider feed =
  -- The type checker can't figure this out on its own.
  (onLeft tshow :: Either SomeException Dawn -> Either Text Dawn) <$> try do
    putStrLn ("boot: requesting L2 Azimuth information from " <> pack provider)

    (dSeed, immediateSponsor)
      <- validateFeedAndGetSponsor provider feed
    dSponsor <- getSponsorshipChain provider immediateSponsor

    putStrLn "boot: retrieving galaxy table"
    dCzar <- (mapToHoonMap . mapFromList) <$>
      (dawnPostRequests provider parseGalaxyTableEntry (map (PointRequest . Ship . fromIntegral) [0..255]))

    putStrLn "boot: retrieving network domains"
    dTurf <- (dawnPostRequests provider parseTurfResponse [TurfRequest ()])
               >>= \case
                 []  -> pure []
                 [t] -> pure (nub t)
                 _   -> error "too many turf responses"

    let dNode = Nothing

    --NOTE  blocknum of 0 is fine because jael ignores it.
    --      should probably be removed from dawn event.
    let dBloq = 0

    pure MkDawn{..}


-- Comet List ------------------------------------------------------------------

dawnCometList :: RIO e [Ship]
dawnCometList = do
  -- Get the jamfile with the list of stars accepting comets right now.
  manager <- io $ C.newManager TLS.tlsManagerSettings
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

shax :: BS.ByteString -> BS.ByteString
shax = SHA256.hash

shas :: BS.ByteString -> BS.ByteString -> BS.ByteString
shas salt = shax . mix salt . shax

shaf :: BS.ByteString -> BS.ByteString -> BS.ByteString
shaf salt ruz = (mix a b)
  where
    haz = shas salt ruz
    a = (take 16 haz)
    b = (drop 16 haz)

-- Given a ring, derives the network login code.
--
-- Note that the network code is a patp, not a patq: the bytes have been
-- scrambled.
deriveCode :: Ring -> Ob.Patp
deriveCode Ring {..} = Ob.patp $
                       bytesAtom $
                       take 8 $
                       shaf (C.pack "pass") $
                       shax $
                       C.singleton 'B' <> ringSign <> ringCrypt

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
