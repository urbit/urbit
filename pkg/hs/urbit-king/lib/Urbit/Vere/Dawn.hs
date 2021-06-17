{-|
    Use etherium to access PKI information.
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

import Data.Bits                     (xor)
import Data.List                     (nub)
import Data.Text                     (splitOn)
import Data.Aeson
import Data.HexString
import Numeric                       (showHex)

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

-- The address of the azimuth contract as a string.
azimuthAddr :: Text
azimuthAddr = "0x223c067f8cf28ae173ee5cafea60ca44c335fecb"

-- Conversion Utilities --------------------------------------------------------

passFromBS :: ByteString -> ByteString -> ByteString -> Pass
passFromBS enc aut sut
  | bytesAtom sut /= 1 = Pass (Ed.PublicKey mempty) (Ed.PublicKey mempty)
  | otherwise          = Pass (Ed.PublicKey aut) (Ed.PublicKey enc)

bsToBool :: ByteString -> Bool
bsToBool bs = bytesAtom bs == 1

clanFromShip :: Ship -> Ob.Class
clanFromShip = Ob.clan . Ob.patp . fromIntegral

shipSein :: Ship -> Ship
shipSein = Ship . fromIntegral . Ob.fromPatp . Ob.sein . Ob.patp . fromIntegral

renderShip :: Ship -> Text
renderShip = Ob.renderPatp . Ob.patp . fromIntegral

hexStrToAtom :: Text -> Atom
hexStrToAtom =
  bytesAtom . reverse . toBytes . hexString . removePrefix . encodeUtf8

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

-- The big problem here is that we can't really use the generated web3 wrappers
-- around the azimuth contracts, especially for the galaxy table request. They
-- make multiple rpc invocations per galaxy request (which aren't even
-- batched!), while Vere built a single batched rpc call to fetch the entire
-- galaxy table.
--
-- The included Network.JsonRpc.TinyClient that Network.Web3 embeds can't do
-- batches, so calling that directly is out.
--
-- Network.JSONRPC appears to not like something about the JSON that Infura
-- returns; it's just hanging? Also no documentation.
--
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

data RawResponse = RawResponse
  { rrId :: Int
  , rrResult :: Text
  }
  deriving (Show)

instance FromJSON RawResponse where
  parseJSON = withObject "Response" $ \v -> do
    rrId <- v .: "id"
    rrResult <- v .: "result"
    pure RawResponse{..}


-- Given a list of methods and parameters, return a list of decoded responses.
dawnPostRequests :: forall req e resp
                  . (ToJSON req, RequestMethod req)
                 => String
                 -> (req -> Text -> resp)
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
                                    , "id"      .= rid
                                    ]

-- Azimuth JSON Requests -------------------------------------------------------

-- Not a full implementation of the Ethereum ABI, but just the ability to call
-- a method by encoded id (like 0x63fa9a87 for `points(uint32)`), and a single
-- UIntN 32 parameter.
encodeCall :: Text -> Int -> Text
encodeCall method idx = method <> leadingZeroes <> renderedNumber
  where
    renderedNumber = pack $ showHex idx ""
    leadingZeroes = replicate (64 - length renderedNumber) '0'

data BlockRequest = BlockRequest
  deriving (Show, Eq)

instance RequestMethod BlockRequest where
  getRequestMethod BlockRequest = "eth_blockNumber"

instance ToJSON BlockRequest where
  toJSON BlockRequest = Array $ fromList []

-- No need to parse, it's already in the format we'll pass as an argument to
-- eth calls which take a block number.
parseBlockRequest :: BlockRequest -> Text -> TextBlockNum
parseBlockRequest _ txt = txt

type TextBlockNum = Text

data PointRequest = PointRequest
  { grqHexBlockNum :: TextBlockNum
  , grqPointId :: Int
  } deriving (Show, Eq)

instance RequestMethod PointRequest where
  getRequestMethod PointRequest{..} = "eth_call"

instance ToJSON PointRequest where
  -- 0x63fa9a87 is the points(uint32) call.
  toJSON PointRequest{..} =
    Array $ fromList [object [ "to" .= azimuthAddr
                             , "data" .= encodeCall "0x63fa9a87" grqPointId],
                      String grqHexBlockNum
                     ]

parseAndChunkResultToBS :: Text -> [ByteString]
parseAndChunkResultToBS result =
  map reverse $
  chunkBytestring 32 $
  toBytes $
  hexString $
  removePrefix $
  encodeUtf8 result

-- The incoming result is a text bytestring. We need to take that text, and
-- spit out the parsed data.
--
-- We're sort of lucky here. After removing the front "0x", we can just chop
-- the incoming text string into 10 different 64 character chunks and then
-- parse them as numbers.
parseEthPoint :: PointRequest -> Text -> EthPoint
parseEthPoint PointRequest{..} result = EthPoint{..}
 where
  [rawEncryptionKey,
   rawAuthenticationKey,
   rawHasSponsor,
   rawActive,
   rawEscapeRequested,
   rawSponsor,
   rawEscapeTo,
   rawCryptoSuite,
   rawKeyRevision,
   rawContinuityNum] = parseAndChunkResultToBS result

  escapeState = if bsToBool rawEscapeRequested
                then Just $ Ship $ fromIntegral $ bytesAtom rawEscapeTo
                else Nothing

  -- Vere doesn't set ownership information, neither did the old Dawn.hs
  -- implementation.
  epOwn = (0, 0, 0, 0)

  epNet = if not $ bsToBool rawActive
    then Nothing
    else Just
      ( fromIntegral $ bytesAtom rawKeyRevision
      , passFromBS rawEncryptionKey rawAuthenticationKey rawCryptoSuite
      , fromIntegral $ bytesAtom rawContinuityNum
      , (bsToBool rawHasSponsor,
         Ship (fromIntegral $ bytesAtom rawSponsor))
      , escapeState
      )

  -- I don't know what this is supposed to be, other than the old Dawn.hs and
  -- dawn.c do the same thing.
  epKid = case clanFromShip (Ship $ fromIntegral grqPointId) of
    Ob.Galaxy -> Just (0, setToHoonSet mempty)
    Ob.Star   -> Just (0, setToHoonSet mempty)
    _         -> Nothing

-- Preprocess data from a point request into the form used in the galaxy table.
parseGalaxyTableEntry :: PointRequest -> Text -> (Ship, (Rift, Life, Pass))
parseGalaxyTableEntry PointRequest{..} result = (ship, (rift, life, pass))
 where
  [rawEncryptionKey,
   rawAuthenticationKey,
   _, _, _, _, _,
   rawCryptoSuite,
   rawKeyRevision,
   rawContinuityNum] = parseAndChunkResultToBS result

  ship = Ship $ fromIntegral grqPointId
  rift = fromIntegral $ bytesAtom rawContinuityNum
  life = fromIntegral $ bytesAtom rawKeyRevision
  pass = passFromBS rawEncryptionKey rawAuthenticationKey rawCryptoSuite

removePrefix :: ByteString -> ByteString
removePrefix withOhEx
  | prefix == "0x" = suffix
  | otherwise      = error "not prefixed with 0x"
 where
  (prefix, suffix) = splitAt 2 withOhEx

chunkBytestring :: Int -> ByteString -> [ByteString]
chunkBytestring size bs
  | null rest = [cur]
  | otherwise = (cur : chunkBytestring size rest)
 where
  (cur, rest) = splitAt size bs

data TurfRequest = TurfRequest
  { trqHexBlockNum :: TextBlockNum
  , trqTurfId :: Int
  } deriving (Show, Eq)

instance RequestMethod TurfRequest where
  getRequestMethod TurfRequest{..} = "eth_call"

instance ToJSON TurfRequest where
  -- 0xeccc8ff1 is the dnsDomains(uint32) call.
  toJSON TurfRequest{..} =
    Array $ fromList [object [ "to" .= azimuthAddr
                             , "data" .= encodeCall "0xeccc8ff1" trqTurfId],
                      String trqHexBlockNum
                     ]

-- This is another hack instead of a full Ethereum ABI response.
parseTurfResponse :: TurfRequest -> Text -> Turf
parseTurfResponse a raw = turf
  where
    without0x = removePrefix $ encodeUtf8 raw
    (_, blRest) = splitAt 64 without0x
    (utfLenStr, utfStr) = splitAt 64 blRest
    utfLen = fromIntegral $ bytesAtom $ reverse $ toBytes $ hexString utfLenStr
    dnsStr = decodeUtf8 $ BS.take utfLen $ toBytes $ hexString utfStr
    turf = Turf $ fmap Cord $ reverse $ splitOn "." dnsStr

-- Azimuth Functions -----------------------------------------------------------

retrievePoint :: String -> TextBlockNum -> Ship -> RIO e EthPoint
retrievePoint endpoint block ship =
  dawnPostRequests endpoint parseEthPoint
    [PointRequest block (fromIntegral ship)] >>= \case
      [x] -> pure x
      _   -> error "JSON server returned multiple return values."

validateFeedAndGetSponsor :: String
                          -> TextBlockNum
                          -> Feed
                          -> RIO e (Seed, Ship)
validateFeedAndGetSponsor endpoint block = \case
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
          whoP <- retrievePoint endpoint block ship
          case epNet whoP of
            Nothing -> pure $ Left "ship not keyed"
            Just (netLife, pass, contNum, (hasSponsor, who), _) -> do
              if (netLife /= life) then
                pure $ Left ("keyfile life mismatch; keyfile claims life " <>
                             show life <> ", but Azimuth claims life " <>
                             show netLife)
              else if ((ringToPass ring) /= pass) then
                pure $ Left "keyfile does not match blockchain"
              -- TODO: The hoon code does a breach check, but the C code never
              -- supplies the data necessary for it to function.
              else
                pure $ Right who


-- Walk through the sponsorship chain retrieving the actual sponsorship chain
-- as it exists on Ethereum.
getSponsorshipChain :: String -> TextBlockNum -> Ship -> RIO e [(Ship,EthPoint)]
getSponsorshipChain endpoint block = loop
  where
    loop ship = do
      putStrLn ("boot: retrieving keys for sponsor " <> renderShip ship)
      ethPoint <- retrievePoint endpoint block ship

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
    putStrLn ("boot: requesting ethereum information from " <> pack provider)
    blockResponses
      <- dawnPostRequests provider parseBlockRequest [BlockRequest]

    hexStrBlock <- case blockResponses of
      [num] -> pure num
      x     -> error "Unexpected multiple returns from block # request"

    let dBloq = hexStrToAtom hexStrBlock
    putStrLn ("boot: ethereum block #" <> tshow dBloq)

    (dSeed, immediateSponsor)
      <- validateFeedAndGetSponsor provider hexStrBlock feed
    dSponsor <- getSponsorshipChain provider hexStrBlock immediateSponsor

    putStrLn "boot: retrieving galaxy table"
    dCzar <- (mapToHoonMap . mapFromList) <$>
      (dawnPostRequests provider parseGalaxyTableEntry $
         map (PointRequest hexStrBlock) [0..255])

    putStrLn "boot: retrieving network domains"
    dTurf <- nub <$> (dawnPostRequests provider parseTurfResponse $
      map (TurfRequest hexStrBlock) [0..2])

    let dNode = Nothing

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
