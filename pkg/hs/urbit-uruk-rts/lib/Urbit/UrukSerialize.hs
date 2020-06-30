module Urbit.UrukSerialize where

import ClassyPrelude hiding (readFile, toList, writeFile)

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.ByteString           hiding (putStrLn, unpack)
import Data.Foldable
import Data.HexString
import Data.Primitive.SmallArray
import Numeric.Natural           (Natural)
import System.Directory
import System.FilePath
import Urbit.Pos
import Urbit.UrukRTS.Types

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Urbit.Uruk.Dash.Exp       as Exp
import qualified Urbit.UrukRTS.JetOptimize as Opt
import qualified Urbit.UrukRTS.OptToFast   as Opt


type PierIO = ReaderT FilePath IO

instance Serialise Hash

instance Serialise Pos where
  encode (MkPos p) = encodeInteger $ toInteger p
  decode = do
    i <- decodeInteger
    pure $ MkPos (fromInteger i)

instance Serialise Exp.DataJet
instance Serialise Exp.SingJet
instance Serialise Match

-- The DumpNode is the version of Node which goes into va
--
-- Keep in sync with Types.Node
data DumpNode
  = DNEss
  | DNKay
  | DNEnh Int -- Always >= 1
  | DNDub
  | DNJut Hash
  | DNMatch Match !Natural ![DumpNode]

  | DNInt Integer
  | DNLis [DumpVal]
  | DNBol Bool

  | DNBox Hash
 deriving (Eq, Ord, Generic)

instance Serialise DumpNode

data DumpJet = DumpJet {
  djArgs :: Int,
  djName :: DumpVal,
  djBody :: DumpVal
  }
  deriving (Eq, Ord, Generic)

instance Serialise DumpJet

data DumpFun = DumpFun {
  dfNeed :: Int,
  dfHead :: DumpNode,
  dfArgs :: [DumpVal]
  }
  deriving (Eq, Ord, Generic)

instance Serialise DumpFun

-- A value
data DumpVal
  = DVUni
  | DVCon DumpVal DumpVal
  | DVLef DumpVal
  | DVRit DumpVal
  | DVNat Natural
  | DVInt Integer
  | DVBol Bool
  | DVLis [DumpVal]
  | DVBox Hash
  | DVFun Hash
  deriving (Eq, Ord, Generic)

instance Serialise DumpVal

-- Don't know the incantation here and don't want to bother Ben.
doHash :: ByteString -> Hash
doHash = Hash . SHA256.hash

-- When dumping, we also need to walk the tree.

toDumpVal :: Val -> PierIO DumpVal
toDumpVal VUni       = pure $ DVUni
toDumpVal (VCon l r) = DVCon <$> toDumpVal l <*> toDumpVal r
toDumpVal (VLef l)   = DVLef <$> toDumpVal l
toDumpVal (VRit r)   = DVRit <$> toDumpVal r
toDumpVal (VNat nat) = pure $ DVNat nat
toDumpVal (VInt i)   = pure $ DVInt i
toDumpVal (VBol b)   = pure $ DVBol b
toDumpVal (VLis xs)  = DVLis <$> mapM toDumpVal xs
toDumpVal (VBox b)   = DVBox <$> writeVal b

toDumpVal (VFun f)   = do
  bs <- (toStrict . serialise) <$> toDumpFun f
  DVFun <$> writeHashFile bs

toDumpFun :: Fun -> PierIO DumpFun
toDumpFun Fun{..} = do
  let dfNeed = fNeed
  dfHead <- toDumpNode fHead
  dfArgs <- mapM toDumpVal (toList fArgs)
  pure $ DumpFun{..}

toDumpJet :: Jet -> PierIO DumpJet
toDumpJet (Jet djArgs inName inBody _ _ _) = do
  djName <- toDumpVal inName
  djBody <- toDumpVal inBody
  pure $ DumpJet{..}

toDumpNode :: Node -> PierIO DumpNode
toDumpNode Ess                     = pure $ DNEss
toDumpNode Kay                     = pure $ DNKay
toDumpNode (Enh x)                 = pure $ DNEnh x
toDumpNode Dub                     = pure $ DNDub
toDumpNode (Jut jet)               = DNJut <$> writeJet jet

toDumpNode (M match natural nodes) = do
  dumpedNodes <- mapM toDumpNode nodes
  pure $ DNMatch match natural dumpedNodes

toDumpNode (Int i)                 = pure $ DNInt i
toDumpNode (Lis v)                 = DNLis <$> mapM toDumpVal v
toDumpNode (Bol b)                 = pure $ DNBol b

toDumpNode (Box v)                 = DNBox <$> writeVal v

-- Step one: we

writeHashFile :: ByteString -> PierIO Hash
writeHashFile bs = do
  let rawHash = doHash bs
  let name = toText $ fromBytes $ unHash rawHash

  hashDir <- ask
  let file = hashDir </> (unpack name)
  exists <- liftIO $ doesFileExist $ file
  unless exists $ do
    liftIO $ putStrLn $ "Writing " ++ name
    liftIO $ writeFile file bs

  pure rawHash

readHashFile :: Hash -> PierIO ByteString
readHashFile rawHash = do
  let name = toText $ fromBytes $ unHash rawHash
  hashDir <- ask
  let file = hashDir </> (unpack name)
  liftIO $ readFile file

-- -----------------------------------------------------------------------

writeNode :: Node -> PierIO Hash
writeNode node = do
  dnode <- toDumpNode node
  let bs = toStrict $ serialise dnode
  writeHashFile bs

writeVal :: Val -> PierIO Hash
writeVal val = do
  bs <- (toStrict . serialise) <$> toDumpVal val
  writeHashFile bs

writeJet :: Jet -> PierIO Hash
writeJet jet = do
  bs <- (toStrict . serialise) <$> toDumpJet jet
  writeHashFile bs


writeHashTo :: String -> Hash -> PierIO ()
writeHashTo name (Hash bs) = do
  hashDir <- ask
  liftIO $ writeFile (hashDir </> name) bs

-- -----------------------------------------------------------------------

fromDumpVal :: DumpVal -> PierIO Val
fromDumpVal DVUni       = pure VUni
fromDumpVal (DVCon l r) = VCon <$> fromDumpVal l <*> fromDumpVal r
fromDumpVal (DVLef l)   = VLef <$> fromDumpVal l
fromDumpVal (DVRit r)   = VRit <$> fromDumpVal r
fromDumpVal (DVNat n)   = pure $ VNat n
fromDumpVal (DVInt i)   = pure $ VInt i
fromDumpVal (DVBol b)   = pure $ VBol b
fromDumpVal (DVLis xs)  = VLis <$> mapM fromDumpVal xs
fromDumpVal (DVBox h)   = VBox <$> readVal h
fromDumpVal (DVFun h)   = VFun <$> readFun h

fromDumpFun :: DumpFun -> PierIO Fun
fromDumpFun DumpFun{..} = do
  let fNeed = dfNeed
  fHead <- fromDumpNode dfHead
  -- let fNeed = nodeArity fHead
  fArgs <- smallArrayFromList <$> mapM fromDumpVal dfArgs
  pure $ Fun{..}

fromDumpJet :: DumpJet -> PierIO Jet
fromDumpJet DumpJet{..} = do
  name <- fromDumpVal djName
  body <- fromDumpVal djBody
  opt <- liftIO $ Opt.compile djArgs name body
  pure $ Opt.optToFast opt

fromDumpNode :: DumpNode -> PierIO Node
fromDumpNode DNEss         = pure $ Ess
fromDumpNode DNKay         = pure $ Kay
fromDumpNode (DNEnh x)     = pure $ Enh x
fromDumpNode DNDub         = pure $ Dub
fromDumpNode (DNJut h)     = Jut <$> readJet h
fromDumpNode (DNInt i)     = pure $ Int i
fromDumpNode (DNLis v)     = Lis <$> mapM fromDumpVal v
fromDumpNode (DNBol b)     = pure $ Bol b

fromDumpNode (DNBox h)     = Box <$> readVal h

fromDumpNode (DNMatch m n nodes) = do
  decodedNodes <- mapM fromDumpNode nodes
  pure $ M m n decodedNodes

---

readVal :: Hash -> PierIO Val
readVal h = do
  dv <- (deserialise . fromStrict) <$> readHashFile h
  fromDumpVal dv

readFun :: Hash -> PierIO Fun
readFun h = do
  df <- (deserialise . fromStrict) <$> readHashFile h
  fromDumpFun df

readJet :: Hash -> PierIO Jet
readJet h = do
  dv <- (deserialise . fromStrict) <$> readHashFile h
  fromDumpJet dv


readHashFrom :: String -> PierIO Hash
readHashFrom name = do
  hashDir <- ask
  h <- liftIO $ readFile (hashDir </> name)
  pure (Hash h)

---

persistToPier :: FilePath -> Val -> IO Hash
persistToPier pier head = flip runReaderT pier $ do
  liftIO $ createDirectoryIfMissing False pier

  headHash <- writeVal head
  writeHashTo "HEAD" headHash
  pure headHash

loadFromPier :: FilePath -> IO Val
loadFromPier pier = flip runReaderT pier $ do
  headHash <- readHashFrom "HEAD"
  readVal headHash
