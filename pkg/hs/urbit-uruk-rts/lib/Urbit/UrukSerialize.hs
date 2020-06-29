module Urbit.UrukSerialize where

import ClassyPrelude hiding (readFile, toList, writeFile)

import Codec.Serialise
import Data.ByteString           hiding (putStrLn, unpack)
import Data.Foldable
import Data.HexString
import Data.Primitive.SmallArray
import System.Directory
import System.FilePath
import Urbit.UrukRTS.Types

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Urbit.UrukRTS.JetOptimize as Opt
import qualified Urbit.UrukRTS.OptToFast   as Opt


type PierIO = ReaderT FilePath IO

instance Serialise Hash

-- The DumpNode is the version of Node which goes into va
--
-- Keep in sync with Types.Node
data DumpNode
  = DNEss
  | DNKay
  | DNEnh Int -- Always >= 1
  | DNDub
  | DNJut Hash
  | DNEye Int
  | DNBee Int --  Always >=  1
  | DNSea Int --  Always >=  1
  | DNSen Int --  Always >=  1
  | DNSeq
  | DNFix
  | DNNat Nat
  | DNInt Integer
  | DNLis [DumpVal]
  | DNBol Bool
  | DNIff
  | DNPak
  | DNZer
  | DNEql
  | DNAdd
  | DNInc
  | DNDec
  | DNFec
  | DNMul
  | DNSub
  | DNDed
  | DNUni
  | DNLef
  | DNRit
  | DNCas
  | DNLet
  | DNCon
  | DNCar
  | DNCdr

  | DNLsh
  | DNLth
  | DNFub
  | DNNot
  | DNXor
  | DNDiv
  | DNTra
  | DNMod
  | DNRap
  | DNZing
  | DNNtot

  | DNIntPositive
  | DNIntNegative

  | DNIntAbs
  | DNIntAdd
  | DNIntDiv
  | DNIntIsZer
  | DNIntIsNeg
  | DNIntIsPos
  | DNIntLth
  | DNIntMul
  | DNIntNegate
  | DNIntSub

  | DNMkBox
  | DNBox Hash
  | DNUnbox

  | DNLCon
  | DNLNil
  | DNGulf
  | DNSnag
  | DNTurn
  | DNWeld

  | DNAddAssoc
  | DNFindAssoc
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
  | DVNat Nat
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
toDumpVal (VBox (BoxVal ref)) = readIORef ref >>= \case
  (BSaved h _)    -> pure $ DVBox h
  (BUnsaved v)    -> DVBox <$> writeVal v
  (BUnloaded h _) -> pure $ DVBox h
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
toDumpNode Ess                   = pure $ DNEss
toDumpNode Kay                   = pure $ DNKay
toDumpNode (Enh x)               = pure $ DNEnh x
toDumpNode Dub                   = pure $ DNDub
toDumpNode (Jut jet)             = DNJut <$> writeJet jet
toDumpNode (Eye x)               = pure $ DNEye x
toDumpNode (Bee x)               = pure $ DNBee x
toDumpNode (Sea x)               = pure $ DNSea x
toDumpNode (Sen x)               = pure $ DNSen x
toDumpNode Seq                   = pure $ DNSeq
toDumpNode Fix                   = pure $ DNFix
toDumpNode (Nat n)               = pure $ DNNat n
toDumpNode (Int i)               = pure $ DNInt i
toDumpNode (Lis v)               = DNLis <$> mapM toDumpVal v
toDumpNode (Bol b)               = pure $ DNBol b
toDumpNode Iff                   = pure $ DNIff
toDumpNode Pak                   = pure $ DNPak
toDumpNode Zer                   = pure $ DNZer
toDumpNode Eql                   = pure $ DNEql
toDumpNode Add                   = pure $ DNAdd
toDumpNode Inc                   = pure $ DNInc
toDumpNode Dec                   = pure $ DNDec
toDumpNode Fec                   = pure $ DNFec
toDumpNode Mul                   = pure $ DNMul
toDumpNode Sub                   = pure $ DNSub
toDumpNode Ded                   = pure $ DNDed
toDumpNode Uni                   = pure $ DNUni
toDumpNode Lef                   = pure $ DNLef
toDumpNode Rit                   = pure $ DNRit
toDumpNode Cas                   = pure $ DNCas
toDumpNode Let                   = pure $ DNLet
toDumpNode Con                   = pure $ DNCon
toDumpNode Car                   = pure $ DNCar
toDumpNode Cdr                   = pure $ DNCdr

toDumpNode Lsh                   = pure $ DNLsh
toDumpNode Lth                   = pure $ DNLth
toDumpNode Fub                   = pure $ DNFub
toDumpNode Not                   = pure $ DNNot
toDumpNode Xor                   = pure $ DNXor
toDumpNode Div                   = pure $ DNDiv
toDumpNode Tra                   = pure $ DNTra
toDumpNode Mod                   = pure $ DNMod
toDumpNode Rap                   = pure $ DNRap
toDumpNode Zing                  = pure $ DNZing
toDumpNode Ntot                  = pure $ DNNtot

toDumpNode IntPositive           = pure $ DNIntPositive
toDumpNode IntNegative           = pure $ DNIntNegative

toDumpNode IntAbs                = pure $ DNIntAbs
toDumpNode IntAdd                = pure $ DNIntAdd
toDumpNode IntDiv                = pure $ DNIntDiv
toDumpNode IntIsZer              = pure $ DNIntIsZer
toDumpNode IntIsNeg              = pure $ DNIntIsNeg
toDumpNode IntIsPos              = pure $ DNIntIsPos
toDumpNode IntLth                = pure $ DNIntLth
toDumpNode IntMul                = pure $ DNIntMul
toDumpNode IntNegate             = pure $ DNIntNegate
toDumpNode IntSub                = pure $ DNIntSub

toDumpNode MkBox                 = pure $ DNMkBox
toDumpNode (Box (BoxVal ref))    = readIORef ref >>= \case
  (BSaved h _) -> pure $ DNBox h
  (BUnsaved v) -> DNBox <$> writeVal v
  (BUnloaded h _) -> pure $ DNBox h
toDumpNode Unbox                 = pure $ DNUnbox

toDumpNode LCon                  = pure $ DNLCon
toDumpNode LNil                  = pure $ DNLNil
toDumpNode Gulf                  = pure $ DNGulf
toDumpNode Snag                  = pure $ DNSnag
toDumpNode Turn                  = pure $ DNTurn
toDumpNode Weld                  = pure $ DNWeld

toDumpNode AddAssoc              = pure $ DNAddAssoc
toDumpNode FindAssoc             = pure $ DNFindAssoc

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

fromDumpVal (DVBox h)   = do
  -- Each value is actually lazy loaded.
  pier <- ask
  let action = runReaderT (readVal h) pier
  liftIO $ VBox <$> packBoxVal (BUnloaded h action)

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
fromDumpNode (DNEye x)     = pure $ Eye x
fromDumpNode (DNBee x)     = pure $ Bee x
fromDumpNode (DNSea x)     = pure $ Sea x
fromDumpNode (DNSen x)     = pure $ Sen x
fromDumpNode DNSeq         = pure $ Seq
fromDumpNode DNFix         = pure $ Fix
fromDumpNode (DNNat n)     = pure $ Nat n
fromDumpNode (DNInt i)     = pure $ Int i
fromDumpNode (DNLis v)     = Lis <$> mapM fromDumpVal v
fromDumpNode (DNBol b)     = pure $ Bol b
fromDumpNode DNIff         = pure Iff
fromDumpNode DNPak         = pure Pak
fromDumpNode DNZer         = pure Zer
fromDumpNode DNEql         = pure Eql
fromDumpNode DNAdd         = pure Add
fromDumpNode DNInc         = pure Inc
fromDumpNode DNDec         = pure Dec
fromDumpNode DNFec         = pure Fec
fromDumpNode DNMul         = pure Mul
fromDumpNode DNSub         = pure Sub
fromDumpNode DNDed         = pure Ded
fromDumpNode DNUni         = pure Uni
fromDumpNode DNLef         = pure Lef
fromDumpNode DNRit         = pure Rit
fromDumpNode DNCas         = pure Cas
fromDumpNode DNLet         = pure Let
fromDumpNode DNCon         = pure Con
fromDumpNode DNCar         = pure Car
fromDumpNode DNCdr         = pure Cdr

fromDumpNode DNLsh         = pure Lsh
fromDumpNode DNLth         = pure Lth
fromDumpNode DNFub         = pure Fub
fromDumpNode DNNot         = pure Not
fromDumpNode DNXor         = pure Xor
fromDumpNode DNDiv         = pure Div
fromDumpNode DNTra         = pure Tra
fromDumpNode DNMod         = pure Mod
fromDumpNode DNRap         = pure Rap
fromDumpNode DNZing        = pure Zing
fromDumpNode DNNtot        = pure Ntot

fromDumpNode DNIntPositive = pure IntPositive
fromDumpNode DNIntNegative = pure IntNegative

fromDumpNode DNIntAbs      = pure IntAbs
fromDumpNode DNIntAdd      = pure IntAdd
fromDumpNode DNIntDiv      = pure IntDiv
fromDumpNode DNIntIsZer    = pure IntIsZer
fromDumpNode DNIntIsNeg    = pure IntIsNeg
fromDumpNode DNIntIsPos    = pure IntIsPos
fromDumpNode DNIntLth      = pure IntLth
fromDumpNode DNIntMul      = pure IntMul
fromDumpNode DNIntNegate   = pure IntNegate
fromDumpNode DNIntSub      = pure IntSub

fromDumpNode DNMkBox       = pure MkBox
fromDumpNode (DNBox h)     = do
  -- Each value is actually lazy loaded.
  pier <- ask
  let action = runReaderT (readVal h) pier
  liftIO $ Box <$> packBoxVal (BUnloaded h action)

fromDumpNode DNUnbox       = pure Unbox

fromDumpNode DNLCon        = pure LCon
fromDumpNode DNLNil        = pure LNil
fromDumpNode DNGulf        = pure Gulf
fromDumpNode DNSnag        = pure Snag
fromDumpNode DNTurn        = pure Turn
fromDumpNode DNWeld        = pure Weld

fromDumpNode DNAddAssoc    = pure AddAssoc
fromDumpNode DNFindAssoc   = pure FindAssoc


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
