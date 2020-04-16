module Urbit.UrukSerialize where

import ClassyPrelude hiding (toList, writeFile)

import Codec.Serialise
import Data.ByteString     hiding (putStrLn, unpack)
import Data.Foldable
import Data.HexString
import System.Directory
import System.FilePath
import Urbit.UrukRTS.Types

import qualified Crypto.Hash.SHA256 as SHA256



newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, Generic)

instance Serialise Hash

-- The DumpNode is the version of Node which goes into va
--
-- Keep in sync with Types.Node
data DumpNode
  = DNJay Int -- Always >= 1
  | DNKay
  | DNEss
  | DNDee
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
  | DVBox DumpVal
  | DVFun Hash
  deriving (Eq, Ord, Generic)

instance Serialise DumpVal

-- Don't know the incantation here and don't want to bother Ben.
doHash :: ByteString -> Hash
doHash = Hash . SHA256.hash

-- When dumping, we also need to walk the tree.

toDumpVal :: Val -> IO DumpVal
toDumpVal VUni       = pure $ DVUni
toDumpVal (VCon l r) = DVCon <$> toDumpVal l <*> toDumpVal r
toDumpVal (VLef l)   = DVLef <$> toDumpVal l
toDumpVal (VRit r)   = DVRit <$> toDumpVal r
toDumpVal (VNat nat) = pure $ DVNat nat
toDumpVal (VInt i)   = pure $ DVInt i
toDumpVal (VBol b)   = pure $ DVBol b
toDumpVal (VLis xs)  = DVLis <$> mapM toDumpVal xs

-- TODO: Boxed values should be forced into a new toplevel
toDumpVal (VBox b)   = DVBox <$> toDumpVal b

toDumpVal (VFun f)   = do
  bs <- (toStrict . serialise) <$> toDumpFun f
  DVFun <$> writeHashFile bs

toDumpFun :: Fun -> IO DumpFun
toDumpFun Fun{..} = do
  dfHead <- toDumpNode fHead
  dfArgs <- mapM toDumpVal (toList fArgs)
  pure $ DumpFun{..}

toDumpJet :: Int -> Val -> Val -> IO DumpJet
toDumpJet djArgs inName inBody = do
  djName <- toDumpVal inName
  djBody <- toDumpVal inBody
  pure $ DumpJet{..}


toDumpNode :: Node -> IO DumpNode
toDumpNode (Jay x) = pure $ DNJay x
toDumpNode Kay = pure $ DNKay
toDumpNode Ess = pure $ DNEss
toDumpNode Dee = pure $ DNDee
toDumpNode (Jut (Jet args name body _ _)) = do
  bs <- (toStrict . serialise) <$> toDumpJet args name body
  hash <- writeHashFile bs
  pure $ DNJut hash
toDumpNode (Eye x) = pure $ DNEye x
toDumpNode (Bee x) = pure $ DNBee x
toDumpNode (Sea x) = pure $ DNSea x
toDumpNode (Sen x) = pure $ DNSen x
toDumpNode Seq = pure $ DNSeq
toDumpNode Fix = pure $ DNFix
toDumpNode (Nat n) = pure $ DNNat n
toDumpNode (Int i) = pure $ DNInt i
toDumpNode (Lis v) = DNLis <$> mapM toDumpVal v -- TODO: Figure out recursion here.
toDumpNode (Bol b) = pure $ DNBol b
toDumpNode Iff = pure $ DNIff
toDumpNode Pak = pure $ DNPak
toDumpNode Zer = pure $ DNZer
toDumpNode Eql = pure $ DNEql
toDumpNode Add = pure $ DNAdd
toDumpNode Inc = pure $ DNInc
toDumpNode Dec = pure $ DNDec
toDumpNode Fec = pure $ DNFec
toDumpNode Mul = pure $ DNMul
toDumpNode Sub = pure $ DNSub
toDumpNode Ded = pure $ DNDed
toDumpNode Uni = pure $ DNUni
toDumpNode Lef = pure $ DNLef
toDumpNode Rit = pure $ DNRit
toDumpNode Cas = pure $ DNCas
toDumpNode Let = pure $ DNLet
toDumpNode Con = pure $ DNCon
toDumpNode Car = pure $ DNCar
toDumpNode Cdr = pure $ DNCdr

toDumpNode Lsh = pure $ DNLsh
toDumpNode Lth = pure $ DNLth
toDumpNode Fub = pure $ DNFub
toDumpNode Not = pure $ DNNot
toDumpNode Xor = pure $ DNXor
toDumpNode Div = pure $ DNDiv
toDumpNode Tra = pure $ DNTra
toDumpNode Mod = pure $ DNMod
toDumpNode Rap = pure $ DNRap
toDumpNode Zing = pure $ DNZing

toDumpNode IntPositive = pure $ DNIntPositive
toDumpNode IntNegative = pure $ DNIntNegative

toDumpNode IntAbs = pure $ DNIntAbs
toDumpNode IntAdd = pure $ DNIntAdd
toDumpNode IntDiv = pure $ DNIntDiv
toDumpNode IntIsZer = pure $ DNIntIsZer
toDumpNode IntIsNeg = pure $ DNIntIsNeg
toDumpNode IntIsPos = pure $ DNIntIsPos
toDumpNode IntLth = pure $ DNIntLth
toDumpNode IntMul = pure $ DNIntMul
toDumpNode IntNegate = pure $ DNIntNegate
toDumpNode IntSub = pure $ DNIntSub

toDumpNode MkBox = pure $ DNMkBox
toDumpNode (Box v) = DNBox <$> writeVal v
toDumpNode Unbox = pure $ DNUnbox

toDumpNode LCon = pure $ DNLCon
toDumpNode LNil = pure $ DNLNil
toDumpNode Gulf = pure $ DNGulf
toDumpNode Snag = pure $ DNSnag
toDumpNode Turn = pure $ DNTurn
toDumpNode Weld = pure $ DNWeld

toDumpNode AddAssoc = pure $ DNAddAssoc
toDumpNode FindAssoc = pure $ DNFindAssoc

-- Step one: we

writeHashFile :: ByteString -> IO Hash
writeHashFile bs = do
  let rawHash = doHash bs
  let name = toText $ fromBytes $ unHash rawHash

  let dir = "/Users/erg/hashData/"

  let file = dir </> (unpack name)
  exists <- doesFileExist $ file
  unless exists $ do
    putStrLn $ "Writing " ++ name
    writeFile file bs

  pure rawHash


writeNode :: Node -> IO Hash
writeNode node = do
  dnode <- toDumpNode node
  let bs = toStrict $ serialise dnode
  writeHashFile bs


writeVal :: Val -> IO Hash
writeVal val = do
  dval <- toDumpVal val
  let bs = toStrict $ serialise dval
  writeHashFile bs
