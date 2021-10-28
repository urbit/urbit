{-|
  `into` is a one-shot executable that sends a single command to an Urbit's
  %khan vane, and produces a single response. Commonly this response will be a
  ticket for a job to be completed out of band; this ticket can be polled for
  status or results.

  Some interesting commands supported by `into`:

  - `code`: produce the ship's current `+code`.
  - `reset`: reset the ship's `+code`.
-}
module Into.Main (main) where

import Control.Monad
import Data.Binary.Builder
import Data.Binary.Strict.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text
import GHC.Natural
import Network.Socket
import Network.Socket.ByteString
import Numeric (showHex)
import System.Environment
import System.FilePath.Posix
import System.Posix
import Text.Printf
import Urbit.Noun
import Urbit.Ob

packNoun :: ToNoun a => a -> BS.ByteString
packNoun jar =
  let pac = jamBS (toNoun jar) in
  BL.toStrict $ toLazyByteString $
    mconcat [putWord64le $ fromIntegral $ BS.length pac,
             fromByteString pac]

codeCmd :: BS.ByteString
codeCmd = packNoun (Cord $ Data.Text.pack "cod", False)

extractNoun :: FromNoun a => BS.ByteString -> IO a
extractNoun = cueBSExn >=> fromNounExn

main :: IO ()
main = withSocketsDo $ do
  print $ (BS.foldr (\b -> (<>) (Data.Text.pack $ printf "%02x" b)) Data.Text.empty) codeCmd
  args <- getArgs
  let paf = args !! 0
  changeWorkingDirectory paf
  sock <- socket AF_UNIX Stream 0
  connect sock (SockAddrUnix $ ".urb" </> "khan.sock")
  send sock codeCmd
  lenBS <- recv sock 8
  let (len', _) = runGet getWord64le lenBS
  case len' of
    Left fal -> error fal
    Right len -> do
      wad <- recv sock $ fromIntegral len
      jar <- (extractNoun wad) :: IO (Cord, Maybe Natural)
      let (_,cod') = jar
      case cod' of
        Nothing -> error "no code"
        Just cod -> putStrLn . unpack . renderPatp . patp $ cod
  close sock
