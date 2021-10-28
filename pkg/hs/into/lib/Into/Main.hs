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

import            Control.Monad       ((>=>))
import            Data.Binary.Builder (fromByteString, putWord64le,
                                       toLazyByteString)
import            Data.Binary.Strict.Get (getWord64le, runGet)
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.Text as T
import            GHC.Natural         (Natural)
import            Network.Socket      (Family(AF_UNIX), SockAddr(SockAddrUnix),
                                       SocketType(Stream), close, connect,
                                       socket, withSocketsDo)
import            Network.Socket.ByteString (send, recv)
import            System.Environment  (getArgs)
import            System.FilePath.Posix ((</>))
import            System.Posix        (changeWorkingDirectory)
import            Text.Printf         (printf)
import            Urbit.Noun          (Cord(..), FromNoun, ToNoun, cueBSExn,
                                       fromNounExn, jamBS, toNoun)
import            Urbit.Ob            (patp, renderPatp)

packNoun :: ToNoun a => a -> B.ByteString
packNoun jar =
  let pac = jamBS (toNoun jar) in
  BL.toStrict $ toLazyByteString $
    mconcat [putWord64le $ fromIntegral $ B.length pac,
             fromByteString pac]

codeCmd :: B.ByteString
codeCmd = packNoun (Cord "cod", False)

extractNoun :: FromNoun a => B.ByteString -> IO a
extractNoun = cueBSExn >=> fromNounExn

hexDumpBS :: B.ByteString -> T.Text
hexDumpBS = B.foldr (\b -> (<>) (T.pack $ printf "%02x" b)) ""

main :: IO ()
main = withSocketsDo $ do
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
        Just cod -> putStrLn . T.unpack . renderPatp . patp $ cod
  close sock
