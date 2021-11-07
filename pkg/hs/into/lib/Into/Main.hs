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

import            Control.Monad
import            Data.Binary.Builder
import            Data.Binary.Strict.Get
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.Map as M
import qualified  Data.Text as T
import            GHC.Natural
import            Network.Socket
import            Network.Socket.ByteString
import            System.Environment
import            System.FilePath.Posix
import            System.Posix
-- import            Text.Printf
import            Urbit.Noun
import            Urbit.Ob

usage :: IO ()
usage = do
  putStrLn "usage: into <pier> <cmd>"
  putStrLn "cmds:"
  mapM_ (putStrLn . ("  " ++)) $ M.keys cmds

khanVersion :: Natural
khanVersion = 0

packNoun :: ToNoun a => a -> B.ByteString
packNoun jar =
  let pac = jamBS (toNoun jar) in
  BL.toStrict $ toLazyByteString $
    mconcat [putWord64le $ fromIntegral $ B.length pac,
             fromByteString pac]

buildCmd :: ToNoun a => a -> B.ByteString
buildCmd com = packNoun (khanVersion, com)

codeCmd :: B.ByteString
codeCmd = buildCmd (Cord "cod", False)

codeResponse :: Noun -> IO ()
codeResponse jar = do
  (res, cod') <- fromNounExn jar :: IO (Cord, Maybe Natural)
  if res /= (Cord "cod") then error $ "bad res: " ++ show res
  else case cod' of
    Nothing -> error "no code"
    Just cod -> putStrLn . T.unpack . renderPatp . patp $ cod

codeResetCmd :: B.ByteString
codeResetCmd = buildCmd (Cord "cod", True)

ackResponse :: String -> Noun -> IO ()
ackResponse msg jar = do
  zac <- fromNounExn jar
  if zac /= (Cord "ack") then error $ "bad jar: " ++ show jar
  else putStrLn msg

massCmd :: B.ByteString
massCmd = buildCmd (Cord "mas", 0 :: Natural)

--            cmd,    wire cmd,     on response
cmds :: M.Map String (B.ByteString, Noun -> IO ())
cmds = M.fromList
  [ ("code",  (codeCmd,       codeResponse))
  , ("reset", (codeResetCmd,  ackResponse "+code reset."))
  , ("mass",  (massCmd,       ackResponse "|mass TODO"))
  ]
cmdFind :: String -> IO (B.ByteString, Noun -> IO ())
cmdFind cmdName = case M.lookup cmdName cmds of
  Nothing -> do { usage; error "cmd not found" }
  Just cmd -> return cmd

extractNoun :: FromNoun a => B.ByteString -> IO a
extractNoun = cueBSExn >=> fromNounExn

-- hexDumpBS :: B.ByteString -> T.Text
-- hexDumpBS = B.foldr (\b -> (<>) (T.pack $ printf "%02x" b)) ""

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  if 2 /= length args then
    do { usage; error "wrong number of arguments" }
  else do
    let [paf, cmdName] = args
    (cmd, act) <- cmdFind cmdName
    changeWorkingDirectory paf
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix $ ".urb" </> "khan.sock")
    send sock cmd
    lenBS <- recv sock 8
    let (len', _) = runGet getWord64le lenBS
    case len' of
      Left fal -> error fal
      Right len -> do
        wad <- recv sock $ fromIntegral len
        jar <- extractNoun wad
        act jar
    close sock
