module Urbit.FxLog where

import Urbit.Prelude hiding (readFile, tail, init)
import Prelude (read, tail, init)

import Data.List.Split (splitOn)
import Data.Text.IO hiding (readFile)
import Data.Maybe (fromJust)
import System.IO hiding (hFlush, hPutStrLn, hGetLine, hIsEOF)
import System.IO.Unsafe
import System.Random

import Urbit.Arvo.Common
import Urbit.Arvo.Effect
import Urbit.Noun.ByteString
import Urbit.Vere.Serf.IPC.Types

import qualified Urbit.Ob as Ob

file :: Handle
file = unsafePerformIO $ do
  id :: Word16 <- randomIO
  openFile ("king-fx-" <> show id <.> "log") WriteMode

recordEf :: Lenient Ef -> IO ()
recordEf = \case
  GoodParse (EfVane (VENewt (NewtEfSend (a, ()) d (MkBytes b)))) -> do
    hPutStrLn file $ tshow (a, d, toBS b)
    hFlush file
  _ -> pure ()

recordPlea :: Plea -> IO ()
recordPlea = \case
  PWork w -> for_ (fx w) recordEf
   where
    fx = \case
      WDone _ _ f -> f
      WSwap _ _ _ f -> f
      WBail _ -> []
  _ -> pure ()


--------------------------------------------------------------------------------

line :: Handle -> IO (Maybe (Lenient Ef))
line h = hIsEOF h >>= \case
  True  -> pure Nothing
  False -> do
    l <- hGetLine h
    case l of
      "" -> pure Nothing
      l  -> pure $ Just $ parse $ unpack l

parse :: String -> Lenient Ef
parse str = convert (atm, dest, byt)
 where
  (begin, res1) = span (/= ',') $ tail str
  (middle, res2) = span (/= ',') $ tail res1
  end = tail res2
  atm = read begin
  dest = case middle of
    (stripPrefix "EachYes \"" -> Just x) ->
      EachYes $ Patp $ fromIntegral
              $ Ob.fromPatp $ either undefined id $ Ob.parsePatp $ pack x
    (stripPrefix "EachNo (AAIpv4 " -> Just x) ->
      let [hos, por] = splitOn " " $ init x
      in EachNo $ AAIpv4 (read hos) (read por)
  byt = read $ init end

convert (a, d, b) =
  GoodParse $ EfVane $ VENewt $ NewtEfSend (a, ()) d (MkBytes $ fromBS b)

{-loadFx :: IO [Lenient Ef]
loadFx = fmap parse <$> filter (/= "") <$> take 1000 <$> lines <$> readFile "sends.in"-}

nextFx :: IO [Lenient Ef]
nextFx = catMaybes <$> replicateM 100 (line han)
 where
  han = unsafePerformIO $ openFile "sends.in" ReadMode

addFx :: IO [Lenient Ef] -> Plea -> IO Plea
addFx fx = \case
  PWork (WDone a b f) -> PWork . WDone a b . (f ++) <$> fx
  PWork (WSwap a b c f) -> PWork . WSwap a b c . (f ++) <$> fx
  PWork (WBail a) -> pure $ PWork $ WBail a
  p -> pure $ p