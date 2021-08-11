module Urbit.FxLog where

import Urbit.Prelude

import Data.Text.IO
import System.IO hiding (hFlush, hPutStrLn)
import System.IO.Unsafe
import System.Random

import Urbit.Arvo.Effect
import Urbit.Noun.ByteString
import Urbit.Vere.Serf.IPC.Types

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
