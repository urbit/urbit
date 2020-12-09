module Urbit.EventLog.Event
  ( buildLogEvent
  , parseLogEvent
  )
  where

import ClassyPrelude
import Data.Serialize
import Urbit.Noun

buildLogEvent :: Mug -> Noun -> ByteString
buildLogEvent mug noun = (runPut $ putWord32le mug) ++ (jamBS noun)

parseLogEvent :: MonadIO m => ByteString -> m (Mug, Noun)
parseLogEvent bs = do
  let (prefix, suffix) = splitAt 4 bs
  let mug = case runGet getWord32le prefix of
        Left _ -> error "Impossible misread of word32 in parseLogEvent"
        Right x -> x
  n <- cueBSExn suffix
  pure (mug, n)
