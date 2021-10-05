{-|
  Khan: Socket File
-}

module Urbit.Vere.Khan.SocketFile
  ( removeSocketFile
  )
where

import Urbit.Prelude

import System.Directory (doesFileExist, removeFile)

removeSocketFile :: FilePath -> IO ()
removeSocketFile pax = do
  doesFileExist pax >>= \case
    True  -> removeFile pax
    False -> pure ()

