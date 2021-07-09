{-|
  Khan: Socket File
-}

module Urbit.Vere.Khan.SocketFile
  ( KhanSocket(..)
  , writeSocketFile
  , removeSocketFile
  )
where

import Urbit.Prelude

import System.Directory (doesFileExist, removeFile)
import Urbit.Arvo       (Port(unPort))


-- Types -----------------------------------------------------------------------

data KhanSocket = KhanSocket
  { pHttps :: Maybe Port
  , pHttp  :: Port
  , pLoop  :: Port
  }
 deriving (Eq, Ord, Show)


-- Creating and Deleting `.http.ports` files. ----------------------------------

socketFileText :: KhanSocket -> Text
socketFileText KhanSocket {..} = unlines $ catMaybes
  [ pHttps <&> \p -> (tshow p <> " secure public")
  , Just (tshow (unPort pHttp) <> " insecure public")
  , Just (tshow (unPort pLoop) <> " insecure loopback")
  ]

removeSocketFile :: FilePath -> IO ()
removeSocketFile pax = do
  doesFileExist pax >>= \case
    True  -> removeFile pax
    False -> pure ()

writeSocketFile :: FilePath -> KhanSocket -> IO ()
writeSocketFile f = writeFile f . encodeUtf8 . socketFileText
