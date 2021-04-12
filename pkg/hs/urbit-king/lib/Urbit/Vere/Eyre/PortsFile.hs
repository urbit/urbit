{-|
  Eyre: Http Server Driver
-}

module Urbit.Vere.Eyre.PortsFile
  ( Ports(..)
  , writePortsFile
  , removePortsFile
  )
where

import Urbit.Prelude

import System.Directory (doesFileExist, removeFile)
import Urbit.Arvo       (Port(unPort))


-- Types -----------------------------------------------------------------------

data Ports = Ports
  { pHttps :: Maybe Port
  , pHttp  :: Port
  , pLoop  :: Port
  }
 deriving (Eq, Ord, Show)


-- Creating and Deleting `.http.ports` files. ----------------------------------

portsFileText :: Ports -> Text
portsFileText Ports {..} = unlines $ catMaybes
  [ pHttps <&> \p -> (tshow p <> " secure public")
  , Just (tshow (unPort pHttp) <> " insecure public")
  , Just (tshow (unPort pLoop) <> " insecure loopback")
  ]

removePortsFile :: FilePath -> IO ()
removePortsFile pax = do
  doesFileExist pax >>= \case
    True  -> removeFile pax
    False -> pure ()

writePortsFile :: FilePath -> Ports -> IO ()
writePortsFile f = writeFile f . encodeUtf8 . portsFileText
