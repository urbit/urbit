module Urbit.Vere.Archive
  ( inport
  , export
  )
where

import Urbit.Prelude

import Urbit.Arvo.Event
import Urbit.Vere.Pier.Types

import qualified Network.HTTP.Types.Method as H

inport :: HasLogFunc e => FilePath -> RIO e [Ev]
inport _ = do
  -- TODO
  pure [EvBlip $ BlipEvHttpServer $ HttpServerEvRequestLocal
    (0, 0, 0, ())
    (HttpServerReq
      { hsrSecure = False
      , hsrAddress = AAmes (Ship 0)
      , hsrRequest = HttpRequest
        { reqMeth = H.GET
        , reqUrl  = "/"
        , reqHead = []
        , reqBody = Just "my awesome file"
        }
      })]

export :: HasLogFunc e => FilePath -> ScryFunc -> RIO e ()
export = undefined
