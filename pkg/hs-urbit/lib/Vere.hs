module Vere where

import ClassyPrelude
import Data.Void
import qualified Vere.Http.Server as Server
import qualified Vere.Http.Client as Client

-- +vere -----------------------------------------------------------------------

data WTFIsThis
  = WTFIsThis (Maybe Varience) TheActualFuckingThing

data Varience = Gold | Iron | Lead

data TheActualFuckingThing
  = HttpServer Server.Eff
  | HttpClient Client.Eff
  | Behn Void
  | Clay Void
  | Boat Void
  | Sync Void
  | Newt Void
  | Ames Void
  | Init Void
  | Term Void

