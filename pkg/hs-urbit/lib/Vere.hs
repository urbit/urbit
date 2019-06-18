module Vere where

import ClassyPrelude
import Data.Void
import Data.Noun
import qualified Vere.Http.Server as Server
import qualified Vere.Http.Client as Client

-- +vere -----------------------------------------------------------------------

data WTFIsThis
  = WTFIsThis (Maybe Varience) Eff

data Varience = Gold | Iron | Lead

data Eff
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


type Perform = Eff -> IO ()

data IODriver = IODriver
  { bornEvent   :: IO Noun
  , startDriver :: (Noun -> STM ()) -> IO (Async (), Perform)
  }
