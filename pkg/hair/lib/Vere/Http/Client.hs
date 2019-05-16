-- +http-client ----------------------------------------------------------------

module Vere.Http.Client where

import ClassyPrelude
import Vere.Http

-- | An http client effect is either requesting outbound, or canceling an old
-- outbound connection.
data Eff
  = Request Word Request
  | CancelRequest Word
