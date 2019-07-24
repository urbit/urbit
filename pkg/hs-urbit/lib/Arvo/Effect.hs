module Arvo.Effect where

import Urbit.Time
import UrbitPrelude

import Arvo.Common (Header, HttpEvent, HttpServerConf, Lane, Method, Mime, Turf)
import Arvo.Common (Routed(..), disorg, reorg)


-- Newt Effects -- Todo What are these? ----------------------------------------

{-
    %turf -- TODO
    %send -- TODO
-}
data NewtEf
    = NewtEfTurf (Atom, ()) [Turf]
    | NewtEfSend (Atom, ()) Lane Bytes
  deriving (Eq, Ord, Show)

deriveNoun ''NewtEf


-- HTTP Client Effects ---------------------------------------------------------

data HttpClientReq = HttpClientReq
    { method     :: Method
    , url        :: Cord
    , headerList :: [Header]
    , body       :: Maybe Octs
    }
  deriving (Eq, Ord, Show)

{-
    %request        -- TODO
    %cancel-request -- TODO
-}
data HttpClientEf
    = HCERequest       (Atom, ()) Word HttpClientReq
    | HCECancelRequest Path       Word
  deriving (Eq, Ord, Show)

deriveNoun ''HttpClientReq
deriveNoun ''HttpClientEf


-- HTTP Server Effects ---------------------------------------------------------

{-
    %set-config -- Update HTTP server configuration.
    %response   -- Respond to an active HTTP request.
-}
data HttpServerEf
    = HSESetConfig (Atom, ())                   HttpServerConf
    | HSEResponse  (Atom, Decimal, Decimal, ()) HttpEvent
  deriving (Eq, Ord, Show)

deriveNoun ''HttpServerEf


-- File System Effects ---------------------------------------------------------

{-
    %hill -- TODO
    %dirk -- mark mount dirty
    %ergo -- TODO
    %ogre -- TODO
-}
data SyncEf
    = SyncEfHill ()  [Term]
    | SyncEfDirk Path Term
    | SyncEfErgo Path Term [(Path, Maybe Mime)]
    | SyncEfOgre Path Term
  deriving (Eq, Ord, Show)

deriveNoun ''SyncEf


-- UDP Effects -----------------------------------------------------------------

{-
    %init -- TODO
    %west -- TODO
    %woot -- TODO
-}
data AmesEf
    = AmesEfInit Path ()
    | AmesEfWest Path Ship Path Noun
    | AmesEfWoot Path Ship (Maybe (Maybe (Term, [Tank])))
  deriving (Eq, Ord, Show)

deriveNoun ''AmesEf


-- Timer Effects ---------------------------------------------------------------

{-
    %doze -- Set or clear timer.
    %void -- Nasty hack to make the parser not treat this as a record.
-}
data BehnEf
    = BehnEfDoze (Atom, ()) (Maybe Wen)
    | BehnEfVoid Void
  deriving (Eq, Ord, Show)

deriveNoun ''BehnEf


-- Terminal Effects ------------------------------------------------------------

{-
    %bel -- TODO
    %clr -- TODO
    %hop -- TODO
    %lin -- TODO
    %mor -- TODO
    %sag -- TODO
    %sav -- TODO
    %url -- TODO
-}
data Blit
    = Bel
    | Clr
    | Hop Word64
    | Lin [Char]
    | Mor
    | Sag Path Noun
    | Sav Path Atom
    | Url Cord
  deriving (Eq, Ord, Show)

{-
    %bbye -- TODO
    %blip -- TODO
    %init -- TODO
    %logo -- Shutdown
    %mass -- Measure memory usage (unused)
    %send -- TODO
-}
data TermEf
    = TermEfBbye Path ()
    | TermEfBlit Path [Blit]
    | TermEfInit (Decimal, ()) ()
    | TermEfLogo Path ()
    | TermEfMass Path Noun -- Irrelevant
    | TermEfSend Path Lane Bytes
  deriving (Eq, Ord, Show)

deriveNoun ''Blit
deriveNoun ''TermEf


-- IO-Driver Routing -----------------------------------------------------------

data VaneEf
    = EfNewt       NewtEf
    | EfHttpClient HttpClientEf
    | EfHttpServer HttpServerEf
    | EfBehn       BehnEf
    | EfAmes       AmesEf
    | EfTerm       TermEf
    | EfClay       SyncEf
    | EfSync       SyncEf
    | EfBoat       SyncEf
  deriving (Eq, Ord, Show)

deriveNoun ''VaneEf


-- Top-Level Effect Type -------------------------------------------------------

data Effect
    = VaneEf VaneEf
    | VegaEf (Cord, Cord, EvilPath)
    | ExitEf (Cord, Cord, EvilPath)
  deriving (Eq, Ord, Show)

instance ToNoun Effect where
  toNoun = \case
    VaneEf v -> disorg (toNoun v)
    ExitEf p -> toNoun (p, (Cord "exit", ()))
    VegaEf p -> toNoun (p, (Cord "vega", ()))

instance FromNoun Effect where
  parseNoun n = do
    routed@(Routed (f, s, p) tv) <- parseNoun n
    case tv of
      ( "exit", A 0 ) -> pure (ExitEf (f, s, p))
      ( "exit", _   ) -> fail "Exit effect expects nil value"
      ( "vega", A 0 ) -> pure (VegaEf (f, s, p))
      ( "vega", _   ) -> fail "Vega effect expects nil value"
      ( _,      _   ) -> VaneEf <$> parseNoun (reorg routed)
