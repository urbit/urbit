module Arvo.Effect where

import Urbit.Time
import UrbitPrelude

import Arvo.Common (Header, HttpEvent, HttpServerConf, Lane, Method, Mime, Turf)
import Arvo.Common (ReOrg(..), reorgThroughNoun)


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
    = VENewt       NewtEf
    | VEHttpClient HttpClientEf
    | VEHttpServer HttpServerEf
    | VEBehn       BehnEf
    | VEAmes       AmesEf
    | VETerm       TermEf
    | VEClay       SyncEf
    | VESync       SyncEf
    | VEBoat       SyncEf
  deriving (Eq, Ord, Show)

deriveNoun ''VaneEf


-- Top-Level Ef Type -----------------------------------------------------------

data Ef
    = EfVane VaneEf
    | EfVega Cord EvilPath -- second path component, rest of path
    | EfExit Cord EvilPath -- second path component, rest of path
  deriving (Eq, Ord, Show)

instance ToNoun Ef where
  toNoun = \case
    EfVane v   -> toNoun $ reorgThroughNoun ("", v)
    EfExit s p -> toNoun $ ReOrg "" s "exit" p (A 0)
    EfVega s p -> toNoun $ ReOrg "" s "vega" p (A 0)

instance FromNoun Ef where
  parseNoun = parseNoun >=> \case
    ReOrg "" s "exit" p (A 0) -> pure (EfExit s p)
    ReOrg "" s "exit" p _     -> fail "%exit effect expects nil value"
    ReOrg "" s "vega" p (A 0) -> pure (EfVega s p)
    ReOrg "" s "vega" p _     -> fail "%vega effect expects nil value"
    ReOrg "" s tag    p val   -> EfVane <$> parseNoun (toNoun (s, tag, p, val))
    ReOrg _  _ _      _ _     -> fail "Non-empty first path-element"
