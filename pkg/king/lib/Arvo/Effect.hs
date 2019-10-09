module Arvo.Effect where

import Urbit.Time
import UrbitPrelude

import Arvo.Common (KingId(..), ServId(..))
import Arvo.Common (Header, HttpEvent, HttpServerConf, Method, Mime)
import Arvo.Common (AmesDest, Turf)
import Arvo.Common (ReOrg(..), reorgThroughNoun)
import Arvo.Common (Desk)


-- Newt Effects ----------------------------------------------------------------

{-
    %turf -- Set which domain names we've bound.
    %send -- Send a UDP packet.
-}
data NewtEf
    = NewtEfTurf (Atom, ()) [Turf]
    | NewtEfSend (Atom, ()) AmesDest Bytes
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
    = HSESetConfig (ServId, ())         HttpServerConf
    | HSEResponse  (ServId, UD, UD, ()) HttpEvent
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
    = SyncEfHill ()  [Desk]
    | SyncEfDirk Path Desk
    | SyncEfErgo Path Desk [(Path, Maybe Mime)]
    | SyncEfOgre Path Desk
  deriving (Eq, Ord, Show)

deriveNoun ''SyncEf


-- UDP Effects -----------------------------------------------------------------

{-
    %init -- "I don't think that's something that can happen"
    %west -- "Those also shouldn't happen"
    %woot -- "Those also shouldn't happen"
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
    = BehnEfDoze (KingId, ()) (Maybe Wen)
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
    = Bel ()
    | Clr ()
    | Hop Word64
    | Lin [Char]
    | Mor ()
    | Sag Path Noun
    | Sav Path Atom
    | Url Cord
  deriving (Eq, Ord)

-- Manual instance to not save the noun/atom in Sag/Sav, because these can be
-- megabytes and makes king hang.
instance Show Blit where
  show (Bel ())     = "Bel ()"
  show (Clr ())     = "Clr ()"
  show (Hop x)      = "Hop " ++ (show x)
  show (Lin c)      = "Lin " ++ (show c)
  show (Mor ())     = "Mor ()"
  show (Sag path _) = "Sag " ++ (show path)
  show (Sav path _) = "Sav " ++ (show path)
  show (Url c)      = "Url " ++ (show c)

{-
    %blip -- TODO
    %init -- TODO
    %logo -- Shutdown
    %mass -- Measure memory usage (unused)
-}
data TermEf
    = TermEfBlit (UD, ()) [Blit]
    | TermEfInit (UD, ()) Ship
    | TermEfLogo Path ()
    | TermEfMass Path Noun -- Irrelevant
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
