{-# LANGUAGE StrictData #-}

-- This is required due to the use of 'Void' in a constructor slot in
-- combination with 'deriveNoun' which generates an unreachable pattern.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
    Effect Types and Their Noun Conversions
-}
module Urbit.Arvo.Effect where

import Urbit.Noun.Time
import Urbit.Prelude

import Control.Monad.Fail (fail)
import Numeric.Natural    (Natural)
import Urbit.Arvo.Common (SocketConf, KingId(..), ServId(..), SocketId(..))
import Urbit.Arvo.Common (Header, HttpEvent, HttpServerConf, Method, Mime, SocketEvent)
import Urbit.Arvo.Common (AmesDest, Turf)
import Urbit.Arvo.Common (ReOrg(..), reorgThroughNoun)
import Urbit.Arvo.Common (Desk, Wynn)


-- Newt Effects ----------------------------------------------------------------

{-|
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

{-|
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

{-|
    %set-config -- Update HTTP server configuration.
    %response   -- Respond to an active HTTP request.
-}
data HttpServerEf
    = HSESetConfig (ServId, ())         HttpServerConf
    | HSEResponse  (ServId, UD, UD, ()) HttpEvent
  deriving (Eq, Ord, Show)

deriveNoun ''HttpServerEf


-- File System Effects ---------------------------------------------------------

{-|
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


-- Timer Effects ---------------------------------------------------------------

{-|
    %doze -- Set or clear timer.
    %void -- Nasty hack to make the parser not treat this as a record.
-}
data BehnEf
    = BehnEfDoze (KingId, ()) (Maybe Wen)
    | BehnEfVoid Void
  deriving (Eq, Ord, Show)

deriveNoun ''BehnEf


-- Terminal Effects ------------------------------------------------------------

{-|
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
    | Klr Stub
    | Lin [Char]
    | Mor ()
    | Sag Path Noun
    | Sav Path Atom
    | Url Cord
  deriving (Eq, Ord)

data Deco
    = DecoBl
    | DecoBr
    | DecoUn
    | DecoNull
  deriving (Eq, Ord, Show)

data Tint
    = TintR
    | TintG
    | TintB
    | TintC
    | TintM
    | TintY
    | TintK
    | TintW
    | TintNull
    | TintTrue Word8 Word8 Word8
  deriving (Eq, Ord, Show)

data Stye = Stye
    { deco :: (HoonSet Deco)
    , back :: Tint
    , fore :: Tint
    }
  deriving (Eq, Ord, Show)

newtype Stub = Stub [(Stye, [Char])]
  deriving (Eq, Ord, Show)

instance ToNoun Deco where
  toNoun = \case
    DecoBl   -> toNoun $ Cord "bl"
    DecoBr   -> toNoun $ Cord "br"
    DecoUn   -> toNoun $ Cord "un"
    DecoNull -> Atom 0

instance FromNoun Deco where
  parseNoun = named "Deco" . \case
    Atom 0 -> pure DecoNull
    n      -> parseNoun @Cord n <&> unCord >>= \case
                "bl" -> pure DecoBl
                "br" -> pure DecoBr
                "un" -> pure DecoUn
                t    -> fail ("invalid: " <> unpack t)

instance ToNoun Tint where
  toNoun = \case
    TintR          -> toNoun $ Cord "r"
    TintG          -> toNoun $ Cord "g"
    TintB          -> toNoun $ Cord "b"
    TintC          -> toNoun $ Cord "c"
    TintM          -> toNoun $ Cord "m"
    TintY          -> toNoun $ Cord "y"
    TintK          -> toNoun $ Cord "k"
    TintW          -> toNoun $ Cord "w"
    TintNull       -> Atom 0
    TintTrue r g b -> Cell (atom r) $ Cell (atom g) (atom b)
                      where atom a = Atom (fromIntegral a :: Natural)

instance FromNoun Tint where
  parseNoun = named "Tint" . \case
    Atom 0 -> pure TintNull
    Cell (Atom r) (Cell (Atom g) (Atom b))
           -> pure (TintTrue (word r) (word g) (word b))
              where word w = fromIntegral w :: Word8
    n      -> parseNoun @Cord n <&> unCord >>= \case
                "r" -> pure TintR
                "g" -> pure TintG
                "b" -> pure TintB
                "c" -> pure TintC
                "m" -> pure TintM
                "y" -> pure TintY
                "k" -> pure TintK
                "w" -> pure TintW
                t   -> fail ("invalid: " <> unpack t)

-- Manual instance to not save the noun/atom in Sag/Sav, because these can be
-- megabytes and makes king hang.
instance Show Blit where
  show (Bel ())     = "Bel ()"
  show (Clr ())     = "Clr ()"
  show (Hop x)      = "Hop " ++ (show x)
  show (Klr s)      = "Klr " ++ (show s)
  show (Lin c)      = "Lin " ++ (show c)
  show (Mor ())     = "Mor ()"
  show (Sag path _) = "Sag " ++ (show path)
  show (Sav path _) = "Sav " ++ (show path)
  show (Url c)      = "Url " ++ (show c)

{-|
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

deriveNoun ''Stye
deriveNoun ''Stub
deriveNoun ''Blit
deriveNoun ''TermEf

-- Khan Effects ----------------------------------------------------------------
data KhanEf
  = KhanEfAvow Path Noun
  | KhanEfFyrd Path Noun
  deriving (Eq, Ord, Show)

deriveNoun ''KhanEf

data SocketEf
  = SEError (KingId, ()) ()
  | SESetConfig (SocketId, ()) SocketConf
  | SEResponse  (SocketId, UD, UD, ()) SocketEvent

  deriving (Eq, Ord, Show)

deriveNoun ''SocketEf
-- IO-Driver Routing -----------------------------------------------------------

data VaneEf
    = VENewt       NewtEf
    | VEHttpClient HttpClientEf
    | VEHttpServer HttpServerEf
    | VESocket     SocketEf
    | VEBehn       BehnEf
    | VETerm       TermEf
    | VEKhan       KhanEf
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
    | EfWend Wynn
  deriving (Eq, Ord, Show)

-- XX HACK
clip :: Noun -> Noun
clip (C (C _ x) y) = C x y
clip _ = error "misclip"

tack :: Noun -> Noun
tack (C x y) = C (C (A 0) x) y
tack _ = error "mistack"

instance ToNoun Ef where
  toNoun = clip . \case
    EfVane v   -> toNoun $ reorgThroughNoun ("", v)
    EfExit s p -> toNoun $ ReOrg "" s "exit" p (A 0)
    EfVega s p -> toNoun $ ReOrg "" s "vega" p (A 0)
    EfWend w   -> toNoun $ reorgThroughNoun ("", w)

instance FromNoun Ef where
  parseNoun = tack >>> parseNoun >=> \case
    ReOrg "" s "exit" p (A 0) -> pure (EfExit s p)
    ReOrg "" s "exit" p _     -> fail "%exit effect expects nil value"
    ReOrg "" s "vega" p (A 0) -> pure (EfVega s p)
    ReOrg "" s "vega" p _     -> fail "%vega effect expects nil value"
    ReOrg "" s "wend" p val   -> EfWend <$> parseNoun val
    ReOrg "" s tag    p val   -> EfVane <$> parseNoun (toNoun (s, tag, p, val))
    ReOrg _  _ _      _ _     -> fail "Non-empty first path-element"

summarizeEffect :: Lenient Ef -> Text
summarizeEffect ef =
  fromNoun (toNoun ef) & \case
    Nothing -> "//invalid %effect"
    Just (pax :: [Cord], tag :: Cord, val :: Noun) ->
      "/" <> intercalate "/" (unCord <$> pax) <> " %" <> unCord tag
