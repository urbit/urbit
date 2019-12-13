module Vere.Term.Logic
    ( SpinnerCause(..), St, Ev(..), Ef(..)
    , init
    , step
    , drawState
    , fromTermEv
    , toTermEv
    ) where

import UrbitPrelude hiding (init)

import Data.Sequence (Seq((:<|)))

import qualified Arvo
import qualified Vere.Term.API as Term


--------------------------------------------------------------------------------

data SpinnerCause = User | Event Text
  deriving (Show)

type SpinnerState = Maybe SpinnerCause

{-
   %line -- Output a line above the edit line.
   %spin -- Set the spinner state.
   %bell -- Ring a bell (no change to the state).
   %draw -- Redraw the current line (no change to the state).
   %move -- Move the cursor position.
   %edit -- Set the edit line, moving the cursor to the end.
   %more -- Write the edit line to history, and clear it.
-}
data Ev
    = EvLine Text
    | EvSpin SpinnerState
    | EvMove Word
    | EvBell
    | EvDraw
    | EvEdit Text
    | EvMore
  deriving (Show)

data Ef
    = EfClear
    | EfWrite Text
    | EfShift Int
    | EfRing
    | EfSpin SpinnerState
  deriving (Show)

data St = St
    { sHistory :: Seq Text
    , sLine    :: Text
    , sCurPos  :: Word
    , sSpinner :: SpinnerState
    }
  deriving (Show)

--------------------------------------------------------------------------------

init :: St
init = St mempty "" 0 Nothing

{-
    When we process `EvMore`, we need to append a newline to the end of
    the current line. During normal play, the ENTER key inserts the
    newline for us, so we need to recreate that newline when we rebuild
    the state for a new terminal connection.
-}
step :: St -> Ev -> St
step st@St{..} = \case
    EvLine t -> st & record t
    EvSpin s -> st { sSpinner = s }
    EvMove w -> st { sCurPos = min w (word $ length sLine) }
    EvEdit t -> st { sLine = t, sCurPos = word (length t) }
    EvMore   -> st { sLine = "", sCurPos = 0 } & record (sLine <> "\n")
    EvBell   -> st
    EvDraw   -> st
  where
    word :: Integral i => i -> Word
    word = fromIntegral

    record :: Text -> St -> St
    record t st@St{..} = st { sHistory = trim (sHistory |> t) }

    trim :: Seq a -> Seq a
    trim s | length s < 20 = s
    trim (_ :<| s)         = s
    trim s                 = s

drawState :: St -> [Ev]
drawState St{..} = hist <> out <> cur <> spin
  where
    hist = EvLine <$> toList sHistory
    out  = if null sLine   then [] else [EvEdit sLine]
    cur  = if 0 == sCurPos then [] else [EvMove $ fromIntegral $ sCurPos]
    spin = maybe [] (singleton . EvSpin . Just) sSpinner


-- Conversion ------------------------------------------------------------------

fromBlit :: Arvo.Blit -> Maybe Ev
fromBlit = \case
    Arvo.Hop w  -> Just $ EvMove $ fromIntegral w
    Arvo.Bel () -> Just EvBell
    Arvo.Clr () -> Just EvDraw
    Arvo.Lin s  -> Just $ EvEdit (pack s)
    Arvo.Mor () -> Just EvMore
    _           -> Nothing

toCause :: Maybe Cord -> SpinnerCause
toCause Nothing         = User
toCause (Just (Cord c)) = Event c

fromCause :: SpinnerCause -> Maybe Cord
fromCause User      = Nothing
fromCause (Event t) = Just (Cord t)

fromTermEv :: Term.Ev -> [Ev]
fromTermEv = \case
    Term.Blits bs -> catMaybes (fromBlit <$> bs)
    Term.Trace t  -> [EvLine $ unCord t]
    Term.Blank    -> [EvLine ""]
    Term.Spinr s  -> [EvSpin $ toCause <$> s]

toTermEv :: Ev -> Term.Ev
toTermEv = \case
    EvLine "" -> Term.Blank
    EvLine t  -> Term.Trace (Cord t)
    EvSpin s  -> Term.Spinr (fromCause <$> s)
    EvMove w  -> Term.Blits [Arvo.Hop $ fromIntegral w]
    EvBell    -> Term.Blits [Arvo.Bel ()]
    EvDraw    -> Term.Blits [Arvo.Clr ()]
    EvEdit t  -> Term.Blits [Arvo.Lin $ unpack t]
    EvMore    -> Term.Blits [Arvo.Mor ()]
