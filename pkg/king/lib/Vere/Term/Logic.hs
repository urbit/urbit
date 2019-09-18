module Vere.Term.Logic
    ( SpinnerCause(..), Ev(..), Ef(..)
    , init
    , step
    , drawState
    ) where

import UrbitPrelude hiding (Empty, getCurrentTime, init)

import Data.Sequence (Seq((:<|), Empty))
import Urbit.Time    (Wen)


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

init :: Wen -> St
init now = St mempty "" 0 Nothing

step :: St -> Ev -> St
step st@St{..} = \case
    EvLine t -> st & record t
    EvSpin s -> st { sSpinner = s }
    EvMove w -> st { sCurPos = min w (word $ length sLine) }
    EvEdit t -> st { sLine = t, sCurPos = word (length t) }
    EvMore   -> st { sLine = "", sCurPos = 0 } & record sLine
    EvBell   -> st
    EvDraw   -> st
  where
    word :: Integral i => i -> Word
    word = fromIntegral

    record :: Text -> St -> St
    record t st@St{..} = st { sHistory = trim (sHistory |> t) }

    trim :: Seq a -> Seq a
    trim Empty             = Empty
    trim s | length s < 20 = s
    trim (_ :<| s)         = s

drawState :: St -> [Ev]
drawState St{..} = hist <> out <> cur <> spin
  where
    hist = EvLine <$> toList sHistory
    out  = if null sLine   then [] else [EvEdit sLine]
    cur  = if 0 == sCurPos then [] else [EvMove $ fromIntegral $ sCurPos]
    spin = maybe [] (singleton . EvSpin . Just) sSpinner
