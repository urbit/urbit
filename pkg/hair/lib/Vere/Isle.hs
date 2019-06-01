module Vere.Isle where

import ClassyPrelude

import qualified Vere.Isle.Util as C
import qualified SDL            as SDL

import Data.Flat (Flat)

--------------------------------------------------------------------------------

data Color
    = Black     | DarkGray
    | Blue      | LightBlue
    | Green     | LightGreen
    | Cyan      | LightCyan
    | Red       | LightRed
    | Magenta   | LightMagenta
    | Brown     | Yellow
    | LightGray | White
  deriving stock    (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass Flat

type Blit = Vector (Vector Color)

solid :: Color -> Blit
solid c = replicate 640 (replicate 480 c)

toRGB :: Color -> (Word8, Word8, Word8)
toRGB = \case
  Black        -> (0x00, 0x00, 0x00)
  DarkGray     -> (0x55, 0x55, 0x55)
  Blue         -> (0x00, 0x00, 0xAA)
  LightBlue    -> (0x55, 0x55, 0xFF)
  Green        -> (0x00, 0xAA, 0x00)
  LightGreen   -> (0x55, 0xFF, 0x55)
  Cyan         -> (0x00, 0xAA, 0xAA)
  LightCyan    -> (0x55, 0xFF, 0xFF)
  Red          -> (0xAA, 0x00, 0x00)
  LightRed     -> (0xFF, 0x55, 0x55)
  Magenta      -> (0xAA, 0x00, 0xAA)
  LightMagenta -> (0xFF, 0x55, 0xFF)
  Brown        -> (0xAA, 0x55, 0x00)
  Yellow       -> (0xFF, 0xFF, 0x55)
  LightGray    -> (0xAA, 0xAA, 0xAA)
  White        -> (0xFF, 0xFF, 0xFF)

--------------------------------------------------------------------------------

main :: IO ()
main = C.withSDL $ C.withWindow "Lesson 01" (640, 480) $
  \w -> do

    screen <- SDL.getWindowSurface w
    -- pixelFormat <- SDL.surfaceFormat `applyToPointer` screen
    -- color <- SDL.mapRGB pixelFormat 0xFF 0xFF 0xFF
    SDL.surfaceFillRect screen Nothing (SDL.V4 maxBound maxBound maxBound maxBound)
    SDL.updateWindowSurface w

    SDL.delay 2000

    SDL.freeSurface screen
