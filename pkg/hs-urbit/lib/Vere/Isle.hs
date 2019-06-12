module Vere.Isle where

import ClassyPrelude
import Data.Word

import qualified Vere.Isle.Util as C
import qualified SDL            as SDL
import qualified Data.Vector    as V

import Data.Bits   (testBit)
import Data.Vector ((!))
import Data.Flat   (Flat)

--------------------------------------------------------------------------------

newtype Word4 = Word4 Word8
  deriving newtype (Eq, Ord, Num, Integral, Real, Enum)

newtype Word10 = Word10 Word16
  deriving newtype (Eq, Ord, Num, Integral, Real, Enum)

data RGB = RGB !Word8 !Word8 !Word8

type Bitmap = Word64                             --  8x8 bitmap

{-
    TODO Storable instance?
     (Then I can use an unboxed vector)
-}
data Tile = Tile
    { tFore :: !Word4
    , tBack :: !Word4
    , tSpry :: !Word10
    }

data Display = Display
    { dColors  :: V.Vector RGB                   -- size: 16
    , dSprites :: V.Vector Bitmap                -- size: 1024
    , dTiles   :: V.Vector Tile                  -- size: 3600 (80x45)
    , dSurf    :: V.Vector SDL.Surface           -- size: 3600 (80x45)
    }

initializeSurfaces :: IO (V.Vector SDL.Surface)
initializeSurfaces =
  V.generateM 3600
    $ const
    $ SDL.createRGBSurface (SDL.V2 8 8)
    $ SDL.RGB888

initialDisplay :: IO Display
initialDisplay =
  do
    surf <- initializeSurfaces
    pure $ Display (V.generate 16   initialColors)
                   (V.generate 1024 initialSprites)
                   (V.generate 3600 initialTiles)
                   surf
  where
    initialSprites :: Int -> Bitmap
    initialSprites = fromIntegral

    green = 4
    white = 15

    initialTiles :: Int -> Tile
    initialTiles i =
      Tile green white (fromIntegral i `mod` 1024)

    initialColors :: Int -> RGB
    initialColors = \case
      0  -> RGB 0x00 0x00 0x00 -- Black
      1  -> RGB 0x55 0x55 0x55 -- DarkGray
      2  -> RGB 0x00 0x00 0xAA -- Blue
      3  -> RGB 0x55 0x55 0xFF -- LightBlue
      4  -> RGB 0x00 0xAA 0x00 -- Green
      5  -> RGB 0x55 0xFF 0x55 -- LightGreen
      6  -> RGB 0x00 0xAA 0xAA -- Cyan
      7  -> RGB 0x55 0xFF 0xFF -- LightCyan
      8  -> RGB 0xAA 0x00 0x00 -- Red
      9  -> RGB 0xFF 0x55 0x55 -- LightRed
      10 -> RGB 0xAA 0x00 0xAA -- Magenta
      11 -> RGB 0xFF 0x55 0xFF -- LightMagenta
      12 -> RGB 0xAA 0x55 0x00 -- Brown
      13 -> RGB 0xFF 0xFF 0x55 -- Yellow
      14 -> RGB 0xAA 0xAA 0xAA -- LightGray
      15 -> RGB 0xFF 0xFF 0xFF -- White
      n  -> error ("bad color: " <> show n)

renderTile :: Display -> Tile -> SDL.Surface -> IO ()
renderTile d (Tile fg bg tx) surf = do
  let for  = dColors d  ! fromIntegral fg
  let bac  = dColors d  ! fromIntegral bg
  let spry = dSprites d ! fromIntegral tx
  for_ [0..63] $ \i -> do
    let col = if testBit spry i then for else bac
    renderPixel i surf col

renderPixel :: Int -> SDL.Surface -> RGB -> IO ()
renderPixel = undefined


-- data Display = Display
    {-dColors  :: V.Vector RGB                   -- size: 16
    , dSprites :: V.Vector Bitmap                -- size: 1024
    , dTiles   :: V.Vector Tile                  -- size: 3600 (80x45)
    , dSurf    :: V.Vector SDL.Surface           -- size: 3600 (80x45)
   -}

render :: Display -> IO ()
render = undefined

draw :: Display -> IO ()
draw = undefined

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
