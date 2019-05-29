module Vere.Isle where

import Prelude
import qualified Vere.Isle.Util as C
import qualified SDL

--------------------------------------------------------------------------------

{-

-}

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
