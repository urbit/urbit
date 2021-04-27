{-|
    Terminal Driver
-}
module Urbit.Vere.Term.Render
    ( clearScreen
    , clearLine
    , cursorRight
    , cursorLeft
    , soundBell
    ) where

import Urbit.Prelude

import qualified System.Console.ANSI as ANSI


-- Types -----------------------------------------------------------------------

clearScreen :: MonadIO m => m ()
clearScreen = liftIO $ ANSI.clearScreen

clearLine :: MonadIO m => m ()
clearLine = liftIO $ ANSI.clearLine

soundBell :: MonadIO m => m ()
soundBell = liftIO $ putStr "\a"

cursorLeft :: MonadIO m => Int -> m ()
cursorLeft = liftIO . ANSI.cursorBackward

cursorRight :: MonadIO m => Int -> m ()
cursorRight = liftIO . ANSI.cursorForward
