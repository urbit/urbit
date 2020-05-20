{-|
    Terminal Driver
-}
module Urbit.Vere.Term.Render
    ( TSize(..)
    , tsize
    , clearScreen
    , clearLine
    , cursorRight
    , cursorLeft
    , soundBell
    ) where

import ClassyPrelude

import qualified System.Console.Terminal.Size as TSize
import qualified System.Console.ANSI          as ANSI


-- Types -----------------------------------------------------------------------

data TSize = TSize
    { tsWide ∷ Word
    , tsTall ∷ Word
    }


--------------------------------------------------------------------------------

{- |
    Get terminal size.  Produces 80x24 as a fallback if unable to figure
    out terminal size.
-}
tsize ∷ IO TSize
tsize = do
    TSize.Window wi hi <- TSize.size <&> fromMaybe (TSize.Window 80 24)
    pure $ TSize { tsWide = wi, tsTall = hi }

clearScreen ∷ MonadIO m ⇒ m ()
clearScreen = liftIO $ ANSI.clearScreen

clearLine ∷ MonadIO m ⇒ m ()
clearLine = liftIO $ ANSI.clearLine

soundBell ∷ MonadIO m ⇒ m ()
soundBell = liftIO $ putStr "\a"

cursorLeft ∷ MonadIO m ⇒ Int → m ()
cursorLeft = liftIO . ANSI.cursorBackward

cursorRight ∷ MonadIO m ⇒ Int → m ()
cursorRight = liftIO . ANSI.cursorForward
