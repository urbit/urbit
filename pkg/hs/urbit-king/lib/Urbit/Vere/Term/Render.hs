{-|
    Terminal Driver
-}
module Urbit.Vere.Term.Render
    ( TSize(..)
    , tsize
    , Terminal
    , Capability
    , TermOutput
    , termText
    , runTermOutput
    , setupTermFromEnv
    , getCapability
    , tiGetOutput1
    , clearScreen
    , clearLine
    , cursorRight
    , cursorLeft
    , soundBell
    ) where

import ClassyPrelude

import qualified System.Console.Terminal.Size as TSize
import qualified System.Console.Terminfo.Base as TInfo
import qualified System.Console.ANSI          as ANSI


--------------------------------------------------------------------------------

type Terminal = TInfo.Terminal

type Capability f = TInfo.Capability f

type TermOutput = TInfo.TermOutput

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

termText ∷ String -> TermOutput
termText = TInfo.termText

runTermOutput ∷ Terminal -> TermOutput -> IO ()
runTermOutput = TInfo.runTermOutput

-- Deprecated ------------------------------------------------------------------

setupTermFromEnv ∷ IO Terminal
setupTermFromEnv = TInfo.setupTermFromEnv

getCapability ∷ Terminal -> Capability a -> Maybe a
getCapability = TInfo.getCapability

tiGetOutput1 ∷ TInfo.OutputCap f => String -> Capability f
tiGetOutput1 = TInfo.tiGetOutput1

--------------------------------------------------------------------------------

clearScreen ∷ IO ()
clearScreen = ANSI.clearScreen

clearLine ∷ IO ()
clearLine = ANSI.clearLine

soundBell ∷ IO ()
soundBell = putStr "\BEL"

cursorLeft ∷ IO ()
cursorLeft = ANSI.cursorBackward 1

cursorRight ∷ IO ()
cursorRight = ANSI.cursorForward 1
