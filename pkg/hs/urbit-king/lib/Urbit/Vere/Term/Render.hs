{-|
    Terminal Driver
-}
module Urbit.Vere.Term.Render
    ( clearScreen
    , clearLine
    , soundBell
    , cursorMove
    , cursorSave
    , cursorRestore
    , putCsi
    , hijack
    , lojack
    ) where

import ClassyPrelude

import qualified System.Console.ANSI as ANSI


-- Types -----------------------------------------------------------------------

clearScreen :: MonadIO m => m ()
clearScreen = liftIO $ ANSI.clearScreen

clearLine :: MonadIO m => m ()
clearLine = liftIO $ ANSI.clearLine

soundBell :: MonadIO m => m ()
soundBell = liftIO $ putStr "\a"

--NOTE  top-left-0-based coordinates
cursorMove :: MonadIO m => Int -> Int -> m ()
cursorMove r c = liftIO $ ANSI.setCursorPosition r c

cursorSave :: MonadIO m => m ()
cursorSave = liftIO ANSI.saveCursor

cursorRestore :: MonadIO m => m ()
cursorRestore = liftIO ANSI.restoreCursor

putCsi :: MonadIO m => Char -> [Int] -> m ()
putCsi c a = liftIO do
    putStr "\x1b["
    putStr $ pack $ mconcat $ intersperse ";" (fmap show a)
    putStr $ pack [c]

hijack :: MonadIO m => Int -> m ()
hijack h = liftIO do
    putCsi 'r' [1, h-1]  --  set scroll region to exclude bottom line
    putCsi 'S' [1]       --  scroll up one line
    cursorMove (h-2) 0   --  move cursor to empty space

lojack :: MonadIO m => m ()
lojack = liftIO do
    putCsi 'r' []  --  reset scroll region
    cursorRestore  --  restory cursor position
