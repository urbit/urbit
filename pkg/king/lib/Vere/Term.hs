{-# OPTIONS_GHC -Wwarn #-}

module Vere.Term (initializeTerminal, term, VereTerminal) where

import UrbitPrelude
import Arvo hiding (Term)
import Vere.Pier.Types

import Data.Char
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Posix.IO
import System.Posix.Terminal

import System.Console.Terminfo.Base

import Data.ByteString.Internal

-- Types -----------------------------------------------------------------------

data TermDrv = TermDrv
  { tdPreviousConfiguration ::  TerminalAttributes
  , tdReader :: Async ()
  }

-- Output to the attached terminal is either a series of vere blits, or it is an
-- injected printf line from the interpreter.
data VereOutput = VereBlitOutput [Blit]
                | VerePrintOutput String

data VereTerminal = VereTerminal
  { vtWidth  :: Word
  , vtHeight :: Word

  --
  , vtWriteQueue :: TQueue VereOutput
  , vtWriter :: Async ()
  }

data LineState = LineState String Int

-- A list of terminal flags that we disable
disabledFlags = [
  -- lflag
  StartStopOutput, KeyboardInterrupts, EnableEcho, EchoLF, ProcessInput, ExtendedFunctions,
  -- iflag
  MapCRtoLF, CheckParity, StripHighBit,
  -- cflag, todo: Terminal library missing CSIZE?
  EnableParity,
  -- oflag
  ProcessOutput
  ]

-- A record used in reading data from stdInput.
data ReadData = ReadData
  { rBuf :: Ptr Word8
  , rEscape :: Bool
  , rBracket :: Bool
  }

-- Utils -----------------------------------------------------------------------

-- TODO: We lie about terminal size for now and just pass 80x24 because getting
-- it is a call to ioctl() which is in IO.

initialBlew w h = EvBlip $ BlipEvTerm $ TermEvBlew (UD 1, ()) w h

initialHail = EvBlip $ BlipEvTerm $ TermEvHail (UD 1, ()) ()


-- What we need is an equivalent to _term_io_suck_char(). That's a manual, hand
-- rolled parser to deal with the escape state.

-- Version one of this is punting on the ops_u.dem flag: whether we're running
-- in daemon mode.

--------------------------------------------------------------------------------

runMaybeTermOutput :: Terminal -> (Terminal -> Maybe TermOutput) -> IO ()
runMaybeTermOutput t getter = case (getter t) of
  Nothing -> pure ()
  Just x -> runTermOutput t x

--------------------------------------------------------------------------------

initializeTerminal :: Acquire VereTerminal
initializeTerminal = mkAcquire start stop
  where
    start :: IO VereTerminal
    start = do
      t <- setupTermFromEnv
      let vtWidth = 80
      let vtHeight = 24

      vtWriteQueue <- newTQueueIO
      vtWriter <- asyncBound (writeTerminal t vtWriteQueue)

      pure VereTerminal{..}

    stop :: VereTerminal -> IO ()
    stop (VereTerminal{..}) = cancel vtWriter

    getCap term cap =
      getCapability term (tiGetOutput1 cap) :: Maybe TermOutput

    vtClearScreen t  = getCap t "clear"
    vtClearToBegin t = getCap t "el"
    vtSoundBell t    = getCap t "bel"
    vtParmLeft t     = getCap t "cub1"
    vtParmRight t    = getCap t "cuf1"

    -- Writes data to the terminal. Both the terminal reading, normal logging,
    -- and effect handling can all emit bytes which go to the terminal.
    writeTerminal :: Terminal -> TQueue VereOutput -> IO ()
    writeTerminal t q = loop (LineState "" 0)
      where
        loop s = do
          x <- atomically $ readTQueue q
          case x of
            VereBlitOutput blits -> do
              newS <- foldM (writeBlit t) s blits
              loop newS
            VerePrintOutput p -> do
              runTermOutput t $ termText "\r"
              runMaybeTermOutput t vtClearToBegin
              runTermOutput t $ termText p
              newS <- termRefreshLine t s
              loop newS

    -- Writes an individual blit to the screen
    writeBlit :: Terminal -> LineState -> Blit -> IO LineState
    writeBlit t ls = \case
      Bel () -> do
        runMaybeTermOutput t vtSoundBell
        pure ls
      Clr () -> do
        runMaybeTermOutput t vtClearScreen
        termRefreshLine t ls
      (Hop w) -> do
        termShowCursor t ls (fromIntegral w)
      (Lin c)  -> do
        ls2 <- termShowClear t ls
        termShowLine t ls2 (pack c)
      (Mor ()) -> do
        termShowMore t ls
      (Sag path noun) -> pure ls
      (Sav path atom) -> pure ls
      (Url url) -> pure ls

    -- Moves the cursor to the requested position
    termShowCursor :: Terminal -> LineState -> Int -> IO LineState
    termShowCursor t (LineState line pos) newPos = do
      if newPos < pos then do
        replicateM_ (pos - newPos) (runMaybeTermOutput t vtParmLeft)
        pure (LineState line newPos)
      else if newPos > pos then do
        replicateM_ (newPos - pos) (runMaybeTermOutput t vtParmRight)
        pure (LineState line newPos)
      else
        pure (LineState line pos)

    -- Displays and sets the current line
    termShowLine :: Terminal -> LineState -> String -> IO LineState
    termShowLine t ls newStr = do
      -- TODO: Really think about how term.c munged cus_w. Amidoinitrit?
      runTermOutput t $ termText newStr
      pure (LineState newStr (length newStr))

    termShowClear :: Terminal -> LineState -> IO LineState
    termShowClear t ls = do
      runTermOutput t $ termText "\r"
      runMaybeTermOutput t vtClearToBegin
      pure (LineState "" 0)

    -- New Current Line
    termShowMore :: Terminal -> LineState -> IO LineState
    termShowMore t ls = do
      runTermOutput t $ termText "\r\n"
      pure (LineState "" 0)

    -- Redraw the current LineState, moving cursor to the end.
    termRefreshLine :: Terminal -> LineState -> IO LineState
    termRefreshLine t ls@(LineState line pos) = do
      runMaybeTermOutput t vtClearToBegin
      newLs <- termShowLine t ls line
      termShowCursor t newLs pos


term :: VereTerminal -> KingId -> QueueEv -> ([Ev], Acquire (EffCb e TermEf))
term VereTerminal{..} king enqueueEv =
    (initialEvents, runTerm)
  where
    initialEvents = [(initialBlew vtWidth vtHeight), initialHail]

    runTerm :: Acquire (EffCb e TermEf)
    runTerm = do
      tim <- mkAcquire start stop
      pure (io . handleEffect vtWriteQueue tim)

    start :: IO TermDrv
    start = do
      putStrLn "term start"
      tdPreviousConfiguration <- getTerminalAttributes stdInput

      -- Create a new configuration where we put the terminal in raw mode and
      -- disable a bunch of preprocessing.
      let newTermSettings =
            flip withTime     0 .
            flip withMinInput 1 $
            foldl' withoutMode tdPreviousConfiguration disabledFlags
      setTerminalAttributes stdInput newTermSettings Immediately

      tdReader <- asyncBound (readTerminal (bell vtWriteQueue))

      pure TermDrv{..}

    bell :: TQueue VereOutput -> IO ()
    bell q = do
      atomically $ writeTQueue q $ VereBlitOutput [Bel ()]

    stop :: TermDrv -> IO ()
    stop (TermDrv{..}) = do
      -- cancel our threads
      cancel tdReader
      -- take the terminal out of raw mode
      setTerminalAttributes stdInput tdPreviousConfiguration Immediately

    -- Reads data from stdInput and emit the proper effect
    --
    -- This entire path is a divergence from how term.c does things,
    -- probably. First, the vtime is 0, not 1 in term.c. So (IIUC), we'll
    -- always have a latency of 1/10 of a second.
    --
    -- A better way to do this would be to get some sort of epoll on stdInput,
    -- since that's kinda closer to what libuv does?
    readTerminal :: (IO ()) -> IO ()

    readTerminal bell = allocaBytes 1 $ \ buf -> loop (ReadData buf False False)
      where
        loop rd@ReadData{..} = do
          -- The problem with using fdRead raw is that it will text encode things
          -- like \ESC instead of 27. That makes it broken for our purposes.
          --
          t <- try (fdReadBuf stdInput rBuf 1)
          case t of
            Left (e :: IOException) -> do
              -- Ignore EAGAINs when doing reads
              loop rd
            Right 0 -> loop rd
            Right _ -> do
              w   <- peek rBuf
              -- print ("{" ++ (show w) ++ "}")
              let c = w2c w
              if rEscape then
                if rBracket then do
                  case c of
                    'A' -> sendBelt $ Aro U
                    'B' -> sendBelt $ Aro D
                    'C' -> sendBelt $ Aro R
                    'D' -> sendBelt $ Aro L
                    _   -> bell
                  loop rd { rEscape = False, rBracket = False}
                else if isAsciiLower c then do
                  sendBelt $ Met $ Cord $ pack [c]
                  loop rd { rEscape = False }
                else if c == '.' then do
                  sendBelt $ Met $ Cord "dot"
                  loop rd { rEscape = False }
                else if w == 8 || w == 127 then do
                  sendBelt $ Met $ Cord "bac"
                  loop rd { rEscape = False }
                else if c == '[' || c == '0' then do
                  loop rd { rBracket = True }
                else do
                  bell
                  loop rd { rEscape = False }
              -- if not escape
              else if False then
                -- TODO: Put the unicode accumulation logic here.
                loop rd
              else if w >= 32 && w < 127 then do
                sendBelt $ Txt $ Tour $ [c]
                loop rd
              else if w == 0 then do
                bell
                loop rd
              else if w == 8 || w == 127 then do
                sendBelt $ Bac ()
                loop rd
              else if w == 13 then do
                sendBelt $ Ret ()
                loop rd
              else if w == 3 then do
                -- ETX (^C)
                putStrLn "{ctrl-c}"
                loop rd
              else if w == 4 then do
                -- EOT (^D)
                putStrLn "{ctrl-d}"
                loop rd
              else if w <= 26 then do
                sendBelt $ Ctl $ Cord $ pack [w2c (w + 97 - 1)]
                loop rd
              else if w == 27 then do
                loop rd { rEscape = True }
              else do
                -- start the utf8 accumulation buffer
                loop rd

        sendBelt :: Belt -> IO ()
        sendBelt b = do
          let blip = EvBlip $ BlipEvTerm $ TermEvBelt (UD 1, ()) $ b
          atomically $ enqueueEv $ blip


    handleEffect :: TQueue VereOutput -> TermDrv -> TermEf -> IO ()
    handleEffect writeQueue TermDrv{..} = \case
      TermEfBlit _ blits -> atomically $ writeTQueue writeQueue (VereBlitOutput blits)
      TermEfInit _ _ -> pure ()
      TermEfLogo path _ -> pure ()
      TermEfMass _ _ -> pure ()
