{-# OPTIONS_GHC -Wwarn #-}

module Vere.Term (initializeLocalTerminal, term, TerminalSystem, tsReaderThread) where

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

-- Output to the attached terminal is either a series of vere blits, or it is an
-- injected printf line from the interpreter.
data VereOutput = VereBlitOutput [Blit]
                | VerePrintOutput String
                | VereBlankLine

data LineState = LineState String Int

-- A record used in reading data from stdInput.
data ReadData = ReadData
  { rdBuf :: Ptr Word8
  , rdEscape :: Bool
  , rdBracket :: Bool
  }

-- Minimal terminal interface.
--
-- A Terminal can either be local or remote. Either way, the Terminal, from the
-- view of the caller, a terminal has a thread which when exits indicates that
-- the session is over, and has a general in/out queue in the types of the
-- vere/arvo interface.
data TerminalSystem = TerminalSystem
  -- | The reader can be waited on, as it shuts itself down when the console
  -- goes away.
  { tsReaderThread :: Async()
  , tsReadQueue    :: TQueue Belt
  , tsWriteQueue   :: TQueue VereOutput
  }

-- Private data to the TerminalSystem that we keep around for stop().
data Private = Private
  { pWriterThread :: Async()
  , pPreviousConfiguration ::  TerminalAttributes
  }

-- Utils -----------------------------------------------------------------------

initialBlew w h = EvBlip $ BlipEvTerm $ TermEvBlew (UD 1, ()) w h

initialHail = EvBlip $ BlipEvTerm $ TermEvHail (UD 1, ()) ()

-- Version one of this is punting on the ops_u.dem flag: whether we're running
-- in daemon mode.

--------------------------------------------------------------------------------

runMaybeTermOutput :: Terminal -> (Terminal -> Maybe TermOutput) -> IO ()
runMaybeTermOutput t getter = case (getter t) of
  Nothing -> pure ()
  Just x -> runTermOutput t x

-- Because of legacy reasons, some file operations are in the terminal
-- driver. These should be filtered out and handled locally instead of in any
-- abstractly connected terminal.
isTerminalBlit :: Blit -> Bool
isTerminalBlit (Sav _ _) = False
isTerminalBlit (Sag _ _) = False
isTerminalBlit _         = True

--------------------------------------------------------------------------------

-- Initializes the generalized input/output parts of the terminal.
--
initializeLocalTerminal :: Acquire TerminalSystem
initializeLocalTerminal = do
    (a, b) <- mkAcquire start stop
    pure a
  where
    start :: IO (TerminalSystem, Private)
    start = do
      --  Initialize the writing side of the terminal
      --
      t <- setupTermFromEnv
      -- TODO: We still need to actually get this from the terminal somehow.

      tsWriteQueue <- newTQueueIO
      pWriterThread <- asyncBound (writeTerminal t tsWriteQueue)

      pPreviousConfiguration <- getTerminalAttributes stdInput

      -- Create a new configuration where we put the terminal in raw mode and
      -- disable a bunch of preprocessing.
      let newTermSettings =
            flip withTime     0 .
            flip withMinInput 1 $
            foldl' withoutMode pPreviousConfiguration disabledFlags
      setTerminalAttributes stdInput newTermSettings Immediately

      tsReadQueue <- newTQueueIO
      tsReaderThread <- asyncBound (readTerminal tsReadQueue tsWriteQueue (bell tsWriteQueue))

      pure (TerminalSystem{..}, Private{..})

    stop :: (TerminalSystem, Private) -> IO ()
    stop (TerminalSystem{..}, Private{..}) = do
      cancel tsReaderThread
      cancel pWriterThread
      -- take the terminal out of raw mode
      setTerminalAttributes stdInput pPreviousConfiguration Immediately

    -- A list of terminal flags that we disable
    disabledFlags = [
      -- lflag
      StartStopOutput, KeyboardInterrupts, EnableEcho, EchoLF,
      ProcessInput, ExtendedFunctions,
      -- iflag
      MapCRtoLF, CheckParity, StripHighBit,
      -- cflag, todo: Terminal library missing CSIZE?
      EnableParity,
      -- oflag
      ProcessOutput
      ]

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
              s <- foldM (writeBlit t) s blits
              loop s
            VerePrintOutput p -> do
              runTermOutput t $ termText "\r"
              runMaybeTermOutput t vtClearToBegin
              runTermOutput t $ termText p
              runTermOutput t $ termText "\r\n"
              s <- termRefreshLine t s
              loop s
            VereBlankLine -> do
              runTermOutput t $ termText "\r\n"
              loop s

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

    -- ring my bell
    bell :: TQueue VereOutput -> IO ()
    bell q = do
      atomically $ writeTQueue q $ VereBlitOutput [Bel ()]

    -- Reads data from stdInput and emit the proper effect
    --
    -- This entire path is a divergence from how term.c does things,
    -- probably. First, the vtime is 0, not 1 in term.c. So (IIUC), we'll
    -- always have a latency of 1/10 of a second.
    --
    -- A better way to do this would be to get some sort of epoll on stdInput,
    -- since that's kinda closer to what libuv does?
    readTerminal :: TQueue Belt -> TQueue VereOutput -> (IO ()) -> IO ()
    readTerminal rq wq bell = allocaBytes 1 $ \ buf -> loop (ReadData buf False False)
      where
        loop rd@ReadData{..} = do
          -- The problem with using fdRead raw is that it will text encode things
          -- like \ESC instead of 27. That makes it broken for our purposes.
          --
          t <- try (fdReadBuf stdInput rdBuf 1)
          case t of
            Left (e :: IOException) -> do
              -- Ignore EAGAINs when doing reads
              loop rd
            Right 0 -> loop rd
            Right _ -> do
              w   <- peek rdBuf
              -- print ("{" ++ (show w) ++ "}")
              let c = w2c w
              if rdEscape then
                if rdBracket then do
                  case c of
                    'A' -> sendBelt $ Aro U
                    'B' -> sendBelt $ Aro D
                    'C' -> sendBelt $ Aro R
                    'D' -> sendBelt $ Aro L
                    _   -> bell
                  loop rd { rdEscape = False, rdBracket = False}
                else if isAsciiLower c then do
                  sendBelt $ Met $ Cord $ pack [c]
                  loop rd { rdEscape = False }
                else if c == '.' then do
                  sendBelt $ Met $ Cord "dot"
                  loop rd { rdEscape = False }
                else if w == 8 || w == 127 then do
                  sendBelt $ Met $ Cord "bac"
                  loop rd { rdEscape = False }
                else if c == '[' || c == '0' then do
                  loop rd { rdBracket = True }
                else do
                  bell
                  loop rd { rdEscape = False }
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
                atomically $ do
                  writeTQueue wq $ VerePrintOutput "interrupt"
                  writeTQueue rq $ Ctl $ Cord "c"
                loop rd
              else if w <= 26 then do
                sendBelt $ Ctl $ Cord $ pack [w2c (w + 97 - 1)]
                loop rd
              else if w == 27 then do
                loop rd { rdEscape = True }
              else do
                -- start the utf8 accumulation buffer
                loop rd

        sendBelt :: Belt -> IO ()
        sendBelt b = do
          atomically $ writeTQueue rq b

--------------------------------------------------------------------------------

term :: TerminalSystem -> KingId -> QueueEv -> ([Ev], Acquire (EffCb e TermEf))
term TerminalSystem{..} king enqueueEv =
    (initialEvents, runTerm)
  where
    initialEvents = [(initialBlew 80 24), initialHail]

    runTerm :: Acquire (EffCb e TermEf)
    runTerm = do
      tim <- mkAcquire start stop
      pure (io . handleEffect)

    start :: IO (Async ())
    start = async readBelt

    stop :: Async () -> IO ()
    stop rb = cancel rb

    readBelt :: IO ()
    readBelt = forever $ do
      b <- atomically $ readTQueue tsReadQueue
      let blip = EvBlip $ BlipEvTerm $ TermEvBelt (UD 1, ()) $ b
      atomically $ enqueueEv $ blip

    handleEffect :: TermEf -> IO ()
    handleEffect = \case
      TermEfBlit _ blits -> do
        let (termBlits, fsWrites) = partition isTerminalBlit blits
        atomically $ writeTQueue tsWriteQueue (VereBlitOutput termBlits)
        for_ fsWrites handleFsWrite
      TermEfInit _ _ -> pure ()
      TermEfLogo path _ -> do
        -- %logo is the shutdown path. A previous iteration just had the reader
        -- thread exit when it saw a ^D, which was wrong because it didn't emit
        -- a ^D to your Urbit, which does things and then sends us a %logo.
        --
        -- But this isn't optimal either. Right now, Pier spins forever,
        -- waiting for some piece to exit or die, and I added the terminal
        -- reading Async for expedience. But the terminal system ending should
        -- additionally trigger taking a snapshot, along with any other clean
        -- shutdown work.
        --
        -- If we have a separate terminal program which connects to the daemon,
        -- this shouldn't be shutdown, but should be a sort of disconnect,
        -- meaning it would be a VereOutput?
        cancel tsReaderThread
      TermEfMass _ _ -> pure ()

    handleFsWrite :: Blit -> IO ()
    handleFsWrite (Sag path noun) = pure ()
    handleFsWrite (Sav path atom) = pure ()
    handleFsWrite _ = pure ()
