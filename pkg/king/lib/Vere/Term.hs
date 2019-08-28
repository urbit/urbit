{-# OPTIONS_GHC -Wwarn #-}

module Vere.Term (initializeTerminal, term, VereTerminal) where

import UrbitPrelude
import Arvo hiding (Term)
import Vere.Pier.Types

import Foreign.Marshal.Alloc
import System.Posix.IO
import System.Posix.Terminal

import System.Console.Terminfo.Base

import qualified Urbit.Time  as Time

-- Types -----------------------------------------------------------------------

data TermDrv = TermDrv
  { tdPreviousConfiguration ::  TerminalAttributes
  , tdReader :: Async ()
  }

data VereTerminal = VereTerminal
  { vtWidth  :: Word
  , vtHeight :: Word
  , vtClearScreen :: Maybe TermOutput

  --
  , vtWriteQueue :: TQueue TermOutput
  , vtWriter :: Async ()
  }

-- A list of terminal flags that we disable
disabledFlags = [
  -- lflag
  EnableEcho, EchoLF, ProcessInput, ExtendedFunctions,
  -- iflag
  MapCRtoLF, CheckParity, StripHighBit,
  -- cflag, todo: Terminal library missing CSIZE?
  EnableParity,
  -- oflag
  ProcessOutput
  ]

-- Utils -----------------------------------------------------------------------

-- TODO: We lie about terminal size for now and just pass 80x24 because getting
-- it is a call to ioctl() which is in IO.

-- TODO: 49 is the string "1", which is what we need to pass to dill as the
-- hard-coded terminal "1" session. Figure out how to turn this into "1" later.
initialBlew w h = EvBlip $ BlipEvTerm $ TermEvBlew (49, ()) w h

initialHail = EvBlip $ BlipEvTerm $ TermEvHail (49, ()) ()


-- What we need is an equivalent to _term_io_suck_char(). That's a manual, hand
-- rolled parser to deal with the escape state.

-- Version one of this is punting on the ops_u.dem flag: whether we're running
-- in daemon mode. This needs to 

--------------------------------------------------------------------------------

initializeTerminal :: Acquire VereTerminal
initializeTerminal = mkAcquire start stop
  where
    start :: IO VereTerminal
    start = do
      t <- setupTermFromEnv
      let vtWidth = 80
      let vtHeight = 24
      let vtClearScreen = getCap t "clear"

      vtWriteQueue <- newTQueueIO
      vtWriter <- asyncBound (writeTerminal t vtWriteQueue)

      pure VereTerminal{..}

    stop :: VereTerminal -> IO ()
    stop (VereTerminal{..}) = cancel vtWriter

    getCap term cap =
      getCapability term (tiGetOutput1 cap) :: Maybe TermOutput

    -- Writes data to the terminal. Both the terminal reading, normal logging,
    -- and effect handling can all emit bytes which go to the terminal.
    writeTerminal :: Terminal -> TQueue TermOutput -> IO ()
    writeTerminal terminal q = forever $ do
      x <- atomically $ readTQueue q
      runTermOutput terminal x


term :: VereTerminal -> KingId -> QueueEv -> ([Ev], Acquire (EffCb TermEf))
term VereTerminal{..} king enqueueEv =
    (initialEvents, runTerm)
  where
    initialEvents = [(initialBlew vtWidth vtHeight), initialHail]

    runTerm :: Acquire (EffCb TermEf)
    runTerm = do
      tim <- mkAcquire start stop
      pure (handleEffect vtWriteQueue tim)

    start :: IO TermDrv
    start = do
      putStrLn "term start"
      tdPreviousConfiguration <- getTerminalAttributes stdInput

      -- Create a new configuration where we put the terminal in raw mode and
      -- disable a bunch of preprocessing.
      let newTermSettings =
            flip withTime     1 .
            flip withMinInput 0 $
            foldl' withoutMode tdPreviousConfiguration disabledFlags
      setTerminalAttributes stdInput newTermSettings Immediately

      tdReader <- asyncBound readTerminal

      pure TermDrv{..}

    stop :: TermDrv -> IO ()
    stop (TermDrv{..}) = do
      -- cancel our threads
      cancel tdReader
      --cancel tdWriter
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
    readTerminal :: IO ()
    readTerminal = allocaBytes 1 $ \ buf -> forever $ do
      t <- try (fdReadBuf stdInput buf 1)
      case t of
        Left (e :: IOException) ->
          -- Ignore EAGAINs when doing reads
          pure ()
        Right 0 -> pure ()
        Right _ -> do
          putStrLn "\r[KEY] "  -- ++ str)
          wen          <- Time.now
          pure ()
--          atomicallu $ enqueuEv $ 

    handleEffect :: TQueue TermOutput -> TermDrv -> TermEf -> IO ()
    handleEffect writeQueue TermDrv{..} = \case
      TermEfBlit _ blits ->
        atomically $ for_ blits (blitToScreen writeQueue)
      TermEfInit _ _ -> pure ()
      TermEfLogo path _ -> pure ()
      TermEfMass _ _ -> pure ()

    -- Write each in
    blitToScreen :: TQueue TermOutput -> Blit -> STM ()
    blitToScreen q = \case
      (Bel ()) -> pure ()
      (Clr ()) -> pure ()
      (Hop w)  -> pure ()
      (Lin c)  -> writeTQueue q $ termText $ pack c
      (Mor ()) -> pure ()
      (Sag path noun) -> pure ()
      (Sav path atom) -> pure ()
      (Url url) -> pure ()

