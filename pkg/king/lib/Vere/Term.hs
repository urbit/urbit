{-# OPTIONS_GHC -Wwarn #-}

module Vere.Term (term) where

import UrbitPrelude
import Arvo hiding (Term)
import Vere.Pier.Types

import System.Posix.IO
import System.Posix.Terminal

import qualified Urbit.Time  as Time

-- Types -----------------------------------------------------------------------

data TermDrv = TermDrv
  { tdPreviousConfiguration ::  TerminalAttributes
  , tdReader :: Async ()
  , tdWriter :: Async ()

  , tdWriteQueue :: TQueue ByteString
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

-- TODO: We lie about terminal size for now because getting it is a call to
-- ioctl().

-- TODO: 49 is the string "1", which is what we need to pass to dill as the
-- hard-coded terminal "1" session. Figure out how to turn this into "1" later.
initialBlew = EvBlip $ BlipEvTerm $ TermEvBlew (49, ()) 80 24

initialHail = EvBlip $ BlipEvTerm $ TermEvHail (49, ()) ()


-- What we need is an equivalent to _term_io_suck_char(). That's a manual, hand
-- rolled parser to deal with the escape state.

--------------------------------------------------------------------------------

term :: KingId -> QueueEv -> ([Ev], Acquire (EffCb TermEf))
term king enqueueEv =
    (initialEvents, runTerm)
  where
    initialEvents = [initialBlew, initialHail]

    runTerm :: Acquire (EffCb TermEf)
    runTerm = do
      tim <- mkAcquire start stop
      pure (handleEffect tim)

    start :: IO TermDrv
    start = do
      putStrLn "term start"
      tdPreviousConfiguration <- getTerminalAttributes stdInput

      -- Create a new configuration where we put the terminal in raw mode and
      -- disable a bunch of preprocessing.
      --
      -- This is a departure from vere's term.c, which set vmin=0 and vtime=0.
      let newTermSettings =
            flip withTime     0 .
            flip withMinInput 0 $
            foldl' withoutMode tdPreviousConfiguration disabledFlags
      setTerminalAttributes stdInput newTermSettings Immediately

      tdWriteQueue <- newTQueueIO

      tdReader <- asyncBound readTerminal
      tdWriter <- asyncBound (writeTerminal tdWriteQueue)

      pure TermDrv{..}

    stop :: TermDrv -> IO ()
    stop (TermDrv{..}) = do
      -- cancel our threads
      cancel tdReader
      -- cancel tdWriter
      -- take the terminal out of raw mode
      setTerminalAttributes stdInput tdPreviousConfiguration Immediately

    -- Reads data from stdInput and emit the proper effect
    readTerminal :: IO ()
    readTerminal = forever $ do
      t <- try (fdRead stdInput 1)
      case t of
        Left (e :: IOException) ->
          -- Ignore EOFs when doing raw reads
          pure ()
        Right (str, bytes) -> do
          print ("[KEY] " ++ (show str))
          wen          <- Time.now
          pure ()

    handleEffect :: TermDrv -> TermEf -> IO ()
    handleEffect TermDrv{..} ef =
      print ("[TERM]" ++ (show ef))

    -- Writes data to the terminal. Both the terminal reading, normal logging,
    -- and effect handling can all emit bytes which go to the terminal.
    writeTerminal :: TQueue ByteString -> IO ()
    writeTerminal q = forever $ do
      x <- atomically $ readTQueue q
      pure ()
