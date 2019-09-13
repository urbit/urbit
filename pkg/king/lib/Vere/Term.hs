{-
    TODO Need somewhere to write out the ports for the remote-terminal
         interface.
    TODO Implement the client side of the remote terminal interface.
-}

module Vere.Term (initializeLocalTerminal, term, TerminalSystem(..)) where

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
import RIO.Directory   (createDirectoryIfMissing)
import RIO.FilePath

import Data.ByteString.Internal

import qualified Vere.NounServ                as Serv
import qualified System.Console.Terminal.Size as Term


-- Terminal Size ---------------------------------------------------------------

data TermSize = TermSize { tsHeight :: Word, tsWidth :: Word }
  deriving (Eq, Ord, Show)

tsFromWindow :: Term.Window Word -> TermSize
tsFromWindow (Term.Window h w) = TermSize h w

deriveNoun ''TermSize


-- Types -----------------------------------------------------------------------

-- Output to the attached terminal is either a series of vere blits, or it is an
-- injected printf line from the interpreter.
data VereOutput = VereBlitOutput [Blit]
                | VerePrintOutput String
                | VereBlankLine
  deriving Show

deriveNoun ''VereOutput

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
data TerminalSystem e = TerminalSystem
  { tsReadFromTerm :: STM Belt
  , tsWriteToTerm  :: VereOutput -> STM ()
  , tsStderr       :: Text -> RIO e ()
  , tsSize         :: TermSize
  }

-- Private data to the TerminalSystem that we keep around for stop().
data Private = Private
  { pReaderThread :: Async ()
  , pWriterThread :: Async ()
  , pPreviousConfiguration ::  TerminalAttributes
  }

-- Utils -----------------------------------------------------------------------

initialBlew w h = EvBlip $ BlipEvTerm $ TermEvBlew (UD 1, ()) w h

initialHail = EvBlip $ BlipEvTerm $ TermEvHail (UD 1, ()) ()

-- Version one of this is punting on the ops_u.dem flag: whether we're running
-- in daemon mode.

--------------------------------------------------------------------------------

runMaybeTermOutput :: Terminal -> (Terminal -> Maybe TermOutput) -> RIO e ()
runMaybeTermOutput t getter = case (getter t) of
  Nothing -> pure ()
  Just x -> io $ runTermOutput t x

rioAllocaBytes :: (MonadIO m, MonadUnliftIO m)
               => Int -> (Ptr a -> m b) -> m b
rioAllocaBytes size action =
  withRunInIO $ \run ->
    allocaBytes size $ \x -> run (action x)

-- Because of legacy reasons, some file operations are in the terminal
-- driver. These should be filtered out and handled locally instead of in any
-- abstractly connected terminal.
isTerminalBlit :: Blit -> Bool
isTerminalBlit (Sav _ _) = False
isTerminalBlit (Sag _ _) = False
isTerminalBlit _         = True

--------------------------------------------------------------------------------

data RemoteTerminalServer e = RTS
    { rtsListen :: STM (Maybe (TerminalSystem e))
    , rtsAsync  :: Async ()
    , rtsNServ  :: Serv.Server (Either TermSize Belt) VereOutput Int
    , rtsActive :: TVar [Void]
    }

{-
    TODO This has a problem where a new connection that never sends any
         data will prevent more connections from being handled. Rethink
         approach.

    TODO We should close the websocket connection if we don't receive
         a size, but I forgot to expose that from the NounServ interface.
-}
_remoteTerminalServer :: ∀e. HasLogFunc e
                      => RAcquire e (RemoteTerminalServer e)
_remoteTerminalServer = mkRAcquire start stop
  where
    start = do
        rtsNServ  <- Serv.wsServer
        rtsActive <- newTVarIO mempty
        newTerms  <- io $ newTBMChanIO 5
        rtsListen <- pure $ readTBMChan newTerms

        let servLoop :: RIO e ()
            servLoop = do
                atomically (Serv.sAccept rtsNServ) >>= \case
                    Nothing -> atomically $ closeTBMChan newTerms
                    Just c -> do
                        logInfo "Got a new terminal connection"
                        atomically (getInitialTermSize c) >>= \case
                            Left err -> logError (display err)
                            Right sz ->
                                atomically $ writeTBMChan newTerms
                                           $ mkTermSys c sz
                        servLoop

        rtsAsync <- async servLoop

        pure RTS{..}

    getInitialTermSize :: Serv.Conn (Either TermSize Belt) VereOutput
                       -> STM (Either Text TermSize)
    getInitialTermSize c = Serv.cRecv c <&> \case
        Nothing        -> Left noData
        Just (Right _) -> Left noSize
        Just (Left sz) -> Right sz
      where
        noData = "Terminal connection closed with no data"
        noSize = "Expected first packet to be term size, but got belt"

    bogusBelt :: Belt
    bogusBelt = Txt ""

    mkTermSys :: Serv.Conn (Either TermSize Belt) VereOutput
              -> TermSize
              -> TerminalSystem e
    mkTermSys Serv.Conn{..} tsSize = TerminalSystem{..}
      where
        -- TODO HACK: API should support terminal disconnect and size change.
        tsReadFromTerm = cRecv <&> \case
            Nothing        -> bogusBelt
            Just (Left _)  -> bogusBelt
            Just (Right b) -> b

        tsWriteToTerm = cSend

        -- TODO What to do here?
        tsStderr t = pure ()

    stop RTS{..} = do
      cancel rtsAsync
      -- acts <- atomically (readTVar rtsActive)
      -- for_ acts (cancel . Serv.sAsync)

--------------------------------------------------------------------------------

-- Initializes the generalized input/output parts of the terminal.
--
initializeLocalTerminal :: HasLogFunc e => RAcquire e (TerminalSystem e)
initializeLocalTerminal = do
    (a, b) <- mkRAcquire start stop
    pure a
  where
    start :: HasLogFunc e => RIO e (TerminalSystem e, Private)
    start = do
      --  Initialize the writing side of the terminal
      --
      t <- io setupTermFromEnv

      tsWriteQueue <- newTQueueIO
      pWriterThread <- asyncBound (writeTerminal t tsWriteQueue)

      pPreviousConfiguration <- io $ getTerminalAttributes stdInput

      -- Create a new configuration where we put the terminal in raw mode and
      -- disable a bunch of preprocessing.
      let newTermSettings = flip withTime 0
                          $ flip withMinInput 1
                          $ foldl' withoutMode pPreviousConfiguration
                          $ disabledFlags

      io $ setTerminalAttributes stdInput newTermSettings Immediately

      tsReadQueue   <- newTQueueIO
      pReaderThread <- asyncBound $ readTerminal tsReadQueue tsWriteQueue
                                  $ bell tsWriteQueue

      let tsStderr = \txt ->
            atomically $ writeTQueue tsWriteQueue $ VerePrintOutput $ unpack txt

      tsSize <- io $ Term.size <&> tsFromWindow . fromMaybe (Term.Window 80 24)

      let tsReadFromTerm = readTQueue tsReadQueue
          tsWriteToTerm  = writeTQueue tsWriteQueue

      pure (TerminalSystem{..}, Private{..})

    stop :: HasLogFunc e
         => (TerminalSystem e, Private) -> RIO e ()
    stop (TerminalSystem{..}, Private{..}) = do
      -- Note that we don't `cancel pReaderThread` here. This is a deliberate
      -- decision because fdRead calls into a native function which the runtime
      -- can't kill. If we were to cancel here, the internal `waitCatch` would
      -- block until the next piece of keyboard input. Since this only happens
      -- at shutdown, just leak the file descriptor.

      cancel pWriterThread
      -- take the terminal out of raw mode
      io $ setTerminalAttributes stdInput pPreviousConfiguration Immediately

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
    writeTerminal :: Terminal -> TQueue VereOutput -> RIO e ()
    writeTerminal t q = loop (LineState "" 0)
      where
        loop s = do
          x <- atomically $ readTQueue q
          case x of
            VereBlitOutput blits -> do
              s <- foldM (writeBlit t) s blits
              loop s
            VerePrintOutput p -> do
              io $ runTermOutput t $ termText "\r"
              runMaybeTermOutput t vtClearToBegin
              io $ runTermOutput t $ termText p
              s <- termRefreshLine t s
              loop s
            VereBlankLine -> do
              io $ runTermOutput t $ termText "\r\n"
              loop s

    -- Writes an individual blit to the screen
    writeBlit :: Terminal -> LineState -> Blit -> RIO e LineState
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
    termShowCursor :: Terminal -> LineState -> Int -> RIO e LineState
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
    termShowLine :: Terminal -> LineState -> String -> RIO e LineState
    termShowLine t ls newStr = do
      -- TODO: Really think about how term.c munged cus_w. Amidoinitrit?
      io $ runTermOutput t $ termText newStr
      pure (LineState newStr (length newStr))

    termShowClear :: Terminal -> LineState -> RIO e LineState
    termShowClear t ls = do
      io $ runTermOutput t $ termText "\r"
      runMaybeTermOutput t vtClearToBegin
      pure (LineState "" 0)

    -- New Current Line
    termShowMore :: Terminal -> LineState -> RIO e LineState
    termShowMore t ls = do
      io $ runTermOutput t $ termText "\r\n"
      pure (LineState "" 0)

    -- Redraw the current LineState, moving cursor to the end.
    termRefreshLine :: Terminal -> LineState -> RIO e LineState
    termRefreshLine t ls@(LineState line pos) = do
      runMaybeTermOutput t vtClearToBegin
      newLs <- termShowLine t ls line
      termShowCursor t newLs pos

    -- ring my bell
    bell :: TQueue VereOutput -> RIO e ()
    bell q = atomically $ writeTQueue q $ VereBlitOutput [Bel ()]

    -- Reads data from stdInput and emit the proper effect
    --
    -- This entire path is a divergence from how term.c does things,
    -- probably. First, the vtime is 0, not 1 in term.c. So (IIUC), we'll
    -- always have a latency of 1/10 of a second.
    --
    -- A better way to do this would be to get some sort of epoll on stdInput,
    -- since that's kinda closer to what libuv does?
    readTerminal :: forall e. HasLogFunc e
                 => TQueue Belt -> TQueue VereOutput -> (RIO e ()) -> RIO e ()
    readTerminal rq wq bell =
      rioAllocaBytes 1 $ \ buf -> loop (ReadData buf False False)
      where
        loop :: ReadData -> RIO e ()
        loop rd@ReadData{..} = do
          -- The problem with using fdRead raw is that it will text
          -- encode things like \ESC instead of 27. That makes it broken
          -- for our purposes.
          t <- io $ try (fdReadBuf stdInput rdBuf 1)
          case t of
            Left (e :: IOException) -> do
              -- Ignore EAGAINs when doing reads
              loop rd
            Right 0 -> loop rd
            Right _ -> do
              w   <- io $ peek rdBuf
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
                logDebug $ displayShow "Ctrl-c interrupt"
                atomically $ do
                  writeTQueue wq $ VerePrintOutput "interrupt\r\n"
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

        sendBelt :: HasLogFunc e => Belt -> RIO e ()
        sendBelt b = do
          logDebug $ displayShow ("terminalBelt", b)
          atomically $ writeTQueue rq b

--------------------------------------------------------------------------------

term :: forall e. HasLogFunc e
     => TerminalSystem e -> (STM ()) -> FilePath -> KingId -> QueueEv
     -> ([Ev], RAcquire e (EffCb e TermEf))
term TerminalSystem{..} shutdownSTM pierPath king enqueueEv =
    (initialEvents, runTerm)
  where
    TermSize hig wit = tsSize

    initialEvents = [initialBlew hig wit, initialHail]

    runTerm :: RAcquire e (EffCb e TermEf)
    runTerm = do
      tim <- mkRAcquire start stop
      pure handleEffect

    start :: RIO e (Async ())
    start = async readBelt

    stop :: Async () -> RIO e ()
    stop rb = cancel rb

    readBelt :: RIO e ()
    readBelt = forever $ atomically $ do
      b <- tsReadFromTerm
      enqueueEv $ EvBlip $ BlipEvTerm $ TermEvBelt (UD 1, ()) b

    handleEffect :: TermEf -> RIO e ()
    handleEffect = \case
      TermEfInit _ _     -> pure ()
      TermEfMass _ _     -> pure ()
      TermEfBlit _ blits -> do
        let (termBlits, fsWrites) = partition isTerminalBlit blits
        atomically $ tsWriteToTerm $ VereBlitOutput termBlits
        for_ fsWrites handleFsWrite

      TermEfLogo path _ -> do
        atomically $ shutdownSTM

    handleFsWrite :: Blit -> RIO e ()
    handleFsWrite (Sag path noun) = performPut path (jamBS noun)
    handleFsWrite (Sav path atom) = pure () --performPut path atom
    handleFsWrite _ = pure ()

    performPut :: Path -> ByteString -> RIO e ()
    performPut path bs = do
      let putOutFile = pierPath </> ".urb" </> "put" </> (pathToFilePath path)
      createDirectoryIfMissing True (takeDirectory putOutFile)
      writeFile putOutFile bs
