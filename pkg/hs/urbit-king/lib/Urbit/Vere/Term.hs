{-|
    Terminal Driver
-}
module Urbit.Vere.Term
    ( module Term
    , localClient
    , connectToRemote
    , runTerminalClient
    , connClient
    , term
    , term'
    ) where

import Data.Char
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import RIO.FilePath
import System.Posix.IO
import System.Posix.Terminal
import Urbit.Arvo
import Urbit.King.App
import Urbit.Noun.Time
import Urbit.Prelude         hiding (getCurrentTime)
import Urbit.Vere.Pier.Types

import Data.List           ((!!))
import RIO.Directory       (createDirectoryIfMissing)
import Urbit.King.API      (readPortsFile)
import Urbit.TermSize      (TermSize(TermSize))
import Urbit.Vere.Term.API (Client(Client), ClientTake(..))

import qualified Data.Set                 as S
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.UTF8     as BS
import qualified System.Console.ANSI      as ANSI
import qualified Urbit.TermSize           as T
import qualified Urbit.Vere.NounServ      as Serv
import qualified Urbit.Vere.Term.API      as Term
import qualified Urbit.Vere.Term.Render   as T


-- Types -----------------------------------------------------------------------

-- | All stateful data in the printing to stdOutput.
data LineState = LineState
  { lsLine            :: Text
  , lsCurPos          :: Int
  , lsSpinTimer       :: Maybe (Async ())
  , lsSpinCause       :: Maybe Text
  , lsSpinFirstRender :: Bool
  , lsSpinFrame       :: Int
  , lsPrevEndTime     :: Wen
  }

-- | A record used in reading data from stdInput.
data ReadData = ReadData
  { rdBuf       :: Ptr Word8
  , rdEscape    :: Bool
  , rdBracket   :: Bool
  , rdUTF8      :: ByteString
  , rdUTF8width :: Int
  }

-- | Private data to the Client that we keep around for stop().
data Private = Private
  { pReaderThread          :: Async ()
  , pWriterThread          :: Async ()
  , pPreviousConfiguration :: TerminalAttributes
  }

-- Utils -----------------------------------------------------------------------

blewEvent :: Word -> Word -> Ev
blewEvent w h = EvBlip $ BlipEvTerm $ TermEvBlew (UD 1, ()) w h

initialHail :: Ev
initialHail = EvBlip $ BlipEvTerm $ TermEvHail (UD 1, ()) ()

-- Version one of this is punting on the ops_u.dem flag: whether we're running
-- in daemon mode.

--------------------------------------------------------------------------------

rioAllocaBytes :: (MonadIO m, MonadUnliftIO m)
               => Int -> (Ptr a -> m b) -> m b
rioAllocaBytes size action =
  withRunInIO $ \run ->
    allocaBytes size $ \x -> run (action x)

{-|
    Because of legacy reasons, some file operations are in the terminal
    driver. These should be filtered out and handled locally instead of
    in any abstractly connected terminal.
-}
isTerminalBlit :: Blit -> Bool
isTerminalBlit (Sav _ _) = False
isTerminalBlit (Sag _ _) = False
isTerminalBlit _         = True

--------------------------------------------------------------------------------

connClient :: Serv.Conn ClientTake [Term.Ev] -> Client
connClient c = Client
    { give = Serv.cSend c
    , take = Serv.cRecv c
    }

connectToRemote :: forall e. HasLogFunc e
                => Port
                -> Client
                -> RAcquire e (Async (), Async ())
connectToRemote port local = mkRAcquire start stop
  where
    stop (x, y) = cancel x >> cancel y
    start = do
        Serv.Client{..} <- Serv.wsClient "/terminal/0" (fromIntegral port)

        -- TODO XX Handle disconnect more cleanly.
        ferry <- async $ forever $ atomically $ asum
            [ Term.take local >>= \case
                  Nothing -> empty
                  Just ev -> Serv.cSend cConn ev
            , Serv.cRecv cConn >>= \case
                  Nothing -> empty
                  Just ev -> Term.give local ev
            ]

        pure (ferry, cAsync)

data HackConfigDir = HCD { _hcdPax :: FilePath }
makeLenses ''HackConfigDir
instance HasPierPath HackConfigDir where pierPathL = hcdPax

runTerminalClient :: forall e. HasLogFunc e => FilePath -> RIO e ()
runTerminalClient pier = runRAcquire $ do
    mPort      <- runRIO (HCD pier) readPortsFile
    port       <- maybe (error "Can't connect") pure mPort
    mExit      <- io newEmptyTMVarIO
    cli        <- localClient (putTMVar mExit ())
    (tid, sid) <- connectToRemote (Port $ fromIntegral port) cli
    atomically $ waitSTM tid <|> waitSTM sid <|> takeTMVar mExit

  where
    runRAcquire :: RAcquire e () -> RIO e ()
    runRAcquire act = rwith act $ const $ pure ()


-- Spinner ---------------------------------------------------------------------

-- Call an STM action after delay of `first` microseconds and then every
-- `rest` microseconds after that.
repeatedly :: Int -> Int -> STM () -> IO ()
repeatedly first rest action = do
  threadDelay first
  forever $ do
    atomically action
    threadDelay rest

spinners :: [Text]
spinners = ["|", "/", "-", "\\"]

leftBracket, rightBracket :: Text
leftBracket = "«"
rightBracket = "»"

_spin_cool_us, _spin_warm_us, _spin_rate_us, _spin_idle_us :: Integral i => i
_spin_cool_us = 500000
_spin_warm_us = 50000
_spin_rate_us = 250000
_spin_idle_us = 500000


-- Client ----------------------------------------------------------------------

{-|
    Initializes the generalized input/output parts of the terminal.
-}
localClient :: forall e. HasLogFunc e
            => STM ()
            -> RAcquire e Client
localClient doneSignal = fst <$> mkRAcquire start stop
  where
    start :: HasLogFunc e => RIO e (Client, Private)
    start = do
      tsWriteQueue  <- newTQueueIO :: RIO e (TQueue [Term.Ev])
      spinnerMVar   <- newEmptyTMVarIO :: RIO e (TMVar ())

      -- Track the terminal size, keeping track of the size of the local
      -- terminal for our own printing, as well as putting size changes into an
      -- event queue so we can send changes to the terminal muxing system.
      tsizeTVar    <- newTVarIO (TermSize 80 24) -- Value doesn't matter.
      tsSizeChange <- newEmptyTMVarIO
      io $ T.liveTermSize (\ts -> atomically $ do
                              -- We keep track of the console's local size for
                              -- our own tank washing.
                              writeTVar tsizeTVar ts

                              -- We queue up changes so we can broadcast them
                              -- to the muxing client.
                              putTMVar tsSizeChange ts)

      pWriterThread <- asyncBound
        (writeTerminal tsWriteQueue spinnerMVar tsizeTVar)

      pPreviousConfiguration <- io $ getTerminalAttributes stdInput

      -- Create a new configuration where we put the terminal in raw mode and
      -- disable a bunch of preprocessing.
      let newTermSettings = flip withTime     0
                          $ flip withMinInput 1
                          $ foldl' withoutMode pPreviousConfiguration
                          $ disabledFlags

      io $ setTerminalAttributes stdInput newTermSettings Immediately

      tsReadQueue <- newTQueueIO
      pReaderThread <- asyncBound
          (readTerminal tsReadQueue tsWriteQueue (bell tsWriteQueue))

      let client = Client { take = Just <$> asum
                              [ readTQueue tsReadQueue <&> ClientTakeBelt,
                                takeTMVar tsSizeChange <&> ClientTakeSize
                              ]
                          , give = writeTQueue tsWriteQueue
                          }

      pure (client, Private{..})

    stop :: HasLogFunc e
         => (Client, Private) -> RIO e ()
    stop (Client{..}, Private{..}) = do
      -- Note that we don't `cancel pReaderThread` here. This is a deliberate
      -- decision because fdRead calls into a native function which the runtime
      -- can't kill. If we were to cancel here, the internal `waitCatch` would
      -- block until the next piece of keyboard input. Since this only happens
      -- at shutdown, just leak the file descriptor.
      cancel pWriterThread

      -- inject one final newline, as we're usually on the prompt.
      putStr "\r\n"

      -- take the terminal out of raw mode
      io $ setTerminalAttributes stdInput pPreviousConfiguration Immediately

    {-
        A list of terminal flags that we disable.

        TODO: Terminal library missing CSIZE?
    -}
    disabledFlags :: [TerminalMode]
    disabledFlags = [ StartStopOutput
                    , KeyboardInterrupts
                    , EnableEcho
                    , EchoLF
                    , ProcessInput
                    , ExtendedFunctions
                    , MapCRtoLF
                    , CheckParity
                    , StripHighBit
                    , EnableParity
                    , ProcessOutput
                    ]


    -- Writes data to the terminal. Both the terminal reading, normal logging,
    -- and effect handling can all emit bytes which go to the terminal.
    writeTerminal :: TQueue [Term.Ev] -> TMVar () -> TVar TermSize -> RIO e ()
    writeTerminal q spinner termSizeVar = do
        currentTime <- io $ now
        loop (LineState "" 0 Nothing Nothing True 0 currentTime)
      where
        writeBlank :: LineState -> RIO e LineState
        writeBlank ls = putStr "\r\n" $> ls

        writeTrace :: LineState -> Text -> RIO e LineState
        writeTrace ls p = do
            putStr "\r"
            T.clearLine
            putStr p
            termRefreshLine ls

        writeSlog :: LineState -> (Atom, Tank) -> RIO e LineState
        writeSlog ls slog = do
            putStr "\r"
            T.clearLine
            TermSize width _ <- atomically $ readTVar termSizeVar
            -- TODO: Ignoring priority for now. Priority changes the color of,
            -- and adds a prefix of '>' to, the output.
            let lines = fmap unTape $ wash (WashCfg 0 width) $ tankTree $ snd slog
            forM lines $ \line -> putStr (line <> "\r\n")
            termRefreshLine ls

        {-
            Figure out how long to wait to show the spinner. When we
            don't have a vane name to display, we assume its a user
            action and trigger immediately. Otherwise, if we receive an
            event shortly after a previous spin, use a shorter delay to
            avoid giving the impression of a half-idle system.
        -}
        doSpin :: LineState -> Maybe Text -> RIO e LineState
        doSpin ls@LineState{..} mTxt = do
            maybe (pure ()) cancel lsSpinTimer

            current <- io $ now
            delay   <- pure $ case mTxt of
                Nothing -> 0
                Just _  ->
                  if (gap current lsPrevEndTime ^. microSecs) < _spin_idle_us
                  then _spin_warm_us
                  else _spin_cool_us

            spinTimer <- io $ async
                            $ repeatedly delay _spin_rate_us
                            $ void
                            $ tryPutTMVar spinner ()

            pure $ ls { lsSpinTimer       = Just spinTimer
                      , lsSpinCause       = mTxt
                      , lsSpinFirstRender = True
                      }

        unspin :: LineState -> RIO e LineState
        unspin ls@LineState{..} = do
              maybe (pure ()) cancel lsSpinTimer
              -- We do a final flush of the spinner mvar to ensure we don't
              -- have a lingering signal which will redisplay the spinner after
              -- we call termRefreshLine below.
              atomically $ tryTakeTMVar spinner

              -- If we ever actually ran the spinner display callback, we need
              -- to force a redisplay of the command prompt.
              ls <- if not lsSpinFirstRender || True
                    then termRefreshLine ls
                    else pure ls

              endTime <- io $ now
              pure $ ls { lsSpinTimer = Nothing, lsPrevEndTime = endTime }

        execEv :: LineState -> Term.Ev -> RIO e LineState
        execEv ls = \case
            Term.Blits bs         -> foldM writeBlit ls bs
            Term.Trace p          -> writeTrace ls (unCord p)
            Term.Slog s           -> writeSlog ls s
            Term.Blank            -> writeBlank ls
            Term.Spinr (Just txt) -> doSpin ls (unCord <$> txt)
            Term.Spinr Nothing    -> unspin ls

        --  TODO What does this do?
        spin :: LineState -> RIO e LineState
        spin ls@LineState{..} = do
            let spinner = (spinners !! lsSpinFrame) ++ case lsSpinCause of
                  Nothing  -> ""
                  Just str -> leftBracket ++ str ++ rightBracket

            putStr (spinner <> pack (ANSI.cursorBackwardCode (length spinner)))

            let newFrame = (lsSpinFrame + 1) `mod` length spinners

            pure $ ls { lsSpinFirstRender = False
                      , lsSpinFrame       = newFrame
                      }

        loop :: LineState -> RIO e ()
        loop ls = do
            join $ atomically $ asum
                [ readTQueue q      >>= pure . (foldM execEv ls >=> loop)
                , takeTMVar spinner >>  pure (spin ls >>= loop)
                ]

    -- Writes an individual blit to the screen
    writeBlit :: LineState -> Blit -> RIO e LineState
    writeBlit ls = \case
      Bel ()        -> T.soundBell $> ls
      Clr ()        -> do T.clearScreen
                          termRefreshLine ls
      Hop w         -> termShowCursor ls (fromIntegral w)
      Klr s         -> do ls2 <- termShowClear ls
                          termShowStub ls2 s
      Lin c         -> do ls2 <- termShowClear ls
                          termShowLine ls2 (pack c)
      Mor ()        -> termShowMore ls
      Sag path noun -> pure ls
      Sav path atom -> pure ls
      Url url       -> pure ls

    termRenderDeco :: Deco -> Char
    termRenderDeco = \case
      DecoBr   -> '1'
      DecoUn   -> '4'
      DecoBl   -> '5'
      DecoNull -> '0'

    termRenderTint :: Tint -> Char
    termRenderTint = \case
      TintK    -> '0'
      TintR    -> '1'
      TintG    -> '2'
      TintY    -> '3'
      TintB    -> '4'
      TintM    -> '5'
      TintC    -> '6'
      TintW    -> '7'
      TintNull -> '9'

    -- Wraps the appropriate escape sequence around a piece of styled text
    termRenderStubSegment :: Stye -> [Char] -> [Char]
    termRenderStubSegment Stye {..} tape =
        case (S.null decoset, back, fore) of
          (True, TintNull, TintNull) -> tape
          _                          -> styled
      where
        decoset = setFromHoonSet deco
        escape  = [chr 27, '[']

        styles = intercalate ";" $ filter (not . null)
          [ intersperse ';' $ fmap termRenderDeco $ toList decoset
          , case back of
              TintNull -> []
              tint     -> ['4', termRenderTint tint]
          , case fore of
              TintNull -> []
              tint     -> ['3', termRenderTint tint]
          ]

        styled = mconcat [escape, styles, "m", tape, escape, "0m"]

    -- Displays and sets styled text as the current line
    termShowStub :: LineState -> Stub -> RIO e LineState
    termShowStub ls (Stub s) = do
      let visualLength = sum $ fmap (length . snd) s
      let outText = pack $ mconcat $ fmap (uncurry termRenderStubSegment) s
      putStr outText
      pure ls { lsLine = outText, lsCurPos = visualLength }

    -- Moves the cursor to the requested position
    termShowCursor :: LineState -> Int -> RIO e LineState
    termShowCursor ls@LineState{..} {-line pos)-} newPos = do
      if newPos < lsCurPos then do
        T.cursorLeft (lsCurPos - newPos)
        pure ls { lsCurPos =  newPos }
      else if newPos > lsCurPos then do
        T.cursorRight (newPos - lsCurPos)
        pure ls { lsCurPos =  newPos }
      else
        pure ls

    -- Moves the cursor left without any mutation of the LineState. Used only
    -- in cursor spinning.
    _termSpinnerMoveLeft :: Int -> RIO e ()
    _termSpinnerMoveLeft = T.cursorLeft

    -- Displays and sets the current line
    termShowLine :: LineState -> Text -> RIO e LineState
    termShowLine ls newStr = do
      putStr newStr
      pure ls { lsLine = newStr, lsCurPos = (length newStr) }

    termShowClear :: LineState -> RIO e LineState
    termShowClear ls = do
        putStr "\r"
        T.clearLine
        pure ls { lsLine = "", lsCurPos = 0  }

    -- New Current Line
    termShowMore :: LineState -> RIO e LineState
    termShowMore ls = do
      putStr "\r\n"
      pure ls { lsLine = "", lsCurPos = 0  }

    -- Redraw the current LineState, maintaining the current curpos
    termRefreshLine :: LineState -> RIO e LineState
    termRefreshLine ls@LineState{lsCurPos,lsLine} = do
      ls <- termShowClear ls
      ls <- termShowLine ls lsLine
      termShowCursor ls lsCurPos

    -- ring my bell
    bell :: TQueue [Term.Ev] -> RIO e ()
    bell q = atomically $ writeTQueue q $ [Term.Blits [Bel ()]]

    -- Reads data from stdInput and emit the proper effect
    --
    -- This entire path is a divergence from how term.c does things,
    -- probably. First, the vtime is 0, not 1 in term.c. So (IIUC), we'll
    -- always have a latency of 1/10 of a second.
    --
    -- A better way to do this would be to get some sort of epoll on stdInput,
    -- since that's kinda closer to what libuv does?
    readTerminal :: forall e. HasLogFunc e
                 => TQueue Belt -> TQueue [Term.Ev] -> (RIO e ()) -> RIO e ()
    readTerminal rq wq bell =
      rioAllocaBytes 1 $ \ buf -> loop (ReadData buf False False mempty 0)
      where
        loop :: ReadData -> RIO e ()
        loop rd@ReadData{..} = do
          -- The problem with using fdRead raw is that it will text encode
          -- things like \ESC instead of 27. That makes it broken for our
          -- purposes.
          --
          io (try $ fdReadBuf stdInput rdBuf 1) >>= \case
            Left (e :: IOException) -> do
              -- Ignore EAGAINs when doing reads
              loop rd
            Right 0 -> loop rd
            Right _ -> do
              w   <- io $ peek rdBuf
              -- print ("{" ++ (show w) ++ "}")
              let c = BS.w2c w
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
              else if rdUTF8width /= 0 then do
                -- continue reading into the utf8 accumulation buffer
                rd@ReadData{..} <- pure rd { rdUTF8 = snoc rdUTF8 w }
                if length rdUTF8 /= rdUTF8width then loop rd
                else do
                  case BS.decode rdUTF8 of
                    Nothing ->
                      error "empty utf8 accumulation buffer"
                    Just (c, bytes) | bytes /= rdUTF8width ->
                      error "utf8 character size mismatch?!"
                    Just (c, bytes) -> sendBelt $ Txt $ Tour $ [c]
                  loop rd { rdUTF8 = mempty, rdUTF8width = 0 }
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
                logInfo $ displayShow "Ctrl-c interrupt"
                atomically $ do
                  writeTQueue wq [Term.Trace "interrupt\r\n"]
                  writeTQueue rq $ Ctl $ Cord "c"
                loop rd
              else if w <= 26 then do
                case pack [BS.w2c (w + 97 - 1)] of
                    "d" -> atomically doneSignal
                    c   -> do sendBelt $ Ctl $ Cord c
                              loop rd
              else if w == 27 then do
                loop rd { rdEscape = True }
              else do
                -- start the utf8 accumulation buffer
                loop rd { rdUTF8 = singleton w,
                          rdUTF8width = if w < 224 then 2
                                        else if w < 240 then 3
                                        else 4 }

        sendBelt :: HasLogFunc e => Belt -> RIO e ()
        sendBelt b = do
          -- logDebug $ displayShow ("terminalBelt", b)
          atomically $ writeTQueue rq b


--------------------------------------------------------------------------------

{-|
  Terminal Driver

  Until blew/hail events succeeds, ignore effects.
  Wait until blew/hail event callbacks invoked.
    If success, signal success.
    If failure, try again several times.
      If still failure, bring down ship.
   Don't wait for other drivers to boot
   Begin normal operation (start accepting requests)
-}
term'
  :: HasPierEnv e
  => (TermSize, Client)
  -> IO ()
  -> RIO e ([Ev], RAcquire e (DriverApi TermEf))
term' (tsize, client) serfSIGINT = do
  let TermSize wi hi = tsize
      initEv = [blewEvent wi hi, initialHail]

  pure (initEv, runDriver)
 where
  runDriver = do
    env <- ask
    ventQ :: TQueue EvErr <- newTQueueIO
    diOnEffect <- term env (tsize, client) (writeTQueue ventQ) serfSIGINT

    let diEventSource = fmap RRWork <$> tryReadTQueue ventQ

    pure (DriverApi {..})

{-|
    Terminal Driver
-}
term :: forall e. (HasPierEnv e)
     => e
     -> (TermSize, Client)
     -> (EvErr -> STM ())
     -> IO ()
     -> RAcquire e (TermEf -> IO ())
term env (tsize, Client{..}) plan serfSIGINT = runTerm
  where
    runTerm :: RAcquire e (TermEf -> IO ())
    runTerm = do
      tim <- mkRAcquire (async readLoop) cancel
      pure (runRIO env . handleEffect)

    {-
        Because our terminals are always `Demux`ed, we don't have to
        care about disconnections.
    -}
    readLoop :: RIO e ()
    readLoop = forever $ do
        atomically take >>= \case
            Nothing                              -> pure ()
            Just (ClientTakeBelt b)              -> do
                when (b == Ctl (Cord "c")) $ do
                  io serfSIGINT
                let beltEv       = EvBlip $ BlipEvTerm $ TermEvBelt (UD 1, ()) $ b
                let beltFailed _ = pure ()
                atomically $ plan (EvErr beltEv beltFailed)
            Just (ClientTakeSize ts@(TermSize w h)) -> do
                let blewFailed _ = pure ()
                atomically $ plan (EvErr (blewEvent w h) blewFailed)

    handleEffect :: TermEf -> RIO e ()
    handleEffect = \case
        TermEfInit _ _     -> pure ()
        TermEfMass _ _     -> pure ()
        TermEfLogo _ _     -> atomically =<< view killPierActionL
        TermEfBlit _ blits -> do
            let (termBlits, fsWrites) = partition isTerminalBlit blits
            atomically $ give [Term.Blits termBlits]
            for_ fsWrites handleFsWrite

    handleFsWrite :: Blit -> RIO e ()
    handleFsWrite (Sag path noun) = performPut path (jamBS noun)
    handleFsWrite (Sav path atom) = performPut path (atomBytes atom)
    handleFsWrite _               = pure ()

    performPut :: Path -> ByteString -> RIO e ()
    performPut path bs = do
      pierPath <- view pierPathL
      let putOutFile = pierPath </> ".urb" </> "put" </> (pathToFilePath path)
      createDirectoryIfMissing True (takeDirectory putOutFile)
      writeFile putOutFile bs
