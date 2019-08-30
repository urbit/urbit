{-
    TODO Handle CTRL-D
-}

module EventBrowser (run) where

import UrbitPrelude

import Arvo
import Data.Conduit
import Urbit.Time
import Vere.Pier.Types

import Control.Monad.Trans.Maybe (MaybeT(..))

import Vere.Log  (EventLog)

import qualified Data.Conduit.Combinators as C
import qualified Vere.Log                 as Log

--------------------------------------------------------------------------------

data Event = Event
    { num :: Word64
    , mug :: Mug
    , wen :: Wen
    , ova :: Ev
    }
  deriving Show

data Input = Next | Prev | Quit | Trim | Effs | Init | Last

--------------------------------------------------------------------------------

run :: HasLogFunc e => EventLog -> RIO e ()
run log = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    logInfo $ displayShow (Log.identity log)
    let cycle = fromIntegral $ lifecycleLen $ Log.identity log
    las <- Log.lastEv log
    loop cycle las las
  where
    failRead cur =
        putStrLn ("ERROR: Failed to read event: " <> tshow cur)

    input cyc las cur mFx = do
        getInput las cur >>= \case
            Next -> loop cyc las (succ cur)
            Prev -> loop cyc las (pred cur)
            Init -> loop cyc las 1
            Last -> loop cyc las las
            Quit -> pure ()
            Trim -> trim cyc las cur mFx
            Effs -> showEffects mFx >> input cyc las cur mFx

    trim cyc las cur mFx = do
        deleteFrom log las cur >>= \case
            True  -> loop cyc (pred cur) (pred cur)
            False -> input cyc las cur mFx

    loop cyc las 0                = loop cyc las 1
    loop cyc las cur | cur > las  = loop cyc las las
    loop cyc las cur | cyc >= cur = do
        putStrLn ""
        putStrLn "    [EVENT]"
        putStrLn ""
        putStrLn "    Lifecycle Nock"
        putStrLn ""
        input cyc las cur (Just [])

    loop cyc las cur = do
        mEv <- peekEvent  log cur
        mFx <- peekEffect log cur

        case mEv of
            Nothing -> failRead cur
            Just ev -> showEvent ev >> showEffectsTeaser mFx

        input cyc las cur mFx

deleteFrom :: HasLogFunc e => EventLog -> Word64 -> Word64 -> RIO e Bool
deleteFrom log las cur = do
    sure <- areYouSure
    if sure then doDelete else abortDelete
    pure sure
  where
    abortDelete = do
        putStrLn "\n\n    [ABORTED]\n"
        putStrLn "    Aborted delete, no events pruned.\n"

    doDelete = do
        Log.trimEvents log cur
        putStrLn "\n\n    [DELETED]\n"
        putStrLn "    It's gone forever!\n"

    question =
      if las == cur
      then mconcat [ "    This will permanently delete the last event (#"
                   , tshow las
                   , ")\n" ]
      else mconcat [ "    This will permanently delete all events in (#"
                   , tshow cur
                   , " - #"
                   , tshow las
                   , ")\n" ]

    areYouSure = do
        putStrLn "\n\n    ARE YOU SURE????"
        putStrLn ""
        putStrLn question
        putStr "(y|n) "
        hFlush stdout
        getChar <&> \case
          'y' -> True
          _   -> False

getInput :: Word64 -> Word64 -> RIO e Input
getInput las cur = do
    putStr ("(" <> tshow cur <> "/" <> tshow las <> ") ")
    hFlush stdout
    getChar >>= \case
        'j'    -> pure Next
        'k'    -> pure Prev
        'q'    -> pure Quit
        'f'    -> pure Effs
        'x'    -> pure Trim
        '0'    -> pure Init
        'G'    -> pure Last
        _      -> do putStrLn "\n"
                     putStrLn help
                     getInput las cur
  where
    help = unlines
       [ "    [HELP]"
       , ""
       , "    k    View the previous event"
       , "    j    View the next event"
       , "    0    View the first event"
       , "    G    View the last event"
       , "    q    Quit"
       , "    x    Delete (only the last event)"
       , "    ?    Show this help"
       ]

showEffectsTeaser :: Maybe FX -> RIO e ()
showEffectsTeaser Nothing   = putStrLn "    [No collected effects]\n"
showEffectsTeaser (Just []) = putStrLn "    [No effects for this event]\n"
showEffectsTeaser (Just fx) = putStrLn $ mconcat
    [ "    ["
    , tshow (length fx)
    , " collected effects. Press 'f' to view]\n"
    ]

showEffects :: Maybe FX -> RIO e ()
showEffects Nothing   = putStrLn "    [No collected effects]\n"
showEffects (Just []) = putStrLn "    [No effects for this event]\n"
showEffects (Just fx) = do
    putStrLn "\n"
    putStrLn "    [EFFECTS]"
    for_ fx $ \ef -> do
        putStrLn ""
        showEffect ef
    putStrLn ""

showEffect :: Lenient Ef -> RIO e ()
showEffect (GoodParse ef) =
    putStrLn $ unlines $ fmap ("    " <>) $ lines $ pack $ ppShow ef
showEffect (FailParse n) =
    putStrLn $ unlines $ fmap ("    " <>) $ lines $ pack $ ppShow n

showEvent :: Event -> RIO e ()
showEvent ev = do
    putStrLn "\n"
    putStrLn "    [EVENT]"
    putStrLn ""
    putStrLn $ unlines $ fmap ("    " <>) $ lines $ pack $ ppShow (ova ev)

peekEffect :: HasLogFunc e => EventLog -> Word64 -> RIO e (Maybe FX)
peekEffect log eId = runMaybeT $ do
   (id, bs) <- MaybeT $ runConduit (Log.streamEffectsRows log eId .| C.head)
   guard (id == eId)
   io $ cueBSExn bs >>= fromNounExn

peekEvent :: HasLogFunc e => EventLog -> Word64 -> RIO e (Maybe Event)
peekEvent log eId = runMaybeT $ do
    octs    <- MaybeT $ runConduit (Log.streamEvents log eId .| C.head)
    noun    <- io $ cueBSExn octs
    (m,w,e) <- io $ fromNounExn noun
    ovum    <- fromNounExn e
    pure (Event eId m w ovum)
