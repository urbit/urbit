{-# language DeriveGeneric #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
{-# language ScopedTypeVariables #-}

{- |
A progress bar in the terminal.

A progress bar conveys the progress of a task. Use a progress bar to
provide a visual cue that processing is underway.
-}
module System.ProgressBar
    ( -- * Getting started
      -- $start

      -- * Example
      -- $example

      -- * Progress bars
      ProgressBar
    , newProgressBar
    , hNewProgressBar
    , renderProgressBar
    , updateProgress
    , incProgress
      -- * Options
    , Style(..)
    , EscapeCode
    , OnComplete(..)
    , defStyle
    , ProgressBarWidth(..)
      -- * Progress
    , Progress(..)
      -- * Labels
    , Label(..)
    , Timing(..)
    , msg
    , percentage
    , exact
    , elapsedTime
    , remainingTime
    , totalTime
    , renderDuration
    ) where

import "base" Control.Concurrent.MVar ( MVar, newMVar, modifyMVar_)
import "base" Control.Monad ( when )
import "base" Data.Int       ( Int64 )
import "base" Data.Monoid    ( Monoid, mempty )
import "base" Data.Ratio     ( Ratio, (%) )
import "base" Data.Semigroup ( Semigroup, (<>) )
import "base" Data.String    ( IsString, fromString )
import "base" GHC.Generics   ( Generic )
import "base" System.IO      ( Handle, stderr, hFlush )
import "deepseq" Control.DeepSeq ( NFData, rnf )
import qualified "terminal-size" System.Console.Terminal.Size as TS
import qualified "text" Data.Text.Lazy             as TL
import qualified "text" Data.Text.Lazy.Builder     as TLB
import qualified "text" Data.Text.Lazy.Builder.Int as TLB
import qualified "text" Data.Text.Lazy.IO          as TL
import "time" Data.Time.Clock ( UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime )

--------------------------------------------------------------------------------

-- | A terminal progress bar.
--
-- A 'ProgressBar' value contains the state of a progress bar.
--
-- Create a progress bar with 'newProgressBar' or 'hNewProgressBar'.
-- Update a progress bar with 'updateProgress' or 'incProgress'.
data ProgressBar s
   = ProgressBar
     { pbStyle :: !(Style s)
     , pbStateMv :: !(MVar (State s))
     , pbRefreshDelay :: !Double
     , pbStartTime :: !UTCTime
     , pbHandle :: !Handle
     }

instance (NFData s) => NFData (ProgressBar s) where
    rnf pb =  pbStyle pb
        `seq` pbStateMv pb
        `seq` pbRefreshDelay pb
        `seq` pbStartTime pb
        -- pbHandle is ignored
        `seq` ()

-- | State of a progress bar.
data State s
   = State
     { stProgress :: !(Progress s)
       -- ^ Current progress.
     , stRenderTime :: !UTCTime
       -- ^ Moment in time of last render.
     }

-- | An amount of progress.
data Progress s
   = Progress
     { progressDone :: !Int
       -- ^ Amount of work completed.
     , progressTodo :: !Int
       -- ^ Total amount of work.
     , progressCustom :: !s
       -- ^ A value which is used by custom labels. The builtin labels
       -- do not care about this field. You can ignore it by using the
       -- unit value '()'.
     }

progressFinished :: Progress s -> Bool
progressFinished p = progressDone p >= progressTodo p

-- | Creates a progress bar.
--
-- The progress bar is drawn immediately. Update the progress bar with
-- 'updateProgress' or 'incProgress'. Do not output anything to your
-- terminal between updates. It will mess up the animation.
--
-- The progress bar is written to 'stderr'. Write to another handle
-- with 'hNewProgressBar'.
newProgressBar
    :: Style s -- ^ Visual style of the progress bar.
    -> Double -- ^ Maximum refresh rate in Hertz.
    -> Progress s -- ^ Initial progress.
    -> IO (ProgressBar s)
newProgressBar = hNewProgressBar stderr

-- | Creates a progress bar which outputs to the given handle.
--
-- See 'newProgressBar'.
hNewProgressBar
    :: Handle
       -- ^ File handle on which the progress bar is drawn. Usually
       -- you select a standard stream like 'stderr' or 'stdout'.
    -> Style s -- ^ Visual style of the progress bar.
    -> Double -- ^ Maximum refresh rate in Hertz.
    -> Progress s -- ^ Initial progress.
    -> IO (ProgressBar s)
hNewProgressBar hndl style maxRefreshRate initProgress = do
    style' <- updateWidth style

    startTime <- getCurrentTime
    hPutProgressBar hndl style' initProgress (Timing startTime startTime)

    stateMv <- newMVar
      State
      { stProgress   = initProgress
      , stRenderTime = startTime
      }
    pure ProgressBar
         { pbStyle = style'
         , pbStateMv = stateMv
         , pbRefreshDelay = recip maxRefreshRate
         , pbStartTime = startTime
         , pbHandle = hndl
         }

-- | Update the width based on the current terminal.
updateWidth :: Style s -> IO (Style s)
updateWidth style =
    case styleWidth style of
      ConstantWidth {} -> pure style
      TerminalWidth {} -> do
        mbWindow <- TS.size
        pure $ case mbWindow of
          Nothing -> style
          Just window -> style{ styleWidth = TerminalWidth (TS.width window) }

-- | Change the progress of a progress bar.
--
-- This function is thread safe. Multiple threads may update a single
-- progress bar at the same time.
--
-- There is a maximum refresh rate. This means that some updates might not be drawn.
updateProgress
    :: forall s
     . ProgressBar s -- ^ Progress bar to update.
    -> (Progress s -> Progress s) -- ^ Function to change the progress.
    -> IO ()
updateProgress progressBar f = do
    updateTime <- getCurrentTime
    modifyMVar_ (pbStateMv progressBar) $ renderAndUpdate updateTime
  where
    renderAndUpdate :: UTCTime -> State s -> IO (State s)
    renderAndUpdate updateTime state = do
        when shouldRender $
          hPutProgressBar hndl (pbStyle progressBar) newProgress timing
        pure State
             { stProgress = newProgress
             , stRenderTime = if shouldRender then updateTime else stRenderTime state
             }
      where
        timing = Timing
                 { timingStart = pbStartTime progressBar
                 , timingLastUpdate = updateTime
                 }

        shouldRender = not tooFast || finished
        tooFast = secSinceLastRender <= pbRefreshDelay progressBar
        finished = progressFinished newProgress

        newProgress = f $ stProgress state

        -- Amount of time that passed since last render, in seconds.
        secSinceLastRender :: Double
        secSinceLastRender = realToFrac $ diffUTCTime updateTime (stRenderTime state)

    hndl = pbHandle progressBar

-- | Increment the progress of an existing progress bar.
--
-- See 'updateProgress' for more information.
incProgress
    :: ProgressBar s -- ^ Progress bar which needs an update.
    -> Int -- ^ Amount by which to increment the progress.
    -> IO ()
incProgress pb n = updateProgress pb $ \p -> p{ progressDone = progressDone p + n }

hPutProgressBar :: Handle -> Style s -> Progress s -> Timing -> IO ()
hPutProgressBar hndl style progress timing = do
    TL.hPutStr hndl $ renderProgressBar style progress timing
    TL.hPutStr hndl $
      if progressFinished progress
      then case styleOnComplete style of
             WriteNewline -> "\n"
             -- Move to beginning of line and then clear everything to
             -- the right of the cursor.
             Clear -> "\r\ESC[K"
      else "\r"
    hFlush hndl

-- | Renders a progress bar.
--
-- >>> let t = UTCTime (ModifiedJulianDay 0) 0
-- >>> renderProgressBar defStyle (Progress 30 100 ()) (Timing t t)
-- "[============>..............................]  30%"
--
-- Note that this function can not use 'TerminalWidth' because it
-- doesn't use 'IO'. Use 'newProgressBar' or 'hNewProgressBar' to get
-- automatic width.
renderProgressBar
    :: Style s
    -> Progress s -- ^ Current progress.
    -> Timing -- ^ Timing information.
    -> TL.Text -- ^ Textual representation of the 'Progress' in the given 'Style'.
renderProgressBar style progress timing = TL.concat
    [ styleEscapePrefix style progress
    , prefixLabel
    , prefixPad
    , styleEscapeOpen style progress
    , styleOpen style
    , styleEscapeDone style progress
    , TL.replicate completed $ TL.singleton $ styleDone style
    , styleEscapeCurrent style progress
    , if remaining /= 0 && completed /= 0
      then TL.singleton $ styleCurrent style
      else ""
    , styleEscapeTodo style progress
    , TL.replicate
        (remaining - if completed /= 0 then 1 else 0)
        (TL.singleton $ styleTodo style)
    , styleEscapeClose style progress
    , styleClose style
    , styleEscapePostfix style progress
    , postfixPad
    , postfixLabel
    ]
  where
    todo = fromIntegral $ progressTodo progress
    done = fromIntegral $ progressDone progress
    -- Amount of (visible) characters that should be used to display to progress bar.
    width = fromIntegral $ getProgressBarWidth $ styleWidth style

    -- Amount of work completed.
    fraction :: Ratio Int64
    fraction | todo /= 0 = done % todo
             | otherwise = 0 % 1

    -- Amount of characters available to visualize the progress.
    effectiveWidth = max 0 $ width - usedSpace
    -- Amount of printing characters needed to visualize everything except the bar .
    usedSpace =   TL.length (styleOpen  style)
                + TL.length (styleClose style)
                + TL.length prefixLabel
                + TL.length postfixLabel
                + TL.length prefixPad
                + TL.length postfixPad

    -- Number of characters needed to represent the amount of work
    -- that is completed. Note that this can not always be represented
    -- by an integer.
    numCompletedChars :: Ratio Int64
    numCompletedChars = fraction * (effectiveWidth % 1)

    completed, remaining :: Int64
    completed = min effectiveWidth $ floor numCompletedChars
    remaining = effectiveWidth - completed

    prefixLabel, postfixLabel :: TL.Text
    prefixLabel  = runLabel (stylePrefix  style) progress timing
    postfixLabel = runLabel (stylePostfix style) progress timing

    prefixPad, postfixPad :: TL.Text
    prefixPad  = pad prefixLabel
    postfixPad = pad postfixLabel

pad :: TL.Text -> TL.Text
pad s | TL.null s = TL.empty
      | otherwise = TL.singleton ' '

-- | Width of progress bar in characters.
data ProgressBarWidth
   = ConstantWidth !Int
     -- ^ A constant width.
   | TerminalWidth !Int
     -- ^ Use the entire width of the terminal.
     --
     -- Identical to 'ConstantWidth' if the width of the terminal can
     -- not be determined.
     deriving (Generic)

instance NFData ProgressBarWidth

getProgressBarWidth :: ProgressBarWidth -> Int
getProgressBarWidth (ConstantWidth n) = n
getProgressBarWidth (TerminalWidth n) = n

{- | Visual style of a progress bar.

The style determines how a progress bar is rendered to text.

The textual representation of a progress bar follows the following template:

\<__prefix__>\<__open__>\<__done__>\<__current__>\<__todo__>\<__close__>\<__postfix__>

Where \<__done__> and \<__todo__> are repeated as often as necessary.

Consider the following progress bar

> "Working [=======>.................]  30%"

This bar can be specified using the following style:

@
'Style'
{ 'styleOpen'    = \"["
, 'styleClose'   = \"]"
, 'styleDone'    = \'='
, 'styleCurrent' = \'>'
, 'styleTodo'    = \'.'
, 'stylePrefix'  = 'msg' \"Working"
, 'stylePostfix' = 'percentage'
, 'styleWidth'   = 'ConstantWidth' 40
, 'styleEscapeOpen'    = const 'TL.empty'
, 'styleEscapeClose'   = const 'TL.empty'
, 'styleEscapeDone'    = const 'TL.empty'
, 'styleEscapeCurrent' = const 'TL.empty'
, 'styleEscapeTodo'    = const 'TL.empty'
, 'styleEscapePrefix'  = const 'TL.empty'
, 'styleEscapePostfix' = const 'TL.empty'
, 'styleOnComplete' = 'WriteNewline'
}
@
-}
data Style s
   = Style
     { styleOpen :: !TL.Text
       -- ^ Bar opening symbol.
     , styleClose :: !TL.Text
       -- ^ Bar closing symbol
     , styleDone :: !Char
       -- ^ Completed work.
     , styleCurrent :: !Char
       -- ^ Symbol used to denote the current amount of work that has been done.
     , styleTodo :: !Char
       -- ^ Work not yet completed.
     , stylePrefix :: Label s
       -- ^ Prefixed label.
     , stylePostfix :: Label s
       -- ^ Postfixed label.
     , styleWidth :: !ProgressBarWidth
       -- ^ Total width of the progress bar.
     , styleEscapeOpen :: EscapeCode s
       -- ^ Escape code printed just before the 'styleOpen' symbol.
     , styleEscapeClose :: EscapeCode s
       -- ^ Escape code printed just before the 'styleClose' symbol.
     , styleEscapeDone :: EscapeCode s
       -- ^ Escape code printed just before the first 'styleDone' character.
     , styleEscapeCurrent :: EscapeCode s
       -- ^ Escape code printed just before the 'styleCurrent' character.
     , styleEscapeTodo :: EscapeCode s
       -- ^ Escape code printed just before the first 'styleTodo' character.
     , styleEscapePrefix :: EscapeCode s
       -- ^ Escape code printed just before the 'stylePrefix' label.
     , styleEscapePostfix :: EscapeCode s
       -- ^ Escape code printed just before the 'stylePostfix' label.
     , styleOnComplete :: !OnComplete
       -- ^ What happens when progress is finished.
     } deriving (Generic)

instance (NFData s) => NFData (Style s)

-- | An escape code is a sequence of bytes which the terminal looks
-- for and interprets as commands, not as character codes.
--
-- It is vital that the output of this function, when send to the
-- terminal, does not result in characters being drawn.
type EscapeCode s
   = Progress s -- ^ Current progress bar state.
  -> TL.Text -- ^ Resulting escape code. Must be non-printable.

-- | What happens when a progress bar is finished.
data OnComplete
   = WriteNewline
     -- ^ Write a new line when the progress bar is finished. The
     -- completed progress bar will remain visible.
   | Clear -- ^ Clear the progress bar once it is finished.
     deriving (Generic)

instance NFData OnComplete

-- | The default style.
--
-- This style shows the progress as a percentage. It does not use any
-- escape sequences.
--
-- Override some fields of the default instead of specifying all the
-- fields of a 'Style' record.
defStyle :: Style s
defStyle =
    Style
    { styleOpen          = "["
    , styleClose         = "]"
    , styleDone          = '='
    , styleCurrent       = '>'
    , styleTodo          = '.'
    , stylePrefix        = mempty
    , stylePostfix       = percentage
    , styleWidth         = TerminalWidth 50
    , styleEscapeOpen    = const TL.empty
    , styleEscapeClose   = const TL.empty
    , styleEscapeDone    = const TL.empty
    , styleEscapeCurrent = const TL.empty
    , styleEscapeTodo    = const TL.empty
    , styleEscapePrefix  = const TL.empty
    , styleEscapePostfix = const TL.empty
    , styleOnComplete    = WriteNewline
    }

-- | A label is a part of a progress bar that changes based on the progress.
--
-- Labels can be at the front (prefix) or at the back (postfix) of a progress bar.
--
-- Labels can use both the current amount of progress and the timing
-- information to generate some text.
newtype Label s = Label{ runLabel :: Progress s -> Timing -> TL.Text } deriving (NFData)

-- | Combining labels combines their output.
instance Semigroup (Label s) where
    Label f <> Label g = Label $ \p t -> f p t <> g p t

-- | The mempty label always outputs an empty text.
instance Monoid (Label s) where
    mempty = msg TL.empty
    mappend = (<>)

-- | Every string is a label which ignores its input and just outputs
-- that string.
instance IsString (Label s) where
    fromString = msg . TL.pack

-- | Timing information about a 'ProgressBar'.
--
-- This information is used by 'Label's to calculate elapsed time, remaining time, total time, etc.
--
-- See 'elapsedTime', 'remainingTime' and 'totalTime'.
data Timing
   = Timing
     { timingStart :: !UTCTime
       -- ^ Moment in time when a progress bar was created. See
       -- 'newProgressBar'.
     , timingLastUpdate :: !UTCTime
       -- ^ Moment in time of the most recent progress update.
     }

-- | Static text.
--
-- The output does not depend on the input.
--
-- >>> msg "foo" st
-- "foo"
msg :: TL.Text -> Label s
msg s = Label $ \_ _ -> s

-- | Progress as a percentage.
--
-- >>> runLabel $ percentage (Progress 30 100 ()) someTiming
-- " 30%"
--
-- __Note__: if no work is to be done (todo == 0) the percentage will
-- be shown as 100%.
percentage :: Label s
percentage = Label render
  where
    render progress _timing
      | todo == 0 = "100%"
      | otherwise = TL.justifyRight 4 ' ' $ TLB.toLazyText $
                      TLB.decimal (round (done % todo * 100) :: Int)
                      <> TLB.singleton '%'
      where
        done = progressDone progress
        todo = progressTodo progress

-- | Progress as a fraction of the total amount of work.
--
-- >>> runLabel $ exact (Progress 30 100 ()) someTiming
-- " 30/100"
exact :: Label s
exact = Label render
  where
    render progress _timing =
        TL.justifyRight (TL.length todoStr) ' ' doneStr <> "/" <> todoStr
      where
        todoStr = TLB.toLazyText $ TLB.decimal todo
        doneStr = TLB.toLazyText $ TLB.decimal done

        done = progressDone progress
        todo = progressTodo progress

-- | Amount of time that has elapsed.
--
-- Time starts when a progress bar is created.
--
-- The user must supply a function which actually renders the amount
-- of time that has elapsed. You can use 'renderDuration' or
-- @formatTime@ from time >= 1.9.
elapsedTime
    :: (NominalDiffTime -> TL.Text)
    -> Label s
elapsedTime formatNDT = Label render
  where
    render _progress timing = formatNDT dt
      where
        dt :: NominalDiffTime
        dt = diffUTCTime (timingLastUpdate timing) (timingStart timing)

-- | Estimated remaining time.
--
-- Tells you how much longer some task is expected to take.
--
-- This label uses a simple estimation algorithm. It assumes progress
-- is linear. To prevent nonsense results it won't estimate remaining
-- time until at least 1 second of work has been done.
--
-- When it refuses to estimate the remaining time it will show an
-- alternative message instead.
--
-- The user must supply a function which actually renders the amount
-- of time that has elapsed. Use 'renderDuration' or @formatTime@ from
-- the time >= 1.9 package.
remainingTime
    :: (NominalDiffTime -> TL.Text)
    -> TL.Text
       -- ^ Alternative message when remaining time can't be
       -- calculated (yet).
    -> Label s
remainingTime formatNDT altMsg = Label render
  where
    render progress timing
        | dt > 1 = formatNDT estimatedRemainingTime
        | progressDone progress <= 0 = altMsg
        | otherwise = altMsg
      where
        estimatedRemainingTime = estimatedTotalTime - dt
        estimatedTotalTime = dt * recip progressFraction

        progressFraction :: NominalDiffTime
        progressFraction
          | progressTodo progress <= 0 = 1
          | otherwise = fromIntegral (progressDone progress)
                      / fromIntegral (progressTodo progress)

        dt :: NominalDiffTime
        dt = diffUTCTime (timingLastUpdate timing) (timingStart timing)

-- | Estimated total time.
--
-- This label uses a simple estimation algorithm. It assumes progress
-- is linear. To prevent nonsense results it won't estimate the total
-- time until at least 1 second of work has been done.
--
-- When it refuses to estimate the total time it will show an
-- alternative message instead.
--
-- The user must supply a function which actually renders the total
-- amount of time that a task will take. You can use 'renderDuration'
-- or @formatTime@ from the time >= 1.9 package.
totalTime
    :: (NominalDiffTime -> TL.Text)
    -> TL.Text
       -- ^ Alternative message when total time can't be calculated
       -- (yet).
    -> Label s
totalTime formatNDT altMsg = Label render
  where
    render progress timing
        | dt > 1 = formatNDT estimatedTotalTime
        | progressDone progress <= 0 = altMsg
        | otherwise = altMsg
      where
        estimatedTotalTime = dt * recip progressFraction

        progressFraction :: NominalDiffTime
        progressFraction
          | progressTodo progress <= 0 = 1
          | otherwise = fromIntegral (progressDone progress)
                      / fromIntegral (progressTodo progress)

        dt :: NominalDiffTime
        dt = diffUTCTime (timingLastUpdate timing) (timingStart timing)

-- | Show amount of time.
--
-- > renderDuration (fromInteger 42)
-- 42
--
-- > renderDuration (fromInteger $ 5 * 60 + 42)
-- 05:42
--
-- > renderDuration (fromInteger $ 8 * 60 * 60 + 5 * 60 + 42)
-- 08:05:42
--
-- Use the time >= 1.9 package to get a formatTime function which
-- accepts 'NominalDiffTime'.
renderDuration :: NominalDiffTime -> TL.Text
renderDuration dt = hTxt <> mTxt <> sTxt
  where
    hTxt | h == 0 = mempty
         | otherwise = renderDecimal h <> ":"
    mTxt | m == 0 = mempty
         | otherwise = renderDecimal m <> ":"
    sTxt = renderDecimal s

    (h, hRem) = ts   `quotRem` 3600
    (m, s   ) = hRem `quotRem`   60

    -- Total amount of seconds
    ts :: Int
    ts = round dt

    renderDecimal n = TL.justifyRight 2 '0' $ TLB.toLazyText $ TLB.decimal n

{- $start

You want to perform some task which will take some time. You wish to
show the progress of this task in the terminal.

    1. Determine the total amount of work

    2. Create a progress bar with 'newProgressBar'

    3. For each unit of work:

        3a. Perform the work

        3b. Update the progress bar with 'incProgress'

Explore the 'Style' and the 'Label' types to see various ways in which
you can customize the progress bar.

You do not have to close the progress bar, or even finish the task. It
is perfectly fine to stop half way (maybe your task throws an
exception).

Just remember to avoid outputting text to the terminal while a
progress bar is active. It will mess up the output a bit.
-}

{- $example

Write a function which represents a unit of work. This could be a file
copy operation, a network operation or some other expensive
calculation. This example simply waits 1 second.

@
  work :: IO ()
  work = threadDelay 1000000 -- 1 second
@

And you define some work to be done. This could be a list of files to
process or some jobs that need to be processed.

@
  toBeDone :: [()]
  toBeDone = replicate 20 ()
@

Now create the progress bar. Use the default style and choose a
maximum refresh rate of 10 Hz. The initial progress is 0 work done out
of 20.

@
  pb <- 'newProgressBar' 'defStyle' 10 ('Progress' 0 20 ())
@

Start performing the work while keeping the user informed of the progress:

@
  for_ toBeDone $ \() -> do
    work             -- perform 1 unit of work
    'incProgress' pb 1 -- increment progress by 1
@

That's it! You get a nice animated progress bar in your terminal. It
will look like this:

@
[==========>................................]  25%
@
-}
