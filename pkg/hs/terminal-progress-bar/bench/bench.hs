{-# language PackageImports #-}
module Main where

import "base" Data.Monoid ( (<>) )
import "criterion" Criterion.Main
import "terminal-progress-bar" System.ProgressBar
import "time" Data.Time.Clock ( UTCTime(..) )

main :: IO ()
main = defaultMain
       [ renderProgressBarBenchmark  10   0
       , renderProgressBarBenchmark  10  50
       , renderProgressBarBenchmark  10 100
       , renderProgressBarBenchmark 100   0
       , renderProgressBarBenchmark 100  50
       , renderProgressBarBenchmark 100 100
       , renderProgressBarBenchmark 200   0
       , renderProgressBarBenchmark 200  50
       , renderProgressBarBenchmark 200 100
       , labelBenchmark "percentage" percentage (Progress   0 100 ())
       , labelBenchmark "percentage" percentage (Progress  50 100 ())
       , labelBenchmark "percentage" percentage (Progress 100 100 ())
       , labelBenchmark "exact"      exact      (Progress   0 100 ())
       , labelBenchmark "exact"      exact      (Progress  50 100 ())
       , labelBenchmark "exact"      exact      (Progress 100 100 ())
       ]

renderProgressBarBenchmark :: Int -> Int -> Benchmark
renderProgressBarBenchmark width done =
    bench name $ nf (\(s, p, t) -> renderProgressBar s p t)
        ( defStyle{styleWidth = ConstantWidth width}
        , Progress done 100 ()
        , someTiming
        )
  where
    name = "progressBar/default - "
           <> show width <> " wide - progress " <> show done <> " % 100"

labelBenchmark :: String -> Label () -> Progress () -> Benchmark
labelBenchmark labelName label progress =
    bench name $ nf (\(p, t) -> runLabel label p t) (progress, someTiming)
  where
    name = "label/" <> labelName <> " "
           <> show (progressDone progress) <> " % "
           <> show (progressTodo progress)

someTime :: UTCTime
someTime = UTCTime (toEnum 0) 0

someTiming :: Timing
someTiming = Timing someTime someTime
