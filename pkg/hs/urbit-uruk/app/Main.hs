module Main (main) where

import ClassyPrelude
import Prelude (read)
import Moon.MoonToUruk (runFastAcker, runFastFib)

main ∷ IO ()
main = do
    getArgs >>= \case
        ["acker", readMay → Just x,readMay → Just y] -> do
            res <- runFastAcker x y
            print res
        ["fib", readMay → Just x] -> do
            res <- runFastFib x
            print res
        _ → do
            putStrLn "usage: urbit-uruk acker x y"
            putStrLn "       urbit-uruk fib x"
