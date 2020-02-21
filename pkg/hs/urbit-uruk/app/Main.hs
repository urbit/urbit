module Main (main) where

import ClassyPrelude
import Prelude (read)
import Uruk.Fast (acker)

main ∷ IO ()
main = do
    getArgs >>= \case
        [readMay → Just x,readMay → Just y] -> do
            res <- acker x y
            print res
        _ → do
            putStrLn "usage: urbit-acker x y"
