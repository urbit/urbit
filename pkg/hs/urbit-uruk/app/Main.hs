module Main (main) where

import ClassyPrelude
import Prelude (read)
import Uruk.Fast (acker)

main ∷ IO ()
main = do
    getArgs >>= \case
        [x,y] -> do
            res <- acker (read $ unpack x) (read $ unpack y)
            print res
