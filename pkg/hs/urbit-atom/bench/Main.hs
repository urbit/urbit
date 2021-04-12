{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Prelude
import Criterion.Main

import Data.ByteString (ByteString)
import Urbit.Atom      (Atom)

import qualified Urbit.Atom.Fast as Fast
import qualified Urbit.Atom.Slow as Slow


-- Examples --------------------------------------------------------------------

a64, a32768 :: Atom
a64    = (2^64) - 1
a32768 = (2^32768)-1

bDog, bBigDog :: ByteString
bDog    = "The quick brown fox jumps over the lazy dog."
bBigDog = mconcat (replicate 800 bDog)


-- Benchmarks ------------------------------------------------------------------

maiDump = Fast.atomBytes
maiLoad = Fast.bytesAtom
sloDump = Slow.atomBytes
sloLoad = Slow.bytesAtom
gmpDump = Fast.exportBytes
gmpLoad = Fast.importBytes

main = defaultMain
  [ bgroup "lit-dump" [ bench "slo" $ whnf sloDump a64
                      , bench "gmp" $ whnf gmpDump a64
                      , bench "mai" $ whnf maiDump a64
                      ]

  , bgroup "big-dump" [ bench "gmp" $ whnf gmpDump a32768
                      , bench "mai" $ whnf maiDump a32768
                      ]

  , bgroup "lit-load" [ bench "slo" $ whnf sloLoad bDog
                      , bench "gmp" $ whnf gmpLoad bDog
                      , bench "mai" $ whnf maiLoad bDog
                      ]

  , bgroup "big-load" [ bench "gmp" $ whnf gmpLoad bBigDog
                      , bench "mai" $ whnf maiLoad bBigDog
                      ]
  ]

