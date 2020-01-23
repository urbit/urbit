module Ur.Noun.Rip where

import ClassyPrelude
import Data.Bits
import Ur.Noun.Atom

import Control.Lens (from, view, (&))

import qualified Data.Vector.Primitive as VP

--------------------------------------------------------------------------------

takeBits ∷ Word → Word → Word
takeBits 64 w = w
takeBits 0  w = 0
takeBits n  w = w .&. (shiftL 1 (fromIntegral n) - 1)

divCeil ∷ Word → Word → Word
divCeil 0 y = 0
divCeil x y = 1 + ((x-1) `div` y)

--------------------------------------------------------------------------------

repn :: Word -> [Word] -> Atom
repn bits blox =
    (bits > 64) & \case
        True  → error "repn only works with block sizes <= 64"
        False → view (from atomWords)
              $ VP.fromList
              $ finish
              $ foldl' f ([], 0, 0)
              $ zip (repeat bits) blox
  where
    finish (acc, wor, n) = reverse
                         $ dropWhile (==0)
                         $ case n of { 0 -> acc; _ -> wor:acc }

    slice size off wor = shiftL (takeBits size wor)
                       $ fromIntegral off

    f (acc, wor, off) (remBlok, blok) =
        let rem = 64 - off in
        compare remBlok rem & \case
            LT -> (acc, res, off+bits)
              where res = wor .|. slice bits off blok
            EQ -> (res:acc, 0, 0)
              where res = (wor .|. slice bits off blok)
            GT -> f (res:acc, 0, 0) (remBlok', blok')
              where res      = wor .|. slice rem off blok
                    remBlok' = remBlok-rem
                    blok'    = shiftR blok (fromIntegral bits)
