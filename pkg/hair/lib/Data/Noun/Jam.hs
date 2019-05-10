module Data.Noun.Jam where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Poet
import Data.Bits
import Control.Lens

import Data.Map      (Map)
import Control.Monad (guard)

--------------------------------------------------------------------------------

jam :: Noun -> Atom
jam = undefined

leadingZeros :: Cursor -> Maybe Int
leadingZeros (Cursor idx buf) = go 0
  where wid  = bitWidth buf
        go n = do guard (n < wid)
                  if bitIdx (idx+n) buf then pure n else go (n+1)

rub :: Cursor -> Maybe (Int, Atom)
rub slc@(Cursor idx buf) =
  leadingZeros slc >>= \case
    0      -> pure (1, 0)
    prefix -> pure (sz, val)
      where
        widIdx = idx + 1 + prefix
        width  = fromSlice (Slice widIdx (prefix - 1) buf)
        datIdx = widIdx + (prefix-1)
        datWid = fromIntegral $ 2^(prefix-1) + width
        sz     = datWid + (2*prefix)
        val    = fromSlice (Slice datIdx datWid buf)

cue :: Atom -> Maybe Noun
cue buf = view _2 <$> go mempty 0
  where
    go :: Map Int Noun -> Int -> Maybe (Int, Noun, Map Int Noun)
    go tbl i =
      case (bitIdx i buf, bitIdx (i+1) buf) of
        (False, _     ) -> do (wid,at) <- rub (Cursor (i+1) buf)
                              let r = toNoun at
                              pure (wid+1, r, insertMap i r tbl)
        (True,  False ) -> do (lSz,lef,tbl) <- go tbl (i+2)
                              (rSz,rit,tbl) <- go tbl (i+2+fromIntegral lSz)
                              let r = Cell lef rit
                              pure (2+lSz+rSz, r, insertMap i r tbl)
        (True,  True  ) -> do (wid,at) <- rub (Cursor (i+2) buf)
                              r <- lookup (fromIntegral at) tbl
                              pure (2+wid, r, tbl)

cueTest :: Maybe [Noun]
cueTest =
  traverse cue [ 0x2, 0xc, 0x48, 0x29, 0xc9, 0x299
               , 0x3170_c7c1, 0x93_c7c1, 0x1bd5_b7dd_e080
               ]
