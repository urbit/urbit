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

bitIdx :: Atom -> Atom -> Bool
bitIdx idx buf = testBit buf (fromIntegral idx)

bitSlice :: Atom -> Atom -> Atom -> Atom
bitSlice idx sz buf = undefined

data Slice = Slice { off :: Atom, buf :: Atom }

leadingZeros :: Slice -> Maybe Atom
leadingZeros (Slice idx buf) = go 0
  where wid  = bitWidth buf
        go n = do guard (n < wid)
                  if bitIdx (idx+n) buf then pure n else go (n+1)

rub :: Slice -> Maybe (Atom, Atom)
rub slc@(Slice idx buf) =
  leadingZeros slc >>= \case
    0      -> pure (1, 0)
    prefix -> pure (sz, val)
      where
        widIdx = idx + 1 + prefix
        width  = bitSlice widIdx (prefix - 1) buf
        datIdx = widIdx + (prefix-1)
        datWid = 2^(prefix-1) + width
        sz     = datWid + (2*prefix)
        val    = bitSlice datIdx datWid buf

cue :: Atom -> Maybe Noun
cue buf = view _2 <$> go mempty 0
  where
    go :: Map Atom Noun -> Atom -> Maybe (Atom, Noun, Map Atom Noun)
    go tbl i =
      case (bitIdx i buf, bitIdx (i+1) buf) of
        (False, _     ) -> do (wid,at) <- rub (Slice (i+1) buf)
                              let r = toNoun at
                              pure (wid+1, r, insertMap i r tbl)
        (True,  False ) -> do (lSz,lef,tbl) <- go tbl (i+2)
                              (rSz,rit,tbl) <- go tbl (i+2+lSz)
                              let r = Cell lef rit
                              pure (2+lSz+rSz, r, insertMap i r tbl)
        (True,  True  ) -> do (wid,at) <- rub (Slice (i+2) buf)
                              r <- lookup at tbl
                              pure (2+wid, r, tbl)
