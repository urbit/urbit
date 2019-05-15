module Data.Noun.Jam where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Poet
import Data.Bits
import Control.Lens
import Text.Printf

import Data.Map      (Map)
import Control.Monad (guard)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck


-- Length-Encoded Atoms --------------------------------------------------------

mat :: Atom -> (Int, Atom)
mat 0   = (1, 1)
mat atm = (bufWid, buffer)
  where
    atmWid = bitWidth atm
    preWid = bitWidth (toAtom atmWid)
    bufWid = preWid + preWid + atmWid
    prefix = shiftL 1 preWid
    extras = takeBits (preWid-1) $ toAtom atmWid
    suffix = xor extras (shiftL atm (preWid-1))
    buffer = bitConcat prefix suffix

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


-- Noun Serialization ----------------------------------------------------------

jam :: Noun -> Atom
jam = view _2 . go 0 mempty
  where
    insertNoun :: Noun -> Int -> Map Noun Int -> Map Noun Int
    insertNoun n i tbl = lookup n tbl
                       & maybe tbl (const $ insertMap n i tbl)

    go :: Int -> Map Noun Int -> Noun -> (Int, Atom, Map Noun Int)
    go idx oldTbl noun =
      let tbl = insertNoun noun idx oldTbl in
      case (Nothing :: Maybe Int, noun) of
          (Just ref, Atom atm) | bitWidth atm <= bitWidth (toAtom ref) ->
              (1+sz, shiftL res 1, tbl)
                where (sz, res) = mat atm
          (Just ref, _) ->
              (2+sz, xor 3 (shiftL res 2), tbl)
                where (sz, res) = mat (toAtom ref)
          (Nothing, Atom atm) ->
              (1+sz, shiftL res 1, tbl)
                where (sz, res) = mat atm
          (Nothing, Cell lef rit) ->
            (2+lSz+rSz, xor 1 (shiftL (bitConcat lRes rRes) 2), rTbl)
              where (lSz, lRes, lTbl) = go (idx+2)   tbl  lef
                    (rSz, rRes, rTbl) = go (idx+lSz) lTbl rit


leadingZeros :: Cursor -> Maybe Int
leadingZeros (Cursor idx buf) = go 0
  where wid  = bitWidth buf
        go n = do guard (n < wid)
                  if bitIdx (idx+n) buf then pure n else go (n+1)

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


-- Tests -----------------------------------------------------------------------

pills :: [Atom]
pills = [ 0x2, 0xc, 0x48, 0x29, 0xc9, 0x299
        , 0x3170_c7c1, 0x93_c7c1, 0xa_72e0, 0x1bd5_b7dd_e080
        ]

cueTest :: Maybe [Noun]
cueTest = traverse cue pills

jamTest :: Maybe [Atom]
jamTest = fmap jam <$> cueTest

prop_jamCue :: Noun -> Bool
prop_jamCue n = Just n == cue (jam n)

prop_matRub :: Atom -> Bool
prop_matRub atm = matSz==rubSz && rubRes==atm
  where
    (matSz, matBuf) = mat atm
    (rubSz, rubRes) = fromMaybe (0,0) (rub $ Cursor 0 matBuf)

main :: IO ()
main = $(defaultMainGenerator)

--    ?:  =(0 a)
--      [1 1]
--    =+  b=(met 0 a)
--    =+  c=(met 0 b)
--    :-  (add (add c c) b)
--    (cat 0 (bex c) (mix (end 0 (dec c) b) (lsh 0 (dec c) a)))

--    |=  a/@
--    ^-  {p/@ q/@}
--    ?:  =(0 a)
--      [1 1]
--    =+  b=(met 0 a)
--    =+  c=(met 0 b)
--    :-  (add (add c c) b)
--    (cat 0 (bex c) )

--  ++  jam
--    |=  a/*
--    ^-  @
--    =+  b=0
--    =+  m=`(map * @)`~
--    =<  q
--    |-  ^-  {p/@ q/@ r/(map * @)}
--    =+  c=(~(get by m) a)
--    ?~  c
--      =>  .(m (~(put by m) a b))
--      ?:  ?=(@ a)
--        =+  d=(mat a)
--        [(add 1 p.d) (lsh 0 1 q.d) m]
--      =>  .(b (add 2 b))
--      =+  d=$(a -.a)
--      =+  e=$(a +.a, b (add b p.d), m r.d)
--      [(add 2 (add p.d p.e)) (mix 1 (lsh 0 2 (cat 0 q.d q.e))) r.e]
--    ?:  ?&(?=(@ a) (lte (met 0 a) (met 0 u.c)))
--      =+  d=(mat a)
--      [(add 1 p.d) (lsh 0 1 q.d) m]
--    =+  d=(mat u.c)
--    [(add 2 p.d) (mix 3 (lsh 0 2 q.d)) m]
