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

bex :: (Num a, Bits a) => Int -> a
bex = shiftL 1

mat' :: Atom -> Buf
mat' 0   = Buf 1 1
mat' atm = Buf bufWid buffer
  where
    atmWid = bitWidth atm
    preWid = bitWidth (toAtom atmWid)
    bufWid = preWid + preWid + atmWid - 1
    prefix = bex preWid
    extras = takeBits (preWid-1) (toAtom atmWid)
    suffix = xor extras (shiftL (takeBits (atmWid-1) atm) (preWid-1))
    buffer = bitConcat prefix suffix

rub' :: Cursor -> Maybe Buf
rub' slc@(Cursor idx buf) =
  leadingZeros slc >>= \case
    0      -> pure (Buf 1 0)
    prefix -> pure (Buf sz val)
      where
        widIdx = idx + 1 + prefix
        width  = fromSlice (Slice widIdx (prefix - 1) buf)
        datIdx = widIdx + (prefix-1)
        datWid = fromIntegral (2^(prefix-1) + width) - 1
        sz     = datWid + (2*prefix)
        val    = bex datWid .|. fromSlice (Slice datIdx datWid buf)

jam' :: Noun -> Atom
jam' = toAtom . fst . go 0 mempty
  where
    insertNoun :: Noun -> Int -> Map Noun Int -> Map Noun Int
    insertNoun n i tbl = lookup n tbl
                       & maybe tbl (const $ insertMap n i tbl)

    go :: Int -> Map Noun Int -> Noun -> (Buf, Map Noun Int)
    go off oldTbl noun =
      let tbl = insertNoun noun off oldTbl in
      case (lookup noun oldTbl, noun) of
        (Just ref, Atom atm) | bitWidth atm <= bitWidth (toAtom ref) ->
          (Buf (1+sz) (shiftL res 1), tbl)
            where Buf sz res = mat' atm
        (Just ref, _) ->
          (Buf (2+sz) (xor 3 (shiftL res 2)), tbl)
            where Buf sz res = mat' (toAtom ref)
        (Nothing, Atom atm) ->
          (Buf (1+sz) (shiftL res 1), tbl)
            where Buf sz res = mat' atm
        (Nothing, Cell lef rit) ->
          (Buf (2+lSz+rSz) (xor 1 (shiftL (lRes .|. shiftL rRes lSz) 2)), rTbl)
            where (Buf lSz lRes, lTbl) = go (off+2)   tbl  lef
                  (Buf rSz rRes, rTbl) = go (off+lSz) lTbl rit

cue' :: Atom -> Maybe Noun
cue' buf = view _2 <$> go mempty 0
  where
    go :: Map Int Noun -> Int -> Maybe (Int, Noun, Map Int Noun)
    go tbl i =
      case (bitIdx i buf, bitIdx (i+1) buf) of
        (False, _     ) -> do Buf wid at <- rub' (Cursor (i+1) buf)
                              let r = toNoun at
                              pure (wid+1, r, insertMap i r tbl)
        (True,  False ) -> do (lSz,lef,tbl) <- go tbl (i+2)
                              (rSz,rit,tbl) <- go tbl (i+2+fromIntegral lSz)
                              let r = Cell lef rit
                              pure (2+lSz+rSz, r, insertMap i r tbl)
        (True,  True  ) -> do Buf wid at <- rub' (Cursor (i+2) buf)
                              r <- lookup (fromIntegral at) tbl & \case
                                     Nothing -> error ("bad-ref-" <> show at)
                                     Just ix -> Just ix
                              pure (2+wid, r, tbl)

--------------------------------------------------------------------------------

mat :: Atom -> Buf
mat 0   = Buf 1 1
mat atm = Buf bufWid buffer
  where
    atmWid = bitWidth atm
    preWid = bitWidth (toAtom atmWid)
    bufWid = preWid + preWid + atmWid
    prefix = shiftL 1 preWid
    extras = takeBits (preWid-1) (toAtom atmWid)
    suffix = xor extras (shiftL atm (preWid-1))
    buffer = bitConcat prefix suffix

bufVal Nothing = "<nil>"
bufVal (Just (Buf sz v)) = show v <> " [" <> show sz <> "]"

rub :: Cursor -> Maybe Buf
rub slc@(Cursor idx buf) = trace (bufVal res) res
  where
    res =
      trace ("rub-" <> show idx) $
      leadingZeros slc >>= \case
        0      -> pure (Buf 1 0)
        prefix -> pure (Buf sz val)
          where
            widIdx = idx + 1 + prefix
            width  = fromSlice (Slice widIdx (prefix - 1) buf)
            datIdx = widIdx + (prefix-1)
            datWid = fromIntegral $ 2^(prefix-1) + width
            sz     = datWid + (2*prefix)
            val    = fromSlice (Slice datIdx datWid buf)

-- Noun Serialization ----------------------------------------------------------

jam :: Noun -> Atom
jam = toAtom . fst . go 0 mempty
  where
    insertNoun :: Noun -> Int -> Map Noun Int -> Map Noun Int
    insertNoun n i tbl = lookup n tbl
                       & maybe tbl (const $ insertMap n i tbl)

    go :: Int -> Map Noun Int -> Noun -> (Buf, Map Noun Int)
    go off oldTbl noun =
      let tbl = insertNoun noun off oldTbl in
      case (lookup noun oldTbl, noun) of
        (Just ref, Atom atm) | bitWidth atm <= bitWidth (toAtom ref) ->
          (Buf (1+sz) (shiftL res 1), tbl)
            where Buf sz res = mat atm
        (Just ref, _) ->
          (Buf (2+sz) (xor 3 (shiftL res 2)), tbl)
            where Buf sz res = mat (toAtom ref)
        (Nothing, Atom atm) ->
          (Buf (1+sz) (shiftL res 1), tbl)
            where Buf sz res = mat atm
        (Nothing, Cell lef rit) ->
          (Buf (2+lSz+rSz) (xor 1 (shiftL (bitConcat lRes rRes) 2)), rTbl)
            where (Buf lSz lRes, lTbl) = go (off+2)   tbl  lef
                  (Buf rSz rRes, rTbl) = go (off+lSz) lTbl rit



leadingZeros :: Cursor -> Maybe Int
leadingZeros (Cursor idx buf) = go 0
  where wid  = bitWidth buf
        go n = do () <- if (n < wid) then pure ()
                                     else error "infinite-atom"
                  guard (n < wid)
                  if bitIdx (idx+n) buf then pure n else go (n+1)

cue :: Atom -> Maybe Noun
cue buf = view _2 <$> go mempty 0
  where
    go :: Map Int Noun -> Int -> Maybe (Int, Noun, Map Int Noun)
    go tbl i =
      trace ("go-" <> show i)
      case (bitIdx i buf, bitIdx (i+1) buf) of
        (False, _     ) -> do Buf wid at <- rub (Cursor (i+1) buf)
                              let r = toNoun at
                              pure (wid+1, r, insertMap i r tbl)
        (True,  False ) -> do (lSz,lef,tbl) <- go tbl (i+2)
                              (rSz,rit,tbl) <- go tbl (i+2+fromIntegral lSz)
                              let r = Cell lef rit
                              pure (2+lSz+rSz, r, insertMap i r tbl)
        (True,  True  ) -> do Buf wid at <- rub (Cursor (i+2) buf)
                              traceM ("ref-" <> show at)
                              r <- lookup (fromIntegral at) tbl & \case
                                     Nothing -> error ("bad-ref-" <> show at)
                                     Just ix -> Just ix
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
    Buf matSz matBuf = mat atm
    Buf rubSz rubRes = fromMaybe mempty (rub $ Cursor 0 matBuf)

prop_jamCue' :: Noun -> Bool
prop_jamCue' n = Just n == cue' (jam' n)

prop_matRub' :: Atom -> Bool
prop_matRub' atm = matSz==rubSz && rubRes==atm
  where
    Buf matSz matBuf = mat' atm
    Buf rubSz rubRes = fromMaybe mempty (rub' $ Cursor 0 matBuf)

main :: IO ()
main = $(defaultMainGenerator)
