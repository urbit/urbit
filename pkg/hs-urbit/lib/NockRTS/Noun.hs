{-# LANGUAGE MagicHash, UnboxedTuples, UnboxedSums #-}

module NockRTS.Noun where

import Data.Word
import GHC.Base                   hiding (C#)
import GHC.Integer.GMP.Internals
import GHC.Prim
import Prelude                   hiding (cons)

import Data.List (intercalate)
import GHC.Real  (underflowError)

--------------------------------------------------------------------------------

type Noun# = (# Word# | BigNat | Cell #)
type Atom# = (# Word# | BigNat #)
type Cell# = (# Noun#, Noun# #)

data Cell = C# Cell#
data Atom = A# Atom#
data Noun = N# Noun#


-- Unboxed Atom Operations -----------------------------------------------------

wordIsZero# :: Word# -> Bool
wordIsZero# w = 0 == (I# (word2Int# w))

words2Atom# :: (# Word#, Word# #) -> Atom#
words2Atom# (# x, y #) =
  if wordIsZero# x
  then (# y |                   #)
  else (#   | wordToBigNat2 x y #)

inc# :: Atom# -> Atom#
inc# (# w |   #) = words2Atom# (plusWord2# w (int2Word# 1#))
inc# (#   | n #) = (# | n #)

plusAtom# :: Atom# -> Atom# -> Atom#
plusAtom# (# x |   #) (# y |   #) = words2Atom# (# x,  y #)
plusAtom# (# w |   #) (#   | n #) = (# | plusBigNatWord n w #)
plusAtom# (#   | n #) (# w |   #) = (# | plusBigNatWord n w #)
plusAtom# (#   | x #) (#   | y #) = (# | plusBigNat x y     #)

minusAtom# :: Atom# -> Atom# -> Atom#
minusAtom# x         (# 0## | #) = x

{-
minusAtom# (NatS# x) (NatS# y)   = case subWordC# x y of
                                     (# l, 0# #) -> NatS# l
                                     _           -> underflowError
minusAtom# (NatS# _) (NatJ# _)   = underflowError
minusAtom# (NatJ# x) (NatS# y)   = bigNatToAtom# (minusBigNatWord x y)
minusAtom# (NatJ# x) (NatJ# y)   = bigNatToAtom# (minusBigNat     x y)
-}


word2Atom# :: Word# -> Atom#
word2Atom# w = (# w | #)

bigNat2Atom# :: BigNat -> Atom#
bigNat2Atom# bn = (# | bn #)


-- Unboxed Cell Operations -----------------------------------------------------

car# :: Cell# -> Noun#
car# (# x, _ #) = x

cdr# :: Cell# -> Noun#
cdr# (# _, y #) = y

cellCons# :: Noun# -> Noun# -> Cell#
cellCons# x y = (# x, y #)


-- Unboxed Noun Operations -----------------------------------------------------

runNoun# :: Noun# -> (Cell -> a) -> (Atom# -> a) -> a
runNoun# (# w |   |   #) c a = a (# w |   #)
runNoun# (#   | n |   #) c a = a (#   | n #)
runNoun# (#   |   | p #) c a = c p

atom2Noun# :: Atom# -> Noun#
atom2Noun# (# w |   #) = (# w |   | #)
atom2Noun# (#   | n #) = (#   | n | #)

word2Noun# :: Word# -> Noun#
word2Noun# w = (# w | | #)

bigNat2Noun# :: BigNat -> Noun#
bigNat2Noun# bn = (# | bn | #)

cell2Noun# :: Cell# -> Noun#
cell2Noun# c = (# | | C# c #)


-- Boxed Operations ------------------------------------------------------------

plusAtom :: Atom -> Atom -> Atom
plusAtom (A# x) (A# y) = A# (plusAtom# x y)

minusAtom :: Atom -> Atom -> Atom
minusAtom (A# x) (A# y) = A# (minusAtom# x y)

negateAtom :: Atom -> Atom
negateAtom = undefined

timesAtom :: Atom -> Atom -> Atom
timesAtom = undefined

atomFromInteger :: Integer -> Atom
atomFromInteger (S# i)  = A# (# int2Word# i |   #)
atomFromInteger (Jp# n) = A# (#             | n #)
atomFromInteger _       = underflowError

signumAtom :: Atom -> Atom
signumAtom = undefined

atom2Noun :: Atom -> Noun
atom2Noun (A# a) = N# (atom2Noun# a)

cell2Noun :: Cell -> Noun
cell2Noun c = N# (# | | c #)

cons :: Noun -> Noun -> Noun
cons (N# x) (N# y) = cell2Noun (C# (cellCons# x y))

runNoun :: Noun -> (Cell -> a) -> (Atom -> a) -> a
runNoun (N# n) f g = runNoun# n (\c -> f c) (\a -> g (A# a))

toAtom :: Noun -> Maybe Atom
toAtom (N# n) = runNoun# n (\_ -> Nothing) (\a -> Just (A# a))

plusNoun :: Noun -> Noun -> Maybe Noun
plusNoun x y = atom2Noun <$> (plusAtom <$> toAtom x <*> toAtom y)


-- Random Bullshit -------------------------------------------------------------

cell2List :: Cell -> [Noun]
cell2List = go []
  where
    go :: [Noun] -> Cell -> [Noun]
    go acc (C# (# x, y #)) = runNoun# y (\c -> go (N# x : acc) c)
                                        (\a -> reverse (N# y : N# x : acc))

list2Noun :: [Noun] -> Noun
list2Noun []     = atom2Noun 0
list2Noun [x]    = x
list2Noun (x:xs) = cons x (list2Noun xs)

fmtCell :: [String] -> String
fmtCell xs = "[" <> intercalate " " xs <> "]"

instance Num Atom where
    (+)         = plusAtom
    (-)         = minusAtom
    (*)         = timesAtom
    negate      = negateAtom
    fromInteger = atomFromInteger
    abs         = id
    signum      = signumAtom

instance Show Atom where
  show (A# (# w |   #)) = show (W# w)
  show (A# (#   | n #)) = show (Jp# n)

instance Show Cell where
  show c = fmtCell (fmap show (cell2List c))

instance Show Noun where
  show (N# (# w |   |   #)) = show (W# w)
  show (N# (#   | n |   #)) = show (Jp# n)
  show (N# (#   |   | c #)) = show c

example :: Noun
example = list2Noun [atom2Noun 1337, atom2Noun 1338, atom2Noun 0]

exampleIO :: IO ()
exampleIO = do
  print example
