module Ur.Common
    ( module ClassyPrelude
    , module Ur.Noun
    , module Control.Arrow
    , module Control.Lens
    , module Prelude
    , At, Dr(..), Ix(..), axis, ix
    ) where

import ClassyPrelude
import Ur.Noun

import Control.Arrow ((<<<), (>>>))
import Control.Lens  ((&))
import Prelude       ((!!))


-- Types -----------------------------------------------------------------------

type At = Atom

data Dr = L | R
  deriving (Eq, Ord, Show)

newtype Ix = Ix { unIx :: [Dr] }
  deriving newtype (Eq, Ord)

instance Show Ix where
    show (Ix ds) = concat (show <$> ds)


-- Tree Indexing ---------------------------------------------------------------

axis :: Ix -> At
axis = go . reverse . unIx
  where
    go = \case []   -> 1
               L:ds -> 2 * go ds
               R:ds -> 2 * go ds + 1

ix :: At -> Ix
ix = Ix . reverse . go
  where
   go = \case 0          -> error "ix called with 0 value"
              1          -> []
              2          -> [L]
              3          -> [R]
              x | even x -> L : go (x `div` 2)
              x          -> R : go (x `div` 2)
