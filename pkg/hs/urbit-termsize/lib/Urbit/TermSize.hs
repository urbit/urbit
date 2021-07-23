{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Urbit.TermSize
  ( TermSize(..)
  , termSize
  , liveTermSize
  )
where

import Prelude

import Data.Functor                 ((<&>))
import System.Console.Terminal.Size (Window(..), size)

import qualified System.Posix.Signals      as Sys
import qualified System.Posix.Signals.Exts as Sys


-- Types -----------------------------------------------------------------------

data TermSize = TermSize
  { tsWide :: !Word
  , tsTall :: !Word
  }
 deriving (Eq, Ord, Show)


-- Utilities -------------------------------------------------------------------

termSize :: IO TermSize
termSize = size <&> \case
  Nothing            -> TermSize 80 24
  Just (Window {..}) -> TermSize width height

liveTermSize :: (TermSize -> IO ()) -> IO TermSize
liveTermSize cb = do
  Sys.installHandler Sys.sigWINCH (Sys.Catch (termSize >>= cb)) Nothing
  ts <- termSize
  cb ts
  pure ts
