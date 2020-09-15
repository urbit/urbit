{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.Arity
  ( Arity(..)
  , appArity
  , arityPos
  , arityInt
  )
where

import ClassyPrelude
import Urbit.Pos


--------------------------------------------------------------------------------

data Arity
  = AriEss       --  S
  | AriKay       --  K
  | AriEnh !Pos  --  Jⁿ
  | AriHdr !Pos  --  Jⁿt
  | AriDub       --  W
  | AriOth !Pos  --  Anything else
 deriving (Show)

appArity :: Arity -> Arity -> Maybe Arity
appArity (AriOth 1) _          = Nothing
appArity AriDub     _          = Just $ AriOth 6
appArity (AriOth n) _          = Just $ AriOth $ pred n
appArity (AriEnh n) (AriEnh 1) = Just $ AriEnh $ succ n
appArity (AriEnh n) _          = Just $ AriHdr n
appArity (AriHdr n) _          = Just $ AriOth n
appArity AriEss     _          = Just $ AriOth 2
appArity AriKay     _          = Just $ AriOth 2

arityPos :: Arity -> Pos
arityPos = \case
  AriEss   -> 3
  AriKay   -> 3
  AriEnh p -> p+2
  AriHdr p -> p+1
  AriDub   -> 7
  AriOth p -> p

arityInt :: Arity -> Int
arityInt = fromIntegral . arityPos
