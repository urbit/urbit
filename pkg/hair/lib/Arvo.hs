module Arvo where

import ClassyPrelude

--------------------------------------------------------------------------------

data Event = Event
data Effect = Effect

data ArvoFn = MkArvoFn (Event -> ([Effect], ArvoFn))

data Arvo r
  = Yield [Effect] (Event -> Arvo r)
  | Pure r


-- Arvo is a Monad -------------------------------------------------------------

bind :: Arvo a -> (a -> Arvo b) -> Arvo b
bind (Pure x)     f = f x
bind (Yield fx k) f = Yield fx (\ev -> bind (k ev) f)

instance Functor Arvo where
  fmap f (Pure v) = Pure (f v)
  fmap f (Yield fx cont) = Yield fx (fmap (fmap f) cont)

instance Applicative Arvo where
  pure     = Pure
  mx <*> y = mx `bind` (\f -> f <$> y)

instance Monad Arvo where
  (>>=) = bind

--------------------------------------------------------------------------------

yield :: [Effect] -> Arvo Event
yield fx = Yield fx Pure

example :: Arvo a
example = do
  Event <- yield [Effect, Effect]
  example
