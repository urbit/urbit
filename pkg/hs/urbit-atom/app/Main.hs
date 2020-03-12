module Main where

import Prelude
import Urbit.Atom
import Urbit.Atom.Fast (bit, byt)

main :: IO ()
main = do
  print (bit, byt)
  f "a"
  f "x"
  f "aa"
  f "ax"
  f "aaa"
  f "aax"
  f "aaaa"
  f "aaax"
  f "aaaaa"
  f "aaaax"
  f "aaaaaa"
  f "aaaaax"
  f "aaaaaaa"
  f "aaaaaax"
  f "aaaaaaaa"
  f "aaaaaaax"
  f "aaaaaaaaa"
  f "aaaaaaaax"

 where
  f x = print (x, utf8Atom x)
