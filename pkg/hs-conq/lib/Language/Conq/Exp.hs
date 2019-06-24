{-# LANGUAGE StandaloneDeriving #-}

module Language.Conq.Exp where

import Language.Conq.Types
import Language.Conq.Grainary

import ClassyPrelude hiding ((<.>), Left, Right, hash, cons)
import Data.Type.Equality
import Type.Reflection
import Data.Coerce
import GHC.Natural
import Control.Category
import Data.Flat
import Data.Flat.Bits
import Data.Bits
import Data.Vector (generate)
import Data.Monoid.Unicode ((∅))
import Data.List.NonEmpty (NonEmpty(..))

import Control.Lens               ((&))
import Control.Monad.Except       (ExceptT, runExceptT)
import Control.Monad.State        (State, get, put, evalState, runState)
import Control.Monad.Trans.Except (throwE)
import Data.Bits                  ((.|.), shiftL, shiftR)
import System.IO.Unsafe           (unsafePerformIO)
import Text.Show                  (showString, showParen)

import qualified Prelude                as P
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as B


-- Unparsing -------------------------------------------------------------------

dispExp :: Exp -> String
dispExp = \case
  ESubj     -> "."
  EPure VN  -> "~"
  EPure v   -> show (valExp v)
  EEval     -> "!"
  ELeft     -> "0"
  EWrit     -> "1"
  EHead     -> "-"
  ETail     -> "+"
  EDist     -> "%"
  EWith x y -> show y <> show x
  ECons x y -> "[" <> show x <> " " <> show y <> "]"
  ECase x y -> "<" <> show x <> " " <> show y <> ">"
  EType t   -> "{" <> show t <> "}"
  ELazy     -> "?"
  EFast     -> "_"
  ETape e   -> "$(" <> dispExp e <> ")"
  EMark     -> "@"
  EQuot x   -> "(" <> show x <> ")"

valExp :: Val -> Exp
valExp (valFor' -> Just e) = EQuot e
valExp VN                  = EPure VN
valExp VV                  = crash
valExp v                   = go v <> EPure VN
  where
    go = \case
      (valFor' → Just e) → EQuot e
      VV                 → crash
      VN                 → ESubj
      V0 VN              → ELeft
      V1 VN              → EWrit
      V0 l               → ELeft <> go l
      V1 r               → EWrit <> go r
      VP x y             → ECons (go x) (go y)
      VT x y _r          → ECons (go x) y
      VR k               → EMark <> go (getGrain k)

crash :: Exp
crash = EEval <> EEval <> (EPure VN)


-- Parsing ---------------------------------------------------------------------

parseSimpl :: String -> Maybe (Exp, String)
parseSimpl = \case
  '.' : xs -> pure (ESubj, xs)
  '~' : xs -> pure (EPure VN, xs) -- TODO EPure
  '!' : xs -> pure (EEval, xs)
  '0' : xs -> pure (ELeft, xs)
  '1' : xs -> pure (EWrit, xs)
  '-' : xs -> pure (EHead, xs)
  '+' : xs -> pure (ETail, xs)
  '%' : xs -> pure (EDist, xs)
  '?' : xs -> pure (ELazy, xs)
  '@' : xs -> pure (EMark, xs)
  _        -> Nothing

parseExp :: String -> Either String (Exp, String)
parseExp str = do
  case parseSimpl str of
    Just (e, xs) -> pure (e, xs)
    Nothing ->
      case str of
        '[':xs -> parseTwo ECons ']' xs
        '<':xs -> parseTwo ECase '>' xs
        '(':xs -> parseSeq ')' xs <&> \case
                    (e,cs) -> (EQuot e, cs)
        _      -> P.Left "bad"

parseSeq :: Char -> String -> Either String (Exp, String)
parseSeq end = go >=> \case
                 (Just x,  buf) -> pure (x, buf)
                 (Nothing, buf) -> P.Left "empty sequence"
  where
    go :: String -> Either String (Maybe Exp, String)
    go = \case
      []                -> pure (Nothing, [])
      c : cd | c == end -> pure (Nothing, cd)
      cs                -> do
        (x, buf) <- parseExp cs
        (y, buf) <- go buf
        case y of
          Nothing -> pure (Just x, buf)
          Just y  -> pure (Just (x <> y), buf)

parseTwo :: (Exp -> Exp -> Exp) -> Char -> String
         -> Either String (Exp, String)
parseTwo cntr end buf = do
  (xs, buf) <- parseSeq ' ' buf
  (ys, buf) <- parseSeq end buf
  pure (cntr xs ys, buf)
