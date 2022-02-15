-- {-# LANGUAGE OverlappingInstances, UndecidableInstances #-}

module Practice.TopLevelDL where

{-
import ClassyPrelude

import Bound
import qualified Data.Foldable as F
import Data.Void

import Practice.DependentLambda
import Practice.Hoon2DependentLambda
import Practice.HoonCommon
import Practice.HoonSyntax
import Practice.Render

data Err
  = ErrRead Text
  | ErrOpen Text
  | ErrFree [Term]
  | ErrType ([Act], Fail)

instance Rolling Err where
  roll = \case
    ErrRead t -> roll t
    ErrOpen t -> roll t
    ErrFree t -> leaf $ "free variables: " <> tshow t
    ErrType t -> roll t

road baseName txt = (cst, cod, val, typ)
 where
  cst = first ErrRead $ parse vest baseName txt
  cod = do
    c <- cst
    o <- first ErrOpen $ open c
    maybe (Left $ ErrFree $ F.toList o) Right $ closed o
  val = undefined --eval absurd <$> cod
  typ = do
    c <- cod
    first ErrType $ runReaderT (play (Con absurd absurd absurd) c) []

ride :: Text -> Code Void
ride t = let (_, Right c, _, _) = road "" t in c

instance Show a => Rolling a where
  roll = leaf . tshow
-}
