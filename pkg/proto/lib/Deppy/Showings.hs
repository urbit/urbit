module Deppy.Showings where

import ClassyPrelude

import Data.Function ((&))

import Deppy.CST
import qualified Deppy.Hoon as H
import qualified Deppy.Core as C
import qualified Noun as N

instance Show (C.Exp Text) where
  show = show . H.resugar

instance Show (H.Hoon Text) where
  show = show . concretize

instance Show CST where
  show = \case
      Var t -> unpack t
      Hax -> "#"
      Fun bs x -> "<|" <> showBound bs x <> "|>"
      Cel bs x -> "[|" <> showBound bs x <> "|]"

      Wut (setToList -> [x]) -> showTag "$" "$" x
      Wut (setToList -> xs) -> "?(" <> intercalate " " (showTag "" "" <$> xs) <> ")"
      Lam bs x -> "<" <> showBound bs x <> ">"
      Cns xs -> "[" <> showGroup xs <> "]"
      Tag a -> showTag "%" "" a
      App xs -> "(" <> showGroup xs <> ")"
      Hed x -> "-." <> show x
      Tal x -> "+." <> show x
      The x y -> "`" <> show x <> "`" <> show y
      Fas x y -> show x <> "/" <> show y
      Obj (mapToList -> cs) -> "{" <> showEnts cs <> "}"
      Cls (mapToList -> tcs) -> "{|" <> showEnts tcs <> "|}"
      Col a x -> showTag "" "" a <> ":" <> show x
      HaxBuc (mapToList -> cs) -> "$%(" <> showEnts cs <> ")"
      HaxCen (mapToList -> cs) -> "$=(" <> showEnts cs <> ")"
      HaxCol bs x -> "$:(" <> showBound bs x <> ")"
      HaxHep bs x -> "$-(" <> showBound bs x <> ")"
      BarCen (mapToList -> cs) -> "|%(" <> showEnts cs <> ")"
      BarTis bs x -> "|=(" <> showBound bs x <> ")"
      CenDot x y -> "%.(" <> show x <> " " <> show y <> ")"
      CenHep x y -> "%-(" <> show x <> " " <> show y <> ")"
      ColHep x y -> ":-(" <> show x <> " " <> show y <> ")"
      ColTar xs -> ":*(" <> showGroup xs <> ")"
      TisFas t x y -> "=/(" <> unpack t <> show x <> " " <> show y <> ")"
      DotDot x y -> "..(" <> showBinder x <> " " <> show y <> ")"
      KetFas x y -> "^/(" <> show x <> " " <> show y <> ")"
      KetHep x y -> "^-(" <> show x <> " " <> show y <> ")"
      WutCen x cs -> case mapToList cs of
        [] -> "?%(" <> show x <> ")"
        cs -> "?%(" <> show x <> "; " <> showEnts' cs <> ")"
    where
      showEnts  xs = intercalate ", " (showEnt "" <$> xs)
      showEnts' xs = intercalate ", " (showEnt "%" <$> xs)
      showEnt s (x, y) = showTag s "" x <> " " <> show y
      showGroup xs = intercalate " " (show <$> xs)
      showTag p1 p2 x = N.fromNoun (N.A x) &
        \case
          Just (N.Cord x) | okay x -> p1 <> unpack x
          _ -> p2 <> show x
      okay = all (flip elem ['a'..'z'])
      showBound bs x =  showBinders bs <> " " <> show x
      showBinders bs = intercalate " " (showBinder <$> bs)
      showBinder (Nothing, x) = show x
      showBinder (Just t, x) = unpack t <> "/" <> show x
   
