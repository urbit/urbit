module Deppy.Showings where

import ClassyPrelude

import Data.Function ((&))

import Deppy.CST
import qualified Deppy.Hoon as H
import qualified Deppy.Core as C
import qualified Noun as N

class Display a where
  display :: a -> String

instance Display (C.Exp Text) where
  display = display . H.resugar

instance Display (H.Hoon Text) where
  display = display . concretize

instance Display CST where
  display = \case
      Var t -> unpack t
      Hax -> "#"
      Fun bs x -> "<|" <> displayBound bs x <> "|>"
      Cel bs x -> "[|" <> displayBound bs x <> "|]"

      Wut (setToList -> [x]) -> displayTag "$" "$" x
      Wut (setToList -> xs) ->
        "?(" <> intercalate " " (displayTag "" "" <$> xs) <> ")"
      Lam bs x -> "<" <> displayBound bs x <> ">"
      Cns xs -> "[" <> displayGroup xs <> "]"
      Tag a -> displayTag "%" "" a
      App xs -> "(" <> displayGroup xs <> ")"
      Hed x -> "-." <> display x
      Tal x -> "+." <> display x
      The x y -> "`" <> display x <> "`" <> display y
      Fas x y -> display x <> "/" <> display y
      Obj (mapToList -> cs) -> "{" <> displayEnts cs <> "}"
      Cls (mapToList -> tcs) -> "{|" <> displayEnts tcs <> "|}"
      Col a x -> displayTag "" "" a <> ":" <> display x
      HaxBuc (mapToList -> cs) -> "$%(" <> displayEnts cs <> ")"
      HaxCen (mapToList -> cs) -> "$=(" <> displayEnts cs <> ")"
      HaxCol bs x -> "$:(" <> displayBound bs x <> ")"
      HaxHep bs x -> "$-(" <> displayBound bs x <> ")"
      BarCen (mapToList -> cs) -> "|%(" <> displayEnts cs <> ")"
      BarTis bs x -> "|=(" <> displayBound bs x <> ")"
      CenDot x y -> "%.(" <> display x <> " " <> display y <> ")"
      CenHep x y -> "%-(" <> display x <> " " <> display y <> ")"
      ColHep x y -> ":-(" <> display x <> " " <> display y <> ")"
      ColTar xs -> ":*(" <> displayGroup xs <> ")"
      TisFas t x y -> "=/(" <> unpack t <> " " <> display x <> " " <> display y <> ")"
      DotDot x y -> "..(" <> displayBinder x <> " " <> display y <> ")"
      DotGal x -> ".<(" <> display x <> ")"
      DotGar x -> ".>(" <> display x <> ")"
      KetFas x y -> "^/(" <> display x <> " " <> display y <> ")"
      KetHep x y -> "^-(" <> display x <> " " <> display y <> ")"
      WutCen x cs -> case mapToList cs of
        [] -> "?%(" <> display x <> ")"
        cs -> "?%(" <> display x <> "; " <> displayEnts' cs <> ")"
      WutHax x cs -> case mapToList cs of
        [] -> "?#(" <> display x <> ")"
        cs -> "?#(" <> display x <> "; " <> displayCelPats cs <> ")"
    where
      displayEnts  xs = intercalate ", " (displayEnt "" <$> xs)
      displayEnts' xs = intercalate ", " (displayEnt "%" <$> xs)
      displayEnt s (x, y) = displayTag s "" x <> " " <> display y
      displayGroup xs = intercalate " " (display <$> xs)
      displayTag p1 p2 x = N.fromNoun (N.A x) &
        \case
          Just (N.Cord x) | okay x -> p1 <> unpack x
          _ -> p2 <> show x
      okay = all (flip elem ['a'..'z'])
      displayBound bs x =  displayBinders bs <> " " <> display x
      displayBinders bs = intercalate " " (displayBinder <$> bs)
      displayBinder (Nothing, x) = display x
      displayBinder (Just t, x) = unpack t <> "/" <> display x
      displayCelPats xs = intercalate ", " (displayCelPat <$> xs)
      displayCelPat (a, (v, c)) = "["
                               <> displayTag "%" "" a
                               <> " "
                               <> unpack v
                               <> "] "
                               <> display c
   
