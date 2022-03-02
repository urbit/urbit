module Practice.ForkKill where

import ClassyPrelude

import Practice.HoonCommon

data Semi
  = Stop
  | Atom Atom
  | Cell Semi Semi
  | Alts [Semi]


meld :: Semi -> Semi -> Maybe (Semi)
meld b c = case (b, c) of
  (Stop, _) -> pure c
  (_, Stop) -> pure b

  (Atom a, Atom b) | a == b -> pure (Atom a)
  (Atom{}, _) -> Nothing
  (_, Atom{}) -> Nothing

  (Alts bs, c) -> pure $ Alts $ catMaybes $ map (`meld` c) bs
  (b, Alts cs) -> pure $ Alts $ catMaybes $ map (b `meld`) cs

  (Cell x y, Cell x' y') -> Cell <$> meld x x' <*> meld y y'

diff :: Semi -> Semi -> Maybe (Semi)
diff b c = case (b, c) of
  (Stop, _) -> pure Stop
