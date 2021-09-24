module Rule where

import ClassyPrelude hiding (last, mask)

import Numeric.Natural

type Atom = Natural

type Rule r = Nail -> Edge r

type Nail = (Hair, Tape)

type Edge t = (Hair, Maybe (t, Nail))

type Tape = String

type Hair = (Atom, Atom)

data Hoon

trip :: Text -> String
trip = unpack

ream :: Text -> Hoon
ream txt = rash txt vest

rash :: Text -> Rule r -> r
rash nab sab = scan (trip nab) sab

scan :: String -> Rule r -> r
scan los sab = fst $ maybe (error "syntax error") id $ snd
             $ full sab ((1, 1), los)

-- | has to fully parse
full :: Rule r -> Rule r
full sef tub = case sef tub of
  (h, Nothing)           -> (h, Nothing)
  (h, Just (r, (j, ""))) -> (h, Just (r, (j, "")))
  (h, Just _)            -> (h, Nothing)

--
-- combinators
--

last :: (Atom, Atom) -> (Atom, Atom) -> (Atom, Atom)
last (p, q) (p', q') = undefined

easy :: a -> Rule a
easy huf = \(p, q) -> (p, Just (huf, (p, q)))

comp :: (a -> b -> r) -> Edge a -> Rule b -> Edge r
comp raq vex sab = case vex of
  (h, Nothing) -> (h, Nothing)
  (h, Just (p, q)) ->
    let yit = sab q
    in case yit of
      (g, Nothing) -> (last h g, Nothing)
      (g, Just (p', q')) -> (last h g, Just (raq p p', q'))

-- | first or second
pose :: Edge a -> Edge a -> Edge a
pose vex sab = case vex of
  (h, Nothing) -> (last h (fst sab), snd sab)
  vex -> vex

-- | replace with constant
cold :: forall a b. a -> Rule b -> Rule a
cold cus rule = \tub -> case rule tub of
  (h, Nothing) -> (h, Nothing)
  (h, Just (p, q)) -> (h, Just (cus, q))

-- | first then second
plug :: Edge a -> Rule b -> Edge (a, b)
plug = comp (\a b -> (a, b))

-- | zero or more times
star :: Rule a -> Rule [a]
star fel = stir [] (\a b -> a:b) fel

stir :: forall a r. r -> (a -> r -> r) -> Rule a -> Rule r
stir zer fun rul = \tub -> case rul tub of
  (h, Nothing) -> (h, Nothing)
  (h, Just (p, q)) -> stir (fun p zer) fun rul q

pfix :: Edge r -> Rule s -> Edge s
pfix vex sab = comp (\a b -> b) vex sab

sfix :: Edge r -> Rule s -> Edge r
sfix = comp (\a b -> a)

-- | infix
ifix :: (Rule r, Rule s) -> Rule t -> Rule t
ifix (pfel, qfel) hof = \x -> pfix (pfel x) $ \x -> sfix (hof x) qfel

-- | match char in set
mask :: [Char] -> Rule Char
mask chs = undefined

--
-- hoon parser
--

vest :: Rule Hoon
vest = full (ifix (gay, gay) (tall vast))

tall = undefined

gay = \x -> pose (gap x) (easy () x)

-- | plural space
gap = cold () \x -> plug (gaq x) (star \x -> pose (vul x) (gah x))

gaq = undefined

vul = undefined

gah = mask ['\n', ' ']

vast = undefined