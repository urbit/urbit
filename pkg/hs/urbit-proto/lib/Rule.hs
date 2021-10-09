module Rule where

import ClassyPrelude hiding (last, mask, pred)

import Numeric.Natural

type Atom = Natural

-- | utf8 string as list
type Tape = String

-- | parsing trace
type Hair = (Atom, Atom)

-- | parsing input
type Nail = (Hair, Tape)

-- | parsing output
-- (why does it include an input?)
type Edge t = (Hair, Maybe (t, Nail))

-- | parsing rule
newtype Rule r = Rule { (!) :: Nail -> Edge r }
  deriving Functor

data Hoon

-- | cord to tape
trip :: Text -> String
trip = unpack

-- | parse cord to hoon
ream :: Text -> Hoon
ream txt = rash txt vest

rash :: Text -> Rule r -> r
rash nab sab = scan (trip nab) sab

scan :: String -> Rule r -> r
scan los sab = fst $ maybe (error "syntax error") id $ snd
             $ full sab ! ((1, 1), los)

-- | has to fully parse
full :: Rule r -> Rule r
full sef = Rule \tub -> case sef ! tub of
  (h, Nothing)           -> (h, Nothing)
  (h, Just (r, (j, ""))) -> (h, Just (r, (j, "")))
  (h, Just _)            -> (h, Nothing)

--
-- combinators
--

-- | farther trace
last :: (Atom, Atom) -> (Atom, Atom) -> (Atom, Atom)
last = max

-- We can write Applicative for Rule but not Edge (what is `pure`?)
instance Applicative Rule where
  pure huf = Rule \(p, q) -> (p, Just (huf, (p, q)))

  rf <*> rx = Rule \tub -> case rf ! tub of
    (h, Nothing)       -> (h, Nothing)
    (h, Just (f, lub)) ->
      case rx ! lub of
        (g, Nothing)       -> (last h g, Nothing)
        (g, Just (x, dub)) -> (last h g, Just (f x, dub))

-- We can write Alternative for Rule but not Edge (what is `empty`?)
instance Alternative Rule where
  empty = Rule \(p, _) -> (p, Nothing)

  vex <|> sab = Rule \tub -> case vex ! tub of
    (h, Nothing) -> let (g, naf) = sab ! tub
                    in (last h g, naf)
    gef -> gef

instance Monad Rule where
  rx >>= f = Rule \tub -> case rx ! tub of
    (h, Nothing) -> (h, Nothing)
    (h, Just (x, lub)) -> case f x ! lub of
      (g, Nothing) -> (last g h, Nothing)
      (g, Just (y, dub)) -> (last g h, Just (y, dub))

easy :: a -> Rule a
easy huf = Rule \(p, q) -> (p, Just (huf, (p, q)))

-- | never parse
fail :: Rule r
fail = Rule \(p, _) -> (p, Nothing)

-- | detect newline
lust :: Char -> Hair -> Hair
lust '\n' (r, c) = (r + 1, 1)
lust _    (r, c) = (r, c + 1)

-- | consume a char
next :: Rule Char
next = Rule \case
  tub@(p, [])   -> fail ! tub
  tub@(p, c:cs) -> (zac, Just (c, (zac, cs))) where zac = lust c p

-- | arbitrary compose
comp :: (a -> b -> r) -> Rule a -> Rule b -> Rule r
comp f a b = f <$> a <*> b
-- comp :: (a -> b -> r) -> Edge a -> Rule b -> Edge r
-- comp raq vex sab = case vex of
--   (h, Nothing) -> (h, Nothing)
--   (h, Just (p, q)) ->
--     let yit = sab q
--     in case yit of
--       (g, Nothing) -> (last h g, Nothing)
--       (g, Just (p', q')) -> (last h g, Just (raq p p', q'))

-- | first or second
pose :: Edge a -> Edge a -> Edge a
pose vex sab = case vex of
  (h, Nothing) -> (last h (fst sab), snd sab)
  vex -> vex

-- | no[t] first and second
-- i.e. negative lookahead
less :: Rule a -> Rule b -> Rule b
less vex sab = Rule \tub -> case (vex ! tub, sab ! tub) of
  -- XX why is it appropriate to use `last` here?
  ((h, Nothing), (g, roq)) -> (last h g, roq)
  ((_, Just{}),  _)        -> fail ! tub


-- | replace with constant
cold :: forall a b. a -> Rule b -> Rule a
cold cus rul = const cus <$> rul
-- cold cus rule = \tub -> case rule tub of
--   (h, Nothing) -> (h, Nothing)
--   (h, Just (p, q)) -> (h, Just (cus, q))

-- | first then second
plug :: Rule a -> Rule b -> Rule (a, b)
plug = comp (\a b -> (a, b))

-- | zero or more times
star :: Rule a -> Rule [a]
star fel = stir [] (\a b -> a:b) fel

{-
++  stir
  ~/  %stir
  |*  [rud=* raq=_=>(~ |*([a=* b=*] [a b])) fel=rule]
  ~/  %fun
  |=  tub=nail
  ^-  (like _rud)
  ::
  ::  lef: successful interim parse results (per .fel)
  ::  wag: initial accumulator (.rud in .tub at farthest success)
  ::
  =+  ^=  [lef wag]
    =|  lef=(list _(fel tub))
    |-  ^-  [_lef (pair hair [~ u=(pair _rud nail)])]
    =+  vex=(fel tub)
    ?~  q.vex
      :-  lef
      [p.vex [~ rud tub]]
    $(lef [vex lef], tub q.u.q.vex)
  ::
  ::  fold .lef into .wag, combining results with .raq
  ::
  %+  roll  lef
  |=  _[vex=(fel tub) wag=wag]  :: q.vex is always (some)
  ^+  wag
  :-  (last p.vex p.wag)
  [~ (raq p.u.+.q.vex p.u.q.wag) q.u.q.wag]
-}
stir :: r -> (a -> r -> r) -> Rule a -> Rule r
stir zer raq rul = loop where loop = (raq <$> rul <*> loop) <|> pure zer 
{-stir zer fun rul = \tub -> case rul tub of
  (h, Nothing) -> (h, Nothing)
  (h, Just (p, q)) -> stir (fun p zer) fun rul q-}

pfix :: Rule r -> Rule s -> Rule s
pfix vex sab = comp (\a b -> b) vex sab

sfix :: Rule r -> Rule s -> Rule r
sfix = comp (\a b -> a)

-- | infix
ifix :: (Rule r, Rule s) -> Rule t -> Rule t
-- ifix (pfel, qfel) hof = \x -> pfix (pfel x) $ \x -> sfix (hof x) qfel
ifix (pfel, qfel) hof = pfix pfel (sfix hof qfel)

-- | match a char according to predicate
-- my own invention lol
pred :: (Char -> Bool) -> Rule Char
pred ped = Rule \case
  tub@(_, c:cs) | ped c -> next ! tub
  tub -> fail ! tub

-- | match a char
just :: Char -> Rule Char
just daf = pred (== daf)

-- | match char in set
mask :: [Char] -> Rule Char
mask bud = pred (`elem` bud)

-- | match char in range
shim :: Char -> Char -> Rule Char
shim les mos = pred \c -> les <= c && c <= mos
--
-- ascii glyphs
--

ace = just ' '
bar = just '|'
bas = just '\\'
buc = just '$'
cab = just '_'
cen = just '%'
col = just ':'
com = just ','
doq = just '"'
dot = just '.'
fas = just '/'
gal = just '<'
gar = just '>'
hax = just '#'
hep = just '-'
kel = just '{'
ker = just '}'
ket = just '^'
lus = just '+'
mic = just ';'
pal = just '('
pam = just '&'
par = just ')'
pat = just '@'
sel = just '['
ser = just ']'
sig = just '~'
soq = just '\''
tar = just '*'
tic = just '`'
tis = just '='
wut = just '?'
zap = just '!'

--
-- parsing (useful idioms)
--

-- | plural space
gap = cold () $ gaq >> star (vul <|> gah)
--cold () \x -> plug (gaq x) (star \x -> pose (vul x) (gah x))

gah = cold () $ mask ['\n', ' ']

-- | end of line
gaq = () <$ just '\n' <|> (gah >> (gah <|> vul)) <|> vul

gay :: Rule ()
gay = gap <|> easy ()

-- | non-control
prn = less (just '\DEL') (shim '\32' '\256')

-- | comments
vul = cold () $ col >> col >> star prn >> just '\n'

--
-- hoon parser
--

vest :: Rule Hoon
vest = full (ifix (gay, gay) (tall vast))

tall = undefined

vast = undefined