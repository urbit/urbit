:: This was a good first try, but to really work, we need this to be lazy.
::
:: TODO: Try to pattern match natural numbers? Any real implementation of this
:: needs to be jetted and matching church numerals becomes efficientish.
::
=/  skewdata
  ..  $
  |=  code
  =/  apply
    |=  (x y)
    ['App' ($ x) ($ y)]
  (W apply ['Ess' ~] ['Kay' ~] ['Enh' 1] ['Dub' ~] code)


=/  app
  |=  (l r)
  (con 'App' (con l r))

=/  ess  ['Ess' ~]
=/  kay  ['Kay' ~]
=/  enh  ['Enh' ~]
=/  dub  ['Dub' ~]


::(skewdata 5)   :: ok
::(skewdata seq) :: ok
::(skewdata (E 'tag' (K K))) :: not ok, 'tag' is huge
::(skewdata skewdata)        :: not ok, don't understand why not.

::  skewtree: we want a lazy recursive data structure here.
::
=/  skew-layer
  ..  $
  |=  code
  =/  recur
    |=  (nucode u)
    ($ nucode)
  =/  apply
    |=  (x y)
    ['App' (recur x) (recur y)]
  (W apply ['Ess' ~] ['Kay' ~] ['Enh' ~] ['Dub' ~] code)


::  foldl should really be in the stdlib.
::
=/  foldl
  ~/  3  foldl
  ..  $
  |=  (fun base rest)
  %+  (cas rest)
    <p ($ fun (fun base (car p)) (cdr p))>
  <u base>


::  match-k: (NK :& x :& y) -> Just $ x
::
=/  match-kay
  |=  top
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l  (car vtop)
    =/  r  (cdr vtop)
    =/  kl  (car l)
    ?:  (eql 'App' kl)
      =/  vl  (cdr l)
      =/  l2  (car vl)
      =/  r2  (cdr vl)
      =/  kl2  (car l2)
      ?:  (eql 'Kay' kl2)
        %-  (trace ['match-kay' top])  |=  ig
        (rit r2)
      (lef ~)
    (lef ~)
  (lef ~)

::  match-left: (reduce → Just xv) :& y → Just $ xv :& y
::
=/  match-left
  |=  (reduce top)
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l-vtop  (car vtop)
    ?-    (reduce l-vtop)
        l
      (lef l)
        r
      =/  r-vtop  (cdr vtop)
      %-  (trace ['match-left' top])  |=  ig
      (rit (con 'App' (con r r-vtop)))
    ==
  (lef ~)

::  match-right: x :& (reduce → Just yv) → Just $ x :& yv
::
=/  match-right
  |=  (reduce top)
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  r-vtop  (cdr vtop)
    ?-    (reduce r-vtop)
        l
      (lef l)
    ::
        r
      =/  l-vtop  (car vtop)
      %-  (trace ['match-right' top])  |=  ig
      (rit (con 'App' (con l-vtop r)))
    ==
  (lef ~)

::  match-ess: NS :& x :& y :& z       → Just $ x :& z :& (y :& z)
=/  match-ess
  |=  top
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l  (car vtop)
    =/  z  (cdr vtop)
    =/  kl  (car l)
    ?:  (eql 'App' kl)
      =/  vl  (cdr l)
      =/  l2  (car vl)
      =/  y  (cdr vl)
      =/  kl2  (car l2)
      ?:  (eql 'App' kl2)
        =/  vvl  (cdr l2)
        =/  l3  (car vvl)
        =/  x  (cdr vvl)
        =/  kl3  (car l3)
        ?:  (eql 'Ess' kl3)
          %-  (trace ['match-ess' top])  |=  ig
          (rit (con 'App' (con (con 'App' (con x y)) (con 'App' (con y z)))))
        (lef ~)
      (lef ~)
    (lef ~)
  (lef ~)


:: jet matching works on an arbitrarily deep tree:
::
:: (((E T) B) x)
:: (((((E E) T) B) x) y)
::
:: The Reference.hs implementation matches in two steps: a) read downwards to
:: collect Es into an arity count and a list of arguments; b) ignore the tag,
:: grab the body and the rest, and if the length of the rest is the same as the
:: count, proceed to instead reconstruct the list with foldl ((b x1) x2)...

:: (foldl app base xs)


::  given an 'App' tree form, turn things into a list, where the deepest node
::  is the leftmost list item.
::
::  (((1 2) 3) 4) -> [1 2 3 4 ~]
::
=/  to-list-form
  ..  $
  |=  top
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l  (car vtop)
    =/  r  (cdr vtop)
    =/  left-recur  ($ l)
    (weld left-recur (lcon r lnil))
  (lcon top lnil)

::  todo: this is defined in JetSpec, why does this not show up here?
::
=/  gte
  |=  (x y)
  (lth y x)

::  todo: list length should be in the standard library
::
=/  lent
  ..  $
  |=  n
  %+  (cas n)
    <p (inc ($ (cdr p)))>
  <u 0>

::  given a list form, returns lef if this isn't a jet and (rit (con nat xs))
::  if this is a jet argument count.
::
=/  m-e-count-arguments
  ..  $
  |=  (count l)
  %+  (cas l)
    |=  p
    ?:  (eql 'Enh' (car (car p)))
      ($ (inc count) (cdr p))
    ?:  (gte count 1)
      (rit (con count (lcon (car p) (cdr p))))
    (lef ~)
  ::
  <u (lef ~)>

::  unpack [tag body rest], returning (rit (con body rest)) if valid.
::
=/  m-e-get-jet
  ..  $
  |=  top
  %+  (cas top)
    |=  a
    %+  (cas (cdr a))
      |=  b
      %+  (cas (cdr b))
        |=  c
        (rit (con (car b) (lcon (car c) (cdr c))))
      <u (lef ~)>
    <u (lef ~)>
  <u (lef ~)>

::  replaces a jet statement with a series of applications when the jet is
::  saturated with arguments.
::
=/  match-enhance
  |=  top
  =/  as-list  (to-list-form top)
  ?-    (m-e-count-arguments 0 as-list)
      l
    (lef ~)
  ::
      r
    =/  n  (car r)
    =/  rest  (cdr r)
    ?-    (m-e-get-jet rest)
        l
      (lef ~)
    ::
        r
      =/  jet-body  (car r)
      =/  xs        (cdr r)
      =/  xs-len    (lent xs)
      ?:  (eql n xs-len)
        %-  (trace ['match-enhance' top])  |=  ig
        (rit (foldl app jet-body xs))
      (lef ~)
    ==
  ==

::  reduce: performs one step of reduction, returning rit if you can reduce and
::  lef if you can't
::
::  TODO: we must get this working with the lazy data structure, but for
::  simplicity, the first implementation uses the skewdata instead of skew-layer
::  representation.
::
=/  reduce
  ..  $
  ::  top: a cons cell of a
  |=  top
  ?-    (match-kay top)
      l
    ?-    (match-left $ top)
        l
      ?-    (match-right $ top)
          l
        ?-    (match-ess top)
            l
          ?-    (match-enhance top)
              l
            (lef 'no such match')
          ::
              r
            (rit r)
          ==
        ::
            r
          (rit r)
        ==
      ::
          r
        (rit r)
      ==
    ::
        r
      (rit r)
    ==
  ::
      r
    (rit r)
  ==

::  eval: perform step evaluation until no more are possible.
::
=/  eval
  ..  $
  |=  x
  ?-    (reduce x)
      l
    x
  ::
      r
    ($ r)
  ==

::  (K K K)
=/  kkk  (app (app kay kay) kay)
::(reduce kkk)

::  ((K K K) K K)
=/  kkk-kk  (app (app (app (app kay kay) kay) kay) kay)
::(eval kkk-kk)


:: (((S K) K) K)
=/  sk-k-k  (app (app (app ess kay) kay) kay)
::(eval sk-k-k)
::(to-list-form sk-k-k)


:: (((E E) K) (S K))
=/  seq-false-tag  (app (app (app enh enh) kay) (app ess kay))
=/  seq-with-kk  (app (app seq-false-tag kay) kay)
::(eval seq-false-tag)
::(to-list-form seq-false-tag)
(eval seq-with-kk)
