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
::          (((       )    )    )
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

::  match-two-enhances: NE n :& NE 1            → Just $ NE (succ n)
::
=/  match-two-enhances
  |=  top
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l  (car vtop)
    =/  r  (car vtop)
    =/  kl  (car l)
    ?:  (eql 'Enh' kl)
      =/  kr  (car r)
      ?:  (eql 'Enh' kr)
        =/  vr  (cdr r)
        ?:  (eql 1 vr)
          =/  vl  (cdr l)
          %-  (trace ['match-two-eee' top])  |=  ig
          (rit (con 'Enh' (inc vl)))
        (lef ~)
      (lef ~)
    (lef ~)
  (lef ~)

::  match-enhance: NE n :& t :& b          → Just $ NM (match n t b) (fromIntegral n) []
::
::  =/  match-enhance
::    |=  top
::    ::  ok, I don't entirely understand what's going on in the


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



=/  match-enhance
  |=  top
  (lef ~)


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
  %-  (trace ['reduce' top])  |=  ig
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
(eval seq-false-tag)


::(foldl add 0 (lcon 1 (lcon 2 (lcon 3 lnil))))
