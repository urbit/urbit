:: This was a good first try, but to really work, we need this to be lazy.
::
=/  skewdata
  ..  $
  |=  code
  =/  apply
    |=  (x y)
    ['App' ($ x) ($ y)]
  (W apply 'Ess' 'Kay' 'Eee' 'Dub' code)

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
  (W apply ['Ess' ~] ['Kay' ~] ['Eee' ~] ['Dub' ~] code)

::  match-k: (NK :& x :& y) -> Just $ x
::
=/  match-kay
  |=  top
  %-  (trace ['match-kay' top])  |=  ig
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l  (car vtop)
    =/  r  (cdr vtop)
    =/  kl  (car l)
    %-  (trace ['match-kay-kl' kl])  |=  ig
    ?:  (eql 'App' kl)
      =/  vl  (cdr l)
      =/  l2  (car vl)
      =/  r2  (cdr vl)
      =/  kl2  (car l2)
      %-  (trace ['match-kay-kl2' kl2])  |=  ig
      ?:  (eql 'Kay' kl2)
        (rit r2)
      (lef ~)
    (lef ~)
  (lef ~)

::  match-left: (reduce → Just xv) :& y → Just $ xv :& y
::
=/  match-left
  |=  (reduce top)
  %-  (trace ['match-left' top])  |=  ig
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l-vtop  (car vtop)
    ?-    (reduce l-vtop)
        l
      (lef l)
        r
      =/  r-vtop  (cdr vtop)
      (rit (con 'App' (con r r-vtop)))
    ==
  (lef ~)

::  match-right: x :& (reduce → Just yv) → Just $ x :& yv
::
=/  match-right
  |=  (reduce top)
  %-  (trace ['match-right' top])  |=  ig
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
      (rit (con 'App' (con l-vtop r)))
    ==
  (lef ~)

::  match-ess: NS :& x :& y :& z       → Just $ x :& z :& (y :& z)
::          (((       )    )    )
=/  match-ess
  |=  top
  %-  (trace ['match-ess' top])  |=  ig
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l  (car vtop)
    =/  z  (cdr vtop)
    =/  kl  (car l)
    %-  (trace ['match-ess-kl' kl])  |=  ig
    ?:  (eql 'App' kl)
      =/  vl  (cdr l)
      =/  l2  (car vl)
      =/  y  (cdr vl)
      =/  kl2  (car l2)
      %-  (trace ['match-ess-kl2' kl2])  |=  ig
      ?:  (eql 'App' kl2)
        =/  vvl  (cdr l2)
        =/  l3  (car vvl)
        =/  x  (cdr vvl)
        %-  (trace ['match-ess-z' z])  |=  ig     :: r  is z
        %-  (trace ['match-ess-y' y])  |=  ig   :: r2 is y
        %-  (trace ['match-ess-x' x])  |=  ig   :: r3 is x
        (rit (con 'App' (con (con 'App' (con x y)) (con 'App' (con y z)))))
      (lef ~)
    (lef ~)
  (lef ~)


:: ((((S K) K) K)
:: (con 'App' (con (con 'App' (con (con 'App' (con (con 'Ess' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~)))


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

::  (K K K) -> (con 'App' (con (con 'App' (con (con 'Kay' ~) (con 'Kay' ~))) (con 'Kay' ~)))
::(reduce (con 'App' (con (con 'App' (con (con 'Kay' ~) (con 'Kay' ~))) (con 'Kay' ~))))

::  ((K K K) K K) -> (con 'App' (con (con 'App' (con (con 'App' (con (con 'App' (con (con 'Kay' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~)))
:: (eval (con 'App' (con (con 'App' (con (con 'App' (con (con 'App' (con (con 'Kay' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~))))


:: (((S K) K) K)
:: (con 'App' (con (con 'App' (con (con 'App' (con (con 'Ess' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~)))
(eval (con 'App' (con (con 'App' (con (con 'App' (con (con 'Ess' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~))))
