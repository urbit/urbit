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
  (W apply ['Ess' ~] ['Kay' ~] ['Eee' 1] ['Dub' ~] code)

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

::  match-two-enhances: NE n :& NE 1            → Just $ NE (succ n)
::
=/  match-two-enhances
  |=  top
  %-  (trace ['match-two-eee' top])  |=  ig
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l  (car vtop)
    =/  r  (car vtop)
    =/  kl  (car l)
    ?:  (eql 'Eee' kl)
      =/  kr  (car r)
      ?:  (eql 'Eee' kr)
        =/  vr  (cdr r)
        ?:  (eql 1 vr)
          =/  vl  (cdr l)
          (rit (con 'Eee' (inc vl)))
        (lef ~)
      (lef ~)
    (lef ~)
  (lef ~)

=/  con-app
  |=  (l r)
  (con 'App' (con l r))

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

:: (foldl con-app base xs)

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
          ?-    (match-two-enhances top)
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

::  (K K K) -> (con 'App' (con (con 'App' (con (con 'Kay' ~) (con 'Kay' ~))) (con 'Kay' ~)))
::(reduce (con 'App' (con (con 'App' (con (con 'Kay' ~) (con 'Kay' ~))) (con 'Kay' ~))))

::  ((K K K) K K) -> (con 'App' (con (con 'App' (con (con 'App' (con (con 'App' (con (con 'Kay' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~)))
:: (eval (con 'App' (con (con 'App' (con (con 'App' (con (con 'App' (con (con 'Kay' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~))))


:: (((S K) K) K)
:: (con 'App' (con (con 'App' (con (con 'App' (con (con 'Ess' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~)))
::(eval (con 'App' (con (con 'App' (con (con 'App' (con (con 'Ess' ~) (con 'Kay' ~))) (con 'Kay' ~))) (con 'Kay' ~))))


:: (E E)
::(eval (con 'App' (con (con 'Eee' 1) (con 'Eee' 1))))


(foldl add 0 (lcon 1 (lcon 2 (lcon 3 lnil))))
