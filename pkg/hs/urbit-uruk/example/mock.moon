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

::  mock: we want to take the data in a skew tree from skew-layer and interpret
::  its reductions down to nothing. 
::
::  TODO: we must get this working with the lazy data structure, but for
::  simplicity, the first implementation uses the skewdata instead of skew-layer
::  representation.
::
=/  mock
  ..  $
  ::  top: a cons cell of a 
  |=  top
  %-  (trace ['one' 'start mock'])  |=  ig
  =/  ktop  (car top)
  %-  (trace ['ktop' ktop])  |=  ig
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    %-  (trace ['vtop' vtop])  |=  ig
    =/  l  (car vtop)
    =/  r  (cdr vtop)
    =/  kl  (car l)
    =/  vl  (cdr l)
    =/  ig  (trace ['kl' kl])
    ::(lef 'udnef')
    ?:  (eql 'App' kl)
      =/  kl2  (car vl)
      =/  kr2  (cdr vl)
      ?:  (eql 'Kay' kl2)
        (rit kr2)
      (lef 'other')
      :: =/  l2  ((car (cdr
      :: ?:  (eql 'Kay' right)
    (lef 'three')
  (lef 'not an app tree')

::             top  ktop  vtop l    kl    vl   l2    r2    
::  (K K K) -> (CON 'App' (CON (CON 'App' (CON 'Kay' 'Kay')) 'Kay'))

:: :: Ben had the great idea that we should move reduction rule format from 

:: no, argument gets evaluated
::(skewdata (K K K))
(mock (con 'App' (con (con 'App' (con 'Kay' 'Kay')) 'Kay')))


::(mock (skew-layer (K K K)))
:: (mock (skew-layer seq))
