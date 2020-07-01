::  Uruk +mock
::

::  runtime base letter representations
=/  app
  |=  (l r)
  (con 'App' (con l r))
=/  ess  ['Ess' ~]
=/  kay  ['Kay' ~]
=/  enh  ['Enh' ~]
=/  dub  ['Dub' ~]

::  Uses W to change a skew value into a tree representation.
::
::  TODO: Make App return two closures so we can represent lazily evaluate the
::  skew. This structure builds the entire tree strictly, including things like
::  natural numbers and that makes it infeasible.
::
=/  to-skewdata
  ~/  1  to-skewdata
  ..  $
  |=  code
  =/  apply
    |=  (x y)
    (app ($ x) ($ y))
  (W apply ess kay enh dub code)

::  Converts a value from the data form back to raw skew
::
=/  from-skewdata
  ~/  1  from-skewdata
  ..  $
  |=  data
  =/  ktop  (car data)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr data)
    =/  lval  (car vtop)
    =/  rval  (cdr vtop)
    ?-    ($ lval)
        err
      (lef ~)
    ::
        lret
      ?-    ($ rval)
          err
        (lef ~)
      ::
          rret
        (rit (lret rret))
      ==
    ==
  ?:  (eql 'Ess' ktop)
    (rit S)
  ?:  (eql 'Kay' ktop)
    (rit K)
  ?:  (eql 'Enh' ktop)
    (rit E)
  ?:  (eql 'Dub' ktop)
    (rit E)
  (lef ~)

:: Tape to cord
=/  crip
  ~/  1  crip
  (rap 8)

::  renders a skewdata as a string of the Uruk for easier reading
::
=/  skewdata-to-string
  ~/  1  skewdata-to-string
  |=  top
  =/  get
    ..  $
    |=  data
    =/  ktop  (car data)
    ?:  (eql 'App' ktop)
      =/  vtop  (cdr data)
      =/  lval  (car vtop)
      =/  rval  (cdr vtop)
      %+  weld  (lcon '(' lnil)
      %+  weld  ($ lval)
      %+  weld  (lcon ' ' lnil)
      %+  weld  ($ rval)
      (lcon ')' lnil)
    ?:  (eql 'Ess' ktop)
      (lcon 'S' lnil)
    ?:  (eql 'Kay' ktop)
      (lcon 'K' lnil)
    ?:  (eql 'Enh' ktop)
      (lcon 'E' lnil)
    ?:  (eql 'Dub' ktop)
      (lcon 'W' lnil)
    lnil
  (crip (get top))

::  foldl should really be in the stdlib.
::
=/  foldl
  ~/  3  foldl
  ..  $
  |=  (fun base rest)
::  %-  (trace ['foldl start' rest])  |=  ig
  %+  (cas rest)
    |=  p
::    %-  (trace 'foldl fun')  |=  ig
    ($ fun (fun base (car p)) (cdr p))
  |=  u
::  %-  (trace 'foldl base')  |=  ig
  base


=/  foldr
  ~/  3  foldr
  ..  $
  |=  (fun base rest)
  %+  (cas rest)
    <p (fun (car p) ($ fun base (cdr p)))>
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
::        %-  (trace ['match-kay' (skewdata-to-string top)])  |=  ig
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
::      %-  (trace ['match-left' (skewdata-to-string top)])  |=  ig
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
::      %-  (trace ['match-right' (skewdata-to-string top)])  |=  ig
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
::          %-  (trace ['match-ess' (skewdata-to-string top)])  |=  ig
          (rit (con 'App' (con (con 'App' (con x z)) (con 'App' (con y z)))))
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


=/  from-list-form
  ..  $
  |=  list
  =/  append
    |=  (fst snd)
    ?-    fst
        l
      (rit snd)
    ::
        rfst
      (rit (app rfst snd))
    ==
::  %-  (trace 'from-list-form')  |=  ig    
  (foldl append (lef ~) list)


::  todo: this is defined in JetSpec, why does this not show up here?
::
=/  gte
  |=  (x y)
  (lth y x)

::  todo: list length should be in the standard library
::
=/  lent
  ~/  1  lent
  ..  $
  |=  n
  %+  (cas n)
    <p (inc ($ (cdr p)))>
  <u 0>

::  given a list form, returns lef if this isn't a jet and (rit (con nat xs))
::  if this is a jet argument count.
::
=/  m-e-count-arguments
  ~/  1  m-e-count-arguments
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
  ~/  1  m-e-get-jet
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
  ~/  1  match-enhance
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
::        %-  (trace ['match-enhance' (skewdata-to-string top)])  |=  ig
        (rit (foldl app jet-body xs))
      (lef ~)
    ==
  ==

=/  match-switch
  ~/  1  match-switch
  |=  top
  =/  ktop  (car top)
  ?:  (eql 'App' ktop)
    =/  vtop  (cdr top)
    =/  l  (car vtop)
    =/  c  (cdr vtop)
    =/  kl  (car l)
    ?:  (eql 'App' kl)
      =/  vl  (cdr l)
      =/  l2  (car vl)
      =/  w  (cdr vl)
      =/  kl2  (car l2)
      ?:  (eql 'App' kl2)
        =/  vvl  (cdr l2)
        =/  l3  (car vvl)
        =/  e  (cdr vvl)
        =/  kl3  (car l3)
        ?:  (eql 'App' kl3)
          =/  vvl  (cdr l3)
          =/  l4  (car vvl)
          =/  k  (cdr vvl)
          =/  kl4  (car l4)
          ?:  (eql 'App' kl4)
            =/  vvl  (cdr l4)
            =/  l5  (car vvl)
            =/  s  (cdr vvl)
            =/  kl5  (car l5)
            ?:  (eql 'App' kl5)
              =/  vvl  (cdr l5)
              =/  l6  (car vvl)
              =/  a  (cdr vvl)
              =/  kl6  (car l6)
              ?:  (eql 'Dub' kl6)
                =/  kc  (car c)
                ?:  (eql 'App' kc)
::                  %-  (trace ['match-dub-app' a])  |=  ig
                  =/  vc  (cdr c)
                  =/  x   (car vc)
                  =/  y   (cdr vc)
                  (rit (app (app a x) y))
                ?:  (eql 'Ess' kc)
::                  %-  (trace ['match-dub-ess' s])  |=  ig
                  (rit s)
                ?:  (eql 'Kay' kc)
::                  %-  (trace ['match-dub-kay' k])  |=  ig
                  (rit k)
                ?:  (eql 'Enh' kc)
::                  %-  (trace ['match-dub-enh' e])  |=  ig
                  (rit e)
                ?:  (eql 'Dub' kc)
::                  %-  (trace ['match-dub-dub' w])  |=  ig
                  (rit w)
                (lef ~)
              (lef ~)
            (lef ~)
          (lef ~)
        (lef ~)
      (lef ~)
    (lef ~)
  (lef ~)


::  reduce: performs one step of reduction, returning rit if you can reduce and
::  lef if you can't
::
::  TODO: we must get this working with the lazy data structure, but for
::  simplicity, the first implementation uses the skewdata instead of skew-layer
::  representation.
::
=/  reduce
  ~/  1  reduce
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
            ?-    (match-switch top)
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
  ::
      r
    (rit r)
  ==

::  eval: perform step evaluation until no more are possible.
::
=/  eval
  ~/  1  eval
  ..  $
  |=  x
  ?-    (reduce x)
      l
    x
  ::
      r
    ($ r)
  ==

::  mock: given a function and a list of arguments, try evaluating it.
::
=/  mock
  ~/  2  mock
  |=  (fun args)
  =/  fun-data  (to-skewdata fun)
  =/  args-data  (turn args to-skewdata)
  ?-    (from-list-form (lcon fun-data args-data))
      l
    l
  ::
      expr
    (from-skewdata (eval expr))
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
::(eval seq-with-kk)


::  (W 0 1 2 3 4 K)
=/  zero   (to-skewdata 0)
=/  one    (to-skewdata 1)
=/  two    (to-skewdata 2)
=/  three  (to-skewdata 3)
=/  four   (to-skewdata 4)
=/  dub-check-k  (app (app (app (app (app (app dub zero) one) two) three) four) ess)
::  (eval dub-check-k)
::  (from-skewdata (eval dub-check-k))


:: +mock: returns (lef err) or (rit result) when passed fun and a list of args.
::
=/  seq-false-tag-raw  (E E K (S K))
(mock seq-false-tag-raw (lcon 8 (lcon 3 lnil)))
