::
::  Jⁿ for convenience
::

=/  JJ  (J J)
=/  JJJ  (J J J)
=/  JJJJ  (J J J J)


::
::  Olegs combinators.
::

=/  I  (J K (S K K))
=/  B  (JJJ K (S (K S) K))
=/  C  (JJJ K (S (K (S (K (S S (K K))) K)) S))


::
::  Booleans
::

=/  ya  (JJ S K)
=/  no  (JJ S (S K))


::
::  Church Encoded Naturals
::

=/  zero  (S K)
=/  succ  (S (S (K S) K))


::
::  Jetted Naturals
::

=/  pak  (J 16 (S (K (JJ K)) (S (S I (K succ)) (K zero))))
=/  inc  (J 1 (S (K pak) (S (S (K S) K))))
=/  zeo  (JJ K zero)
=/  one  (inc zeo)
=/  two  (inc one)
=/  thr  (inc two)
=/  for  (inc thr)
=/  fiv  (inc for)
=/  six  (inc fiv)
=/  sev  (inc six)
=/  ate  (inc sev)
=/  nin  (inc ate)


::
::  Pairs
::

=/  car  (J 13 (S I (K K)))
=/  cdr  (J 14 (S I (K (S K))))
=/  con
  %-  (JJJ 12)
  (S (K (S (K (S (K (S (K (S S (K K))) K)) S)) (S I))) K)


::
::  Control Flow
::

=/  seq  (JJ 17 (S K))
=/  W1  (JJ I I)
=/  W2  (JJJ I I)
=/  W3  (JJJJ I I)


::
::  Recursion
::

=/  Z
  %*  S
    (S (S (K S) K) (K (S W2 I)))
    (S (S (K S) K) (K (S W2 I)))
  ==

=/  fix
  %-  (JJ K)
  %-  (S I)
  %*  W2
    ((S (K ((S (K (JJ K))) (S I)))) ((S W2) I))
    ((S (K ((S (K (JJ K))) (S I)))) ((S W2) I))
  ==

::
::  Sums
::

=/  lef
  %-  (JJJ 9)
  (S (K (S (K (S (K K))) (S I))) K)

=/  rit
  %-  (JJJ 10)
  (S (K (S (K K) (S I))) K)

=/  cas
  %-  (JJJ 15)
  I


::
::  Atom Operations
::

=/  dec
  %-  (J 3)
  (S (S I (K (S (S cas (K (K (rit 0)))) (K (S (K rit) inc))))) (K (lef ~)))

=/  fec
  %-  (J 3)
  (S (S (S (K cas) dec) (K (K 0))) (K I))

=/  add
  %-  (JJ K)
  (S (K (S (K pak))) (S (K S) (S (K (S (K S) K)))))

=/  mul
  %-  (JJ K)
  (S (K (S (K pak))) (S (K S) K))

=/  sub
  %-  (JJ 4)
  %*  S
    (K (S (S I (K (S (S cas (K lef)) (K dec))))))
    (S (K K) rit)
  ==

=/  zer
  %-  (J K)
  (S (S I (K (K no))) (K ya))

=/  eql
  %-  (JJ K)
  (S (S (K S) (S (S (K S) (S (K (S (K cas))) sub)) (K (K (K no))))) (K (K zer)))


::
::  Exports
::

=/  oleg     [I B C]
=/  church   [zero succ]
=/  natjet   [pak inc]
=/  sums     [lef rit cas]
=/  digits   [[[zeo one two] thr for] [[fiv six sev] ate nin]]
=/  bools    [ya no]
=/  pairs    [con car cdr]
=/  control  [seq W1 W2 W3 fix]
=/  math     [dec fec add mul sub zer eql]

[oleg church natjet digits bools pairs control math sums]
