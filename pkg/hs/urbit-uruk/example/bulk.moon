=/  jn
  =/  go
    ..  $
    |=  n
    ?:  (zer n)  J
    ($ (fec n) J)
  (B go fec)

=/  sn
  =/  bod
    ..  $
    |=  x
    ?:  (zer (fec x))  S
    (B S (B ($ (fec x))))
  |=  x
  ((jn (add 2 x)) K (bod x))

=/  bn
  =/  bod
    ..  $
    |=  x
    ?:  (zer (fec x))  B
    (B B ($ (fec x)))
  |=  x
  ((jn (add 2 x)) K (bod x))

=/  cn
  =/  bod
    ..  $
    |=  x
    ?:  (zer (fec x))  C
    (B C (B ($ (fec x))))
  |=  x
  ((jn (add 2 x)) K (bod x))

=/  yet
  |=  n
  ((jn (inc n)) I I)

:*
  [B (bn 1) (bn 5)]
  [S (sn 1) (sn 5)]
  [C (cn 1) (cn 5)]
  [(yet 1) (yet 2) (yet 5)]
==
