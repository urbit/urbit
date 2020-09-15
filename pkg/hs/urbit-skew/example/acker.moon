=/  acker
  ~/  2  acker
  ..  $
  |=  (x y)
  ?:  (zer x)
    (inc y)
  ?:  (zer y)
    ($ (fec x) 1)
  ($ (fec x) ($ x (fec y)))
(acker 3 9)
