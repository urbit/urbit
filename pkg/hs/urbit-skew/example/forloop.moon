=/  body
  ~/  3  body
  |=  (x y z)
  =/  ig  (trace ['body' x y z])
  ~

=/  call
  ~/  3  call
  |=  (x y z)
  =/  p  (inc z)
  =/  q  (inc p)
  =/  r  (inc q)
  =/  s  (inc r)
  %+  turn
    %+  lcon  z
    %+  lcon  p
    %+  lcon  q
    %+  lcon  r
    %+  lcon  s
    lnil
  (body x y)

=/  main
  ~/  1  main
  |=  n
  (call n (inc n) (inc (inc n)))

(main 3)
