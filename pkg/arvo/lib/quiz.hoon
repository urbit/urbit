|_  [eny=@uv runs=@ud]
++  check  |=  [vax=vase]
  =?  runs  =(0 runs)  100
  =+  size=1
  =+  sax=(slot 6 vax)
  =+  run-i=0
  =+  rng=~(. og eny)
  |-  ^-  ?
  =+  sam=(~(fill quiz [size rng]) sax)
  =+  res=(slam vax sam)
  ?:  =(run-i runs)
      ~&  success-runs+run-i
      %.y
  ?:  =(+:res %.y)
    :: Arbitrarily chosen growth pace.
    =+  new-size=(add +(size) (div (mul size 2) 21))
    $(run-i +(run-i), rng +:(rads:rng 1), size new-size)
  ~&  fail-with-sam+q.sam
  %.n
++  quiz  |_  [size=@ud rng=_og]
  ++  fill  |=  [sax=vase]
    ^+  sax
    =+  new-rng=+:(rads:rng 1)
    ?+  p.sax  ~&(warn-unfill-sam+`type`p.sax sax)
      [%atom p=* q=~]  sax(q (gen-atom p.p.sax))
      [%atom *]        sax
      [%cell p=* q=*]  %=  sax
                          q  :-  q:(fill (slot 2 sax))
                                 q:(fill(rng new-rng) (slot 3 sax))
                      ==
      [%face p=* q=*]  sax(q q:(fill [p=q.p.sax q=q.sax]))
    ==
  ++  gen-atom  |=  [aur=@tas]
    ^-  @
    (rad:rng size)
  --
--