|_  [eny=@uv]
++  check  |=  [vax=vase]
  =+  sax=(slot 6 vax)
  =+  sam=(~(fill quiz [1.000 ~(. og eny)]) sax)
  =+  res=(slam vax sam)
  ^-  ?
  ?:  =(+:res %.y)
      ~&  %success
      %.y
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
                        q  :-  q:(fill [p=p.p.sax q=-.q.sax])
                               q:(fill(rng new-rng) [p=q.p.sax q=+.q.sax])
                     ==
    [%face p=* q=*]  sax(q q:(fill [p=q.p.sax q=q.sax]))
  ==
  ++  gen-atom  |=  [aur=@tas]
    ^-  @
    (rad:rng size)
  --
--
