|_  [eny=@uv]
++  size  1.000
++  check  |=  [vax=vase]
  =+  rng=~(. og eny)
  =+  sax=(slot 6 vax)
  =+  sam=(fill sax rng)
  =+  res=(slam vax sam)
  ^-  ?
  ?:  =(+:res %.y)
      ~&  "success"
      %.y
  ~&  "failure"
  ~&  "sample:"
  ~&  -:res
  %.n
++  fill  |=  [sax=vase rng=_og]
  ^+  sax
  ?+  p.sax  ~&('fail' !!)
    [%atom p=* q=~]  sax(q -:(gen-atom p.p.sax rng))
    [%atom *]        sax
    [%cell p=* q=*]  sax(q [q:(fill [p=p.p.sax q=-.q.sax] rng) q:(fill [p=q.p.sax q=+.q.sax] rng)])
    [%face p=* q=*]  sax(q q:(fill [p=q.p.sax q=q.sax] rng))
  ==
++  gen-atom  |=  [aur=@tas rng=_og]
  ^+  [0 rng]
  (rads:rng size)
--
