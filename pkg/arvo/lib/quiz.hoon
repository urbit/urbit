|_  [eny=@uv]
++  check  |=  [vax=vase size=@ud]
  =+  ran=~(. og eny)
  =+  sax=(slot 6 vax)
  =+  sam=(fill sax size ran)
  =+  res=(slam vax sam)
  ^-  ?
  ?:  =(+:res %.y)
      ~&  "success"
      %.y
  ~&  "failure"
  ~&  "sample:"
  ~&  -:res
  %.n
++  fill  |=  [sax=vase size=@ud ran=_og]
  ^+  sax
  ?+  p.sax  ~&('fail' !!)
    [%atom p=* q=~]  sax(q -:(gen-atom p.p.sax ran size))
    [%atom *]        sax
    [%face p=* q=*]  sax(q q:(fill [p=q.p.sax q=q.sax] size ran))
  ==
++  gen-atom  |=  [aur=@tas ran=_og size=@ud]
  ^+  [0 ran]
  (rads:ran +(size))
--
