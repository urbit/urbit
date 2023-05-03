|_  [eny=@uv]
++  check  |=  [vax=vase size=@ud]
  =+  ran=~(. og eny)
  =+  sax=(slot 6 vax)
  =/  res  ?+  p.sax  ~&('fail' !!)
             [%atom p=term q=~]      =+(sam=(gen-atom p.p.sax ran size) [-:sam (slam vax !>(-:sam))])
             [%atom p=term q=[~ @]]  [sax (slam vax sax)]
           ==
  ^-  ?
  ?:  =(+>:res 0)
      ~&  "success"
      %.y
  ~&  "failure"
  ~&  "sample:"
  ~&  -:res
  %.n
++  gen-atom  |=  [aur=@tas ran=_og size=@ud]
  ^+  [0 ran]
  (rads:ran +(size))
--
