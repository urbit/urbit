|_  [eny=@uv]
++  check  |=  [vax=vase size=@ud]
  =+  ran=~(. og eny)
  =+  sax=(slot 6 vax)
  ?+  -<.sax  ~&('fail' !!)
    %atom
      ^-  ?
      =^  sam  ran  (rads:ran +(size))
      =+  res=(slam vax !>(sam))
      ?:  =(+:res 0)
         ~&  "success"
         %.y
      ~&  "failure"
      ~&  "sample:"
      ~&  sam
      %.n
  ==
--
