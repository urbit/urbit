=>
|%
+$  card  card:neo
--
^-  firm:neo
|%
++  state  %hoon
++  poke   (sy %clay-res ~)
++  deps   *deps:neo
++  kids   *kids:neo
++  form
  |_  [=bowl:neo =ever:neo state=vase *]
  +*  sta  !<(@t state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%clay-res stud)
    =+  !<(=res:clay:neo vax)
    :_  state
    ~&  res/res
    ~
  ++  init
    |=  old=(unit vase)
    =+  !<(=desk (need old))
    :_  !>(desk)
    [#/[p/our.bowl]/$/clay %poke %clay-req !>(`req:clay:neo`[%peer ~ desk /neo])]^~
  --
--
  
