|%
++  state  %atom
++  poke   (sy %atom ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    =+  !<(val=@ud vax)
    `!>(val)
  ++  init
    |=  vas=(unit vase)
    `(need vas)
  --
--
