|%
++  state  %hoon
++  poke   ~
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    !!
  ++  init
    |=  vas=(unit vase)
    `(need vas)
  --
--
