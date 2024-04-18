/@  sky
|%
++  state  %sky
++  poke   (sy %sky ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%sky stud)
    =/  new  (sky !<(sky vax))
    `!>(new)
  ++  init
    |=  vas=(unit vase)
    `(need vas)
  --
--
