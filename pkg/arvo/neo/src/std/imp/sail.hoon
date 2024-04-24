/@  sail
|%
++  state  %sail
++  poke   (sy %sail ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(stud %sail)
    `vax
  ++  init
    |=  vas=(unit vase)
    `(need vas)
  --
--
