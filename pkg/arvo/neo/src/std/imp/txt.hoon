/@  txt
|%
++  state  %txt
++  poke   (sy %txt ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%txt stud)
    =/  new  (txt !<(txt vax))
    `!>(new)
  ++  init
    |=  vas=(unit vase)
    `(need vas)
  --
--
