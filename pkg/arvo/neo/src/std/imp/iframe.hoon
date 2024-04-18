/@  iframe
|%
++  state  %iframe
++  poke   (sy %iframe ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%iframe stud)
    =/  new  (iframe !<(iframe vax))
    `!>(new)
  ++  init
    |=  vas=(unit vase)
    `(need vas)
  --
--
