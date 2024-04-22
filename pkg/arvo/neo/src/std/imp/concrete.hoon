=>
  |%
  ++  get-dep
    |=  =bowl:neo
    ^-  @ud
    !<(@ud q.pail.q:(~(got by deps.bowl) %a))
  --
|%
++  state  %atom
++  poke   (sy %rely ~)
++  kids   *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  :~  [%a & [%atom %sig] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    =+  !<(=rely:neo vax)
    `!>((get-dep bowl))
  ++  init
    |=  vas=(unit vase)
    `!>((get-dep bowl))
  --
--
