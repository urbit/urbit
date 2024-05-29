/@  pal-type=pal
/@  pal-diff
=> 
|%
++  card  card:neo
--
^-  kook:neo
|%
++  state  pro/%pal
++  poke  (sy %pal-diff ~)
++  kids  *kids:neo
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  +*  sta  !<(pal-type state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  sta  sta
    ?>  =(%pal-diff stud)
    =+  !<(poke=pal-diff vax)
    :-  ~
    :-  %pal
    !>  ^-  pal-type
    ?-  -.poke
      %add-tag  sta(tags (~(put in tags.sta) term.poke))
      %del-tag  sta(tags (~(del in tags.sta) term.poke))
    ==
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need old)
  --
--
