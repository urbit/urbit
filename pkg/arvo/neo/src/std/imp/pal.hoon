/@  pal-type=pal
/@  pal-diff
=> 
|%
++  card  card:neo
--
^-  firm:neo
|%
++  state  %pal
++  poke  (sy %pal-diff ~)
++  kids  *kids:neo
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<(pal-type state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    =/  sta  sta
    ?>  =(%pal-diff stud)
    =+  !<(poke=pal-diff vax)
    :-  ~
    !>  ^-  pal-type
    ~!  sta
    ?-  -.poke
      %add-tag  sta(tags (~(put in tags.sta) term.poke))
      %del-tag  sta(tags (~(del in tags.sta) term.poke))
    ==
  ++  init
    |=  old=(unit vase)
    =+  !<(p=pal-type (need old))
    `!>(p)
  --
--
