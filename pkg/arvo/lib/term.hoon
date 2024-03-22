/-  neo
=> 
|%
++  card  card:neo
--
^-  firm:neo
|%
++  state  %term
++  poke   (sy %term ~)
++  kids   ~
++  deps   ~
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<(@tas state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(stud %term)
    =+  !<(=term vax)
    `!>(term)
  ++  init
    |=  old=(unit vase)
    ^-  (quip card:neo vase)
    ?~  old
      `!>(*term)
    =+  !<(=term u.old)
    `!>(term)
  --
--
