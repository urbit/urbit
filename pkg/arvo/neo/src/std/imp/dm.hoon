/@  dm-diff
/@  message
=>
|%
++  card  card:neo
--
^-  firm:neo
|%
++  state  %sig
++  poke  (sy %dm-diff %rely ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%da |]
      [%message %sig]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  
  ::
  :-  %link
  ::
  :+  req=|  [%sig %dm-diff]
  :+  ~  %y
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%da |]
      [%message %sig]
    ==
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?:  =(%rely stud)
      =+  !<([=term =stem:neo] vax)
      ?>  ?=(%y -.q.stem)
      =/  =pail:neo  pail:(snag 0 ~(val by kids.q.stem))
      =+  !<(=message q.pail)
      ::  TODO handle
      ?<  =(our.bowl from.message)
      :_  !>(~)
      :_  ~  
      :*                               
        (welp here.bowl ~[da/now.bowl])
        %make                 
        [%message `q.pail ~]
      ==
    ?>  =(%dm-diff stud)
    =/  poke  !<(dm-diff vax)
    ?>  =(our ship.src):bowl
    ?>  =(%msg -.poke)
    :_  !>(~)
    :~  
      :*
        (welp were.bowl ~[da/now.bowl])
        %make 
        [%message `!>(message.poke) ~]
      ==
    ==
  ++  init
    |=  old=(unit vase)  `!>(~)
  --
--
