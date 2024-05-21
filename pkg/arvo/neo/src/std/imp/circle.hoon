/@  circle
/@  diff=circle-diff
|%
++  state  %circle
++  poke   (sy %circle-diff ~)
++  kids   
  %-  ~(gas by *kids:neo)
  :~  [[|/%p |] %sig %sig]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ?>  ?=(%circle-diff stud)
    =+  !<(=diff vax)
    :_  state-vase
    ?-    -.diff
        %add  
      %+  turn  ~(tap by p.diff)
      |=  [=ship =made:neo]
      [(snoc here.bowl p/ship) %make made]
    ::
        %del
      %+  turn  ~(tap in p.diff)
      |=  =ship
      [(snoc here.bowl p/ship) %tomb ~]
    ==
  ++  init
    |=  new=(unit vase)
    :-  ~
    ?~  new
      !>(*circle)
    u.new
  --
--
