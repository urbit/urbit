/@  diff=circles-diff
|%
++  state  %sig
++  poke   (sy %circles-diff ~)
++  kids   
  %-  ~(gas by *kids:neo)
  :~  [[%|^%tas |] [%sig %circle-diff]]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ?>  ?=(%circles-diff -.stud)
    =+  !<(=diff vax)
    :_  state-vase
    ?-    -.diff
        %add  
      %+  turn  ~(tap by p.diff)
      |=  [=term =made:neo]
      [(snoc here.bowl term) %make made]
    ::
        %del
      %+  turn  ~(tap in p.diff)
      |=  =term
      [(snoc here.bowl term) %tomb ~]
    ==
  ++  init
    |=  vas=(unit vase)
    `*vase
  --
--
