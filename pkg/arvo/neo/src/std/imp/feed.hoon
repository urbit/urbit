/@  diff=feed-diff
|%
++  state  %sig
++  poke   (sy %feed-diff ~)
++  kids   
  %-  ~(gas by *kids:neo)
  :~  [[&/%items |/%da |] %feed-item %feed-item-diff]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  acl/[req=& [%circle %sig] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ?>  ?=(%feed-diff stud)
    =+  !<(=diff vax)
    :_  state-vase
    ?-    -.diff
        %add  
      [(snoc here.bowl da/now.bowl) %make p.diff]~
    ::
        %del
      %+  turn  ~(tap in p.diff)
      |=  =@da
      [(snoc here.bowl da/da) %tomb ~]
    ==
  ++  init
    |=  vas=(unit vase)
    `*vase
  --
--
