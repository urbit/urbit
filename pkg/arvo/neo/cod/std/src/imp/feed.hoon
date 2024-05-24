/@  diff=feed-diff
|%
++  state  pro/%sig
++  poke   (sy %feed-diff ~)
++  kids   
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  [[&/%items |/%da |] pro/%feed-item (sy %feed-item-diff ~)]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  acl/[req=& [pro/%circle ~] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =saga:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  ?=(%feed-diff stud)
    =+  !<(=diff vax)
    :_  q.saga
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
    |=  pal=(unit pail:neo)
    `sig/!>(~)
  --
--
