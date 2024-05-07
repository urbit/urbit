/@  diff=feed-item-diff
/@  state=feed-item
=>
|%
+$  circle  (set ship)
++  check-perms
  |=  =bowl:neo
  =+  !<(=circle q.pail.q:(~(got by deps.bowl) %acl))
  (~(has in circle) ship.src.bowl)
--
|%
++  state  %feed-item
++  poke   (sy %feed-item-diff ~)
++  kids   
  %-  ~(gas by *kids:neo)
  :~  [[&/%likes |/%p |] %sig %sig]
      [[&/%comments |/%p |] %feed-comment %sig]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  :-  %acl
      :+  req=&  [%circle %circle-diff]
      :+  ~  %y
      %-  ~(gas by *kids:neo)
      :~  [[|/%p |] %sig %sig]
      ==
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<(^state state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    =/  sta  sta
    ?>  (check-perms bowl)
    ?>  ?=(%feed-item-diff stud)
    =+  !<(=diff vax)
    ~!  diff
    ?-    -.diff
        %add-like
      =.  likes.sta  +(likes.sta)
      :_  !>(sta)
      [(welp here.bowl %likes p/ship.src.bowl ~) %make %sig ~ ~]~
    ::
        %add-comment
      =.  comments.sta  +(comments.sta)
      :_  !>(sta)
      [(welp here.bowl %comments da/now.bowl ~) %make made.diff]~
    ==
  ++  init
    |=  vas=(unit vase)
    `(need vas)
  --
--
