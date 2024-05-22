/@  diff=feed-item-diff
/@  state=feed-item
/@  circle
=>
|%
++  check-perms
  |=  =bowl:neo
  =+  !<(=circle q.pail:(need fil.q:(~(got by deps.bowl) %acl)))
  (~(has in circle) ship.src.bowl)
--
|%
++  state  pro/%feed-item
++  poke   (sy %feed-item-diff ~)
++  kids   
  %-  ~(gas by *kids:neo)
  :~  [[&/%likes |/%p |] pro/%sig ~]
      [[&/%comments |/%p |] pro/%feed-comment ~]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  :-  %acl
      :+  req=&  [%circle %sig]
      :+  ~  %y
      %-  ~(gas by *kids:neo)
      :~  [[|/%p |] pro/%sig ~]
      ==
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  +*  sta  !<(^state state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    =/  sta  sta
    ?>  (check-perms bowl)
    ?>  ?=(%feed-item-diff stud)
    =+  !<(=diff vax)
    ?-    -.diff
        %add-like
      =.  likes.sta  +(likes.sta)
      :_  feed-item/!>(sta)
      [(welp here.bowl %likes p/ship.src.bowl ~) %make %sig ~ ~]~
    ::
        %add-comment
      =.  comments.sta  +(comments.sta)
      :_  feed-item/!>(sta)
      [(welp here.bowl %comments da/now.bowl ~) %make made.diff]~
    ==
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need pal)
  --
--
