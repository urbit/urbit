::  link [landscape]:
::
/-  *link, gra=graph-store, *resource
/+  store=link-store, graph-store, default-agent, verb, dbug
::
|%
+$  spore-any  $%(spore-1 state-0)
+$  state-any  $%(state-1 state-0)
+$  spore-1  [%1 cards=*]
+$  state-1  [%1 cards=(list card)]
+$  state-0
  $:  %0
      by-group=(map path links)
      by-site=(map site (list [path submission]))
      discussions=(per-path-url discussion)
  ==
::
+$  links
  $:  ::NOTE  all lists by recency
      =submissions
      ours=pages
      seen=(set url)
  ==
::
+$  discussion
  $:  =comments
      ours=notes
  ==
::
+$  card  card:agent:gall
--
::
=|  state-1
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    do    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  ::
  =/  s  !<(spore-any old)
  ?:  ?=(%1 -.s)
    [~ this(state s(cards ~))]
  ::  defer card emission to later event
  ::
  =;  [cards=(list card) that=_this]
    :_  that(state [%1 cards])
    [%pass /load %arvo %b %wait now.bowl]~
  ::
  :_  this(state *state-1)
  =/  orm  orm:graph-store
  |^  ^-  (list card)
  %-  zing
  %+  turn  ~(tap by by-group.s)
  |=  [=path =links]
  ^-  (list card)
  ?.  ?=([@ ~] path)
    (on-bad-path path links)
  =/  =resource  [our.bowl i.path]
  :_  [(archive-graph resource)]~
  %+  add-graph  resource
  ^-  graph:gra
  %+  gas:orm  ~
  =/  comments  (~(gut by discussions.s) path *(map url discussion))
  %+  turn  submissions.links
  |=  sub=submission
  ^-  [atom node:gra]
  :-  time.sub
  =/  contents  ~[text+title.sub url+url.sub]
  =/  parent-hash  `@ux`(sham ~ ship.sub time.sub contents)
  :-  ^-  post:gra
      :*  author=ship.sub
          index=~[time.sub]
          time-sent=time.sub
          contents
          hash=`parent-hash
          signatures=~
      ==
  ^-  internal-graph:gra
  =/  dis  (~(get by comments) url.sub)
  ?~  dis
    [%empty ~]
  :-  %graph
  ^-  graph:gra
  %+  gas:orm  ~
  %+  turn  comments.u.dis
  |=  [=ship =time udon=@t]
  ^-  [atom node:gra]
  :-  time
  :_  `internal-graph:gra`[%empty ~]
  =/  contents  ~[text+udon]
  :*  author=ship
      index=~[time.sub time]
      time-sent=time
      contents
      hash=``@ux`(sham `parent-hash ship time contents)
      signatures=~
  ==
  ::
  ++  on-bad-path
    |=  [=path =links]
    ^-  (list card)
    ~&  discarding-malformed-links+[path links]
    ~
  ::
  ++  add-graph
    |=  [=resource =graph:gra]
    ^-  card
    %-  poke-graph-store
    [%0 now.bowl %add-graph resource graph `%graph-validator-link %.y]
  ::
  ++  archive-graph
    |=  =resource
    ^-  card
    %-  poke-graph-store
    [%0 now.bowl %archive-graph resource]
  ::
  ++  poke-graph-store
    |=  =update:gra
    ^-  card
    :*  %pass  /migrate-link  %agent  [our.bowl %graph-store]
        %poke  %graph-update  !>(update)
    ==
  --
::
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+    sign-arvo  (on-arvo:def wire sign-arvo)
      [%b %wake *]
    [cards.state this]
  ==
++  on-fail   on-fail:def
--
