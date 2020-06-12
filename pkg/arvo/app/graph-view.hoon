/+  view=graph-view, store=graph-store, default-agent, verb, dbug
::
|%
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      connections=(map atom:store time)
  ==
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this       .
    def        ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  [%pass /updates %agent [our.bowl %graph-store] %watch /updates]~
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark                (on-poke:def mark vase)
        %graph-view-action  (view-action !<(action:view vase))
    ==
  [cards this]
  ::
  ++  view-action
    |=  =action:view
    ^-  (quip card _state)
    ?-  -.action
        %fetch  (fetch +.action)
    ==
  ::
  ++  scry-for
    |*  [=mold =path]
    .^  mold
      %gx
      (scot %p our.bowl)
      %graph-store
      (scot %da now.bowl)
      (snoc `^path`path %noun)
    ==
  ::
  ++  fetch
    |=  [conn=atom:store type=fetch-type:view]
    ^-  (quip card _state)
    =/  keys    (scry-for resources:store /keys)
    :_  state
    :-  (give conn [%graph-update !>([%0 [%keys keys]])])
    %+  turn  ~(tap in keys)
    |=  [=ship =term]
    (give conn [%graph-update !>((add-graph ship term))])
  ::
  ++  add-graph
    |=  [=ship =term]
    ^-  update:store
    :-  %0
    :+  %add-graph
      [ship term]
    (scry-for graph:store /graph/(scot %p ship)/[term])
  ::
  ++  give
      |=  [conn=atom:store =cage]
      ^-  card
      [%give %fact [/updates/(scot %ud conn)]~ cage]
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?+  path  (on-watch:def path)
      [%updates @ ~]
    :-  [%give %fact ~ %json !>([(frond:enjs:format %graph-view s+'bound')])]~
    this(connections (~(put by connections) (slav %ud i.t.path) now.bowl))
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    :_  this
    [%pass /updates %agent [our.bowl %graph-store] %watch /updates]~
  ::
      %fact
    ?+  p.cage.sign  (on-agent:def wire sign)
        %graph-update
      :_  this
      %+  give
        %+  turn  ~(tap by connections)
        |=  [=atom:store *]
        ^-  path
        /updates/(scot %ud atom)
      cage.sign
    ==
  ==
  ::
  ++  give
    |=  [paths=(list path) =cage]
    ^-  (list card)
    [%give %fact paths cage]~
  --
::
++  on-save   !>(state)
++  on-load   on-load:def
++  on-arvo   on-arvo:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
