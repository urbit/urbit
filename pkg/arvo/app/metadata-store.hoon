::  metadata-store: data store for application metadata and mappings 
::  between groups and application data (chatrooms, publish notebooks, etc)
::
/-  *metadata-store
/+  default-agent
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      =associated
      group-indices=(jug group-path [app-name app-path])
      app-name-indices=(jug app-name [group-path app-path])
      app-path-indices=(map [app-name app-path] group-path)
  ==
--
::
=|  state-zero
=*  state  -
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this           .
      metadata-core  +>
      mc             ~(. metadata-core bowl)
      def            ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  on-init:def
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?:  ?=(%metadata-action mark)
        (poke-metadata-action:mc !<(metadata-action vase))
      (on-poke:def mark vase)
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    |^
    =/  cards=(list card)
      ?+  path  (on-watch:def path)
          [%all ~]      (give %metadata-initial !>(associated))
          [%updates ~]  ~
          [%app-name @ ~]
        =/  =app-name  i.t.path
        =/  app-indices  (metadata-for-app:mc app-name)
        (give %metadata-update !>([%app-indices app-indices]))
      ==
    [cards this]
    ::
    ++  give
      |=  =cage
      ^-  (list card)
      [%give %fact ~ cage]~
    --
  ::
  ++  on-leave  on-leave:def
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%x %all ~]               ``noun+!>(associated)
        [%x %group-indices ~]     ``noun+!>(group-indices)
        [%x %app-name-indices ~]  ``noun+!>(app-name-indices)
        [%x %app-path-indices ~]  ``noun+!>(app-path-indices)
        [%x %metadata @ @ @ ~]
      =/  =group-path  (stab i.t.t.path)
      =/  =app-name    `@tas`i.t.t.t.path
      =/  =app-path    (stab i.t.t.t.t.path)
      ``noun+!>((~(got by associated) [group-path app-name app-path]))
    ::
        [%x %app-name @ ~]
      =/  =app-name  i.t.t.path
      ``noun+!>((metadata-for-app:mc app-name))
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
::
++  poke-metadata-action
  |=  act=metadata-action
  ^-  (quip card _state)
  ?>  (team:title our.bowl src.bowl)
  ?-  -.act
      %add
    (handle-add group-path.act app-name.act app-path.act metadata.act)
  ::
      %remove
    (handle-remove group-path.act app-name.act app-path.act)
  ==
::
++  handle-add
  |=  [=group-path =app-name =app-path =metadata]
  ^-  (quip card _state)
  :-  %+  send-diff  app-name
      ?.  (~(has by app-path-indices) [app-name app-path])
        [%add group-path app-name app-path metadata]
      [%update-metadata group-path app-name app-path metadata]
  %=  state
      associated
    (~(put by associated) [group-path app-name app-path] metadata)
  ::
      group-indices
    (~(put ju group-indices) group-path [app-name app-path])
  ::
      app-name-indices
    (~(put ju app-name-indices) app-name [group-path app-path])
  ::
      app-path-indices
    (~(put by app-path-indices) [app-name app-path] group-path)
  ==
::
++  handle-remove
  |=  [=group-path =app-name =app-path]
  ^-  (quip card _state)
  :-  (send-diff app-name [%remove group-path app-name app-path])
  %=  state
      associated
    (~(del by associated) [group-path app-name app-path])
  ::
      group-indices
    (~(del ju group-indices) group-path [app-name app-path])
  ::
      app-name-indices
    (~(del ju app-name-indices) app-name [group-path app-path])
  ::
      app-path-indices
    (~(del by app-path-indices) [app-name app-path])
  ==
::
++  metadata-for-app
  |=  =app-name
  %-  ~(gas by *(map [group-path ^app-name app-path] metadata))
  %+  turn  ~(tap in (~(got by app-name-indices) app-name))
  |=  [=group-path =app-path]
  :-  [group-path app-name app-path]
  (~(got by associated) [group-path app-name app-path])
::
++  send-diff
  |=  [=app-name upd=metadata-update]
  ^-  (list card)
  |^
  %-  zing
  :~  (update-subscribers /all upd)
      (update-subscribers /updates upd)
      (update-subscribers [%app-name app-name ~] upd)
  ==
  ::
  ++  update-subscribers
    |=  [pax=path upd=metadata-update]
    ^-  (list card)
    [%give %fact ~[pax] %metadata-update !>(upd)]~
  --
--
