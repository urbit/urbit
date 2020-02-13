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
      associated=(map [group-path app-name app-path] metadata)
      group-indices=(jug group-path [app-name app-path])
      app-name-indices=(jug app-name [app-path group-path])
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
  ++  on-init            on-init:def
  ++  on-save   !>(state)
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
        [%x %all ~]            ``noun+!>(associated)
        [%x %group-indices ~]  ``noun+!>(group-indices)
        [%x %app-name-indices ~]    ``noun+!>(app-name-indices)
        [%x %app-path-indices ~]    ``noun+!>(app-path-indices)
        [%x %metadata * * *]
      ::  TODO: figure out the best way to do this with two
      ::  variable-length arguments, talk to mark?
      !!
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
::
++  poke-metadata-action
  |=  act=metadata-action
  ^-  (quip card _state)
  ?>  (team:title our.bol src.bol)
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
  ?<  (~(has by app-path-indices) [app-name app-path])
  :-  (send-diff [%add group-path app-name app-path metadata])
  %=  state
      associated
    (~(put by associated) [group-path app-name app-path] metadata)
  ::
      group-indices
    (~(put ju group-indices) group-path [app-name app-path])
  ::
      app-name-indices
    (~(put ju app-name-indices) app-name [app-path group-path])
  ::
      app-path-indices
    (~(put by app-path-indices) [app-name app-path] group-path)
  ==
::
++  handle-remove
  |=  [=group-path =app-name =app-path]
  ^-  (quip card _state)
  :-  (send-diff [%remove group-path app-name app-path])
  %=  state
      associated
    (~(del by associated) [group-path app-name app-path])
  ::
      group-indices
    (~(del ju group-indices) group-path [app-name app-path])
  ::
      app-name-indices
    (~(del ju app-name-indices) app-name [app-path group-path])
  ::
      app-path-indices
    (~(del by app-path-indices) [app-name app-path])
  ==
::
++  send-diff
  |=  act=metadata-action
  ^-  (list card)
  |^
  %-  zing
  :~  (update-subscribers /all act)
      (update-subscribers /updates act)
  ==
  ::
  ++  update-subscribers
    |=  [pax=path act=metadata-action]
    ^-  (list card)
    [%give %fact ~[pax] %metadata-update !>(act)]~
  --
--
