::  metadata-store: data store for application metadata and mappings
::  between groups and resources within applications
::
::  group-paths are expected to be an existing group path
::  resources are expected to correspond to existing app paths
::
::  note: when scrying for metadata, to make the arguments safe in paths,
::  encode group-path and app-path using (scot %t (spat group-path))
::
::  +watch paths:
::  /all                                  assocations + updates
::  /updates                              just updates
::  /app-name/%app-name                   specific app's associations + updates
::
::  +peek paths:
::  /associations                                  all associations
::  /group-indices                                 all group indices
::  /app-indices                                   all app indices
::  /resource-indices                              all resource indices
::  /metadata/%group-path/%app-name/%app-path      specific metadatum
::  /app-name/%app-name                            associations for app
::  /group/%group-path                             associations for group
::
/+  *metadata-json, default-agent, verb, dbug
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      =associations
      group-indices=(jug group-path resource)
      app-indices=(jug app-name [group-path app-path])
      resource-indices=(jug resource group-path)
  ==
--
::
=|  state-zero
=*  state  -
%+  verb  |
%-  agent:dbug
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
          [%all ~]      (give %metadata-update !>([%associations associations]))
          [%updates ~]  ~
          [%app-name @ ~]
        =/  =app-name  i.t.path
        =/  app-indices  (metadata-for-app:mc app-name)
        (give %metadata-update !>([%associations app-indices]))
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
        [%y %group-indices ~]     ``noun+!>(group-indices)
        [%y %app-indices ~]       ``noun+!>(app-indices)
        [%y %resource-indices ~]  ``noun+!>(resource-indices)
        [%x %associations ~]      ``noun+!>(associations)
        [%x %app-name @ ~]
      =/  =app-name  i.t.t.path
      ``noun+!>((metadata-for-app:mc app-name))
    ::
        [%x %group *]
      =/  =group-path  t.t.path
      ``noun+!>((metadata-for-group:mc group-path))
    ::
        [%x %metadata @ @ @ ~]
      =/  =group-path  (stab (slav %t i.t.t.path))
      =/  =resource    [`@tas`i.t.t.t.path (stab (slav %t i.t.t.t.t.path))]
      ``noun+!>((~(get by associations) [group-path resource]))
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  poke-metadata-action
  |=  act=metadata-action
  ^-  (quip card _state)
  ?>  (team:title our.bowl src.bowl)
  ?-  -.act
      %add
    (handle-add group-path.act resource.act metadata.act)
  ::
      %remove
    (handle-remove group-path.act resource.act)
  ==
::
++  handle-add
  |=  [=group-path =resource =metadata]
  ^-  (quip card _state)
  :-  %+  send-diff  app-name.resource
      ?.  (~(has by resource-indices) resource)
        [%add group-path resource metadata]
      [%update-metadata group-path resource metadata]
  %=  state
      associations
    (~(put by associations) [group-path resource] metadata)
  ::
      group-indices
    (~(put ju group-indices) group-path resource)
  ::
      app-indices
    (~(put ju app-indices) app-name.resource [group-path app-path.resource])
  ::
      resource-indices
    (~(put ju resource-indices) resource group-path)
  ==
::
++  handle-remove
  |=  [=group-path =resource]
  ^-  (quip card _state)
  :-  (send-diff app-name.resource [%remove group-path resource])
  %=  state
      associations
    (~(del by associations) [group-path resource])
  ::
      group-indices
    (~(del ju group-indices) group-path resource)
  ::
      app-indices
    (~(del ju app-indices) app-name.resource [group-path app-path.resource])
  ::
      resource-indices
    (~(del ju resource-indices) resource group-path)
  ==
::
++  metadata-for-app
  |=  =app-name
  ^-  ^associations
  %-  ~(gas by *^associations)
  %+  turn  ~(tap in (~(gut by app-indices) app-name ~))
  |=  [=group-path =app-path]
  :-  [group-path [app-name app-path]]
  (~(got by associations) [group-path [app-name app-path]])
::
++  metadata-for-group
  |=  =group-path
  ^-  ^associations
  %-  ~(gas by *^associations)
  %+  turn  ~(tap in (~(gut by group-indices) group-path ~))
  |=  =resource
  :-  [group-path resource]
  (~(got by associations) [group-path resource])
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
