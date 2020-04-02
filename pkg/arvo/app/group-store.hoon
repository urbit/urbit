::  group-store: data store for groups of ships
::
/-  *group-store
/+  default-agent, verb, dbug
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      =groups
  ==
::
+$  diff
  $%  [%group-update group-update]
      [%group-initial groups]
  ==
--
::
=|  state-zero
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this        .
      group-core  +>
      gc          ~(. group-core bowl)
      def         ~(. (default-agent this %|) bowl)
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
      ?:  ?=(%group-action mark)
        (poke-group-action:gc !<(group-action vase))
      (on-poke:def mark vase)
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    |^
    =/  cards=(list card)
      ?+    path  (on-watch:def path)
          [%all ~]   (give %group-initial !>(groups))
          [%updates ~]  ~
          [%keys ~]  (give %group-update !>([%keys ~(key by groups)]))
          [%group *]
        (give %group-update !>([%path (~(got by groups) t.path) t.path]))
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
      [%x *]  ``noun+!>((~(get by groups) t.path))
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
::
++  poke-group-action
  |=  action=group-action
  ^-  (quip card _state)
  ?>  (team:title our.bol src.bol)
  ?-  -.action
      %add       (handle-add action)
      %remove    (handle-remove action)
      %bundle    (handle-bundle action)
      %unbundle  (handle-unbundle action)
  ==
::
++  handle-add
  |=  act=group-action
  ^-  (quip card _state)
  ?>  ?=(%add -.act)
  ?~  pax.act
    [~ state]
  ?.  (~(has by groups) pax.act)
    [~ state]
  =/  members  (~(got by groups) pax.act)
  =.  members  (~(uni in members) members.act)
  ?:  =(members (~(got by groups) pax.act))
    [~ state]
  :-  (send-diff pax.act act)
  state(groups (~(put by groups) pax.act members))
::
++  handle-remove
  |=  act=group-action
  ^-  (quip card _state)
  ?>  ?=(%remove -.act)
  ?~  pax.act
    [~ state]
  ?.  (~(has by groups) pax.act)
    [~ state]
  =/  members  (~(got by groups) pax.act)
  =.  members  (~(dif in members) members.act)
  ?:  =(members (~(got by groups) pax.act))
    [~ state]
  :-  (send-diff pax.act act)
  state(groups (~(put by groups) pax.act members))
::
++  handle-bundle
  |=  act=group-action
  ^-  (quip card _state)
  ?>  ?=(%bundle -.act)
  ?~  pax.act
    [~ state]
  ?:  (~(has by groups) pax.act)
    [~ state]
  :-  (send-diff pax.act act)
  state(groups (~(put by groups) pax.act *group))
::
++  handle-unbundle
  |=  act=group-action
  ^-  (quip card _state)
  ?>  ?=(%unbundle -.act)
  ?~  pax.act
    [~ state]
  ?.  (~(has by groups) pax.act)
    [~ state]
  :-  (send-diff pax.act act)
  state(groups (~(del by groups) pax.act))
::
++  update-subscribers
  |=  [pax=path act=group-action]
  ^-  (list card)
  [%give %fact ~[pax] %group-update !>(act)]~
::
++  send-diff
  |=  [pax=path act=group-action]
  ^-  (list card)
  %-  zing
  :~  (update-subscribers /all act)
      (update-subscribers /updates act)
      (update-subscribers [%group pax] act)
      ?.  |(=(%bundle -.act) =(%unbundle -.act))
        ~
      (update-subscribers /keys act)
  ==
::
--
