::  metadata-hook: allow syncing foreign metadata
::
/-  *metadata-store
/+  default-agent
|%
+$  card  card:agent:gall
::
++  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      synced=(map [group-path resource] ship)
  ==
--
=|  state-zero
=*  state  -
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this        .
      hook-core  +>
      hc          ~(. hook-core bowl)
      def         ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  on-init:def
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?.  ?=(%metadata-hook-action mark)
      (on-poke:def mark vase)
    =^  cards  state
      (poke-hook-action:gc !<(metadata-hook-action vase))
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    (on-watch:def path)
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %watch-ack
      =^  cards  state
        (watch-ack:cc wire p.sign) 
      [cards this]
    ::
        %kick
      ?>  ?=([@ @ *] wire)
      =/  =ship  (slav %p i.wire)
      ?.  (~(has by synced.state) wire)
        [~ this]
      =/  group-path  [%group wire]
      =/  group-wire  [i.wire group-path]
      :_  this
      [%pass group-wire %agent [ship %group-hook] %watch group-path]~
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %metadata-update
        =^  cards  state
          (fact-metadata-update:cc wire !<(metadata-update q.cage.sign))
        [cards this]
      ==
    ==
  --
::
|_  bol=bowl:gall
++  watch-ack
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip card _state)
  ?~  saw
    [~ state]
  ?>  ?=(^ wir)
  [~ state(synced (~(del by synced) t.wir))]
--
