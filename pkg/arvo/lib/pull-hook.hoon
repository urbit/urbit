::  lib/pull-hook: helper for creating a push hook
::  
::   lib/pull-hook is a helper for automatically pulling data from a
::   corresponding push-hook to a store. 
::   
::   ## Interfacing notes:
::
::   The inner door may interact with the library by producing cards.
::   Do not pass any cards on a wire beginning with /helper as these
::   wires are reserved by this library. Any watches/pokes/peeks not
::   listed below will be routed to the inner door.
::
::   ##  Subscription paths
::
::   /tracking: The set of resources we are pulling
::
::   ##  Pokes
::
::   %pull-hook-action: Add/remove a resource from pulling.
::
/-  *pull-hook
/+  default-agent, resource
::
::
|%
+$  card   card:agent:gall
::
::  $config: configuration for the pull hook
::
::    .store-name: name of the store to send subscription updates to.
::    .update-mark: mark that updates will be tagged with
::    .push-hook-name: name of the corresponding push-hook
::
+$  config
  $:  store-name=term
      update=mold
      update-mark=term
      push-hook-name=term
  ==
::  
::  $base-state-0: state for the pull hook
::
::    .tracking: a map of resources we are pulling, and the ships that
::    we are pulling them from.
::    .inner-state: state given to internal door
::
+$  base-state-0
  $:  tracking=(map resource ship)
      inner-state=vase
  ==
::
+$  state-0  [%0 base-state-0]
::
+$  state-1  [%1 base-state-0]
::
+$  versioned-state 
  $%  state-0
      state-1
  ==
::
++  default
  |*  [pull-hook=* =config]
  |_  =bowl:gall
  ::
  ++  on-pull-nack
    |=  [=resource =tang]
    =/  =tank  leaf+"subscribe failed from {<dap.bowl>} for {<resource>}"
    %-  (slog tank tang)
    [~ pull-hook]
  ::
  ++  on-pull-kick
    |=  =resource
    *(unit path)
  --
::
++  pull-hook
  |*  config
  $_  ^|
  |_  bowl:gall
  ::  +on-pull-nack: handle failed pull subscription
  ::
  ::    This arm is called when a pull subscription fails. lib/pull-hook
  ::    will automatically delete the resource from .tracking by the
  ::    time this arm is called.
  ::
  ++  on-pull-nack
    |~  [resource tang]
    *[(list card) _^|(..on-init)]
  ::  +on-pull-kick: produce any additional resubscribe path
  ::
  ::    If non-null, the produced path is appended to the original
  ::    subscription path. This should be used to encode extra
  ::    information onto the path in order to reduce the payload of a
  ::    kick and resubscribe.
  ::
  ::    If null, a resubscribe is not attempted
  ::
  ++  on-pull-kick
    |~  resource
    *(unit path)
  ::
  ::  from agent:gall
  ++  on-init
    *[(list card) _^|(..on-init)]
  ::
  ++  on-save
    *vase
  ::
  ++  on-load
    |~  vase
    *[(list card) _^|(..on-init)]
  ::
  ++  on-poke
    |~  [mark vase]
    *[(list card) _^|(..on-init)]
  ::
  ++  on-watch
    |~  path
    *[(list card) _^|(..on-init)]
  ::
  ++  on-leave
    |~  path
    *[(list card) _^|(..on-init)]
  ::
  ++  on-peek
    |~  path
    *(unit (unit cage))
  ::
  ++  on-agent
    |~  [wire sign:agent:gall]
    *[(list card) _^|(..on-init)]
  ::
  ++  on-arvo
    |~  [wire sign-arvo]
    *[(list card) _^|(..on-init)]
  ::
  ++  on-fail
    |~  [term tang]
    *[(list card) _^|(..on-init)]
  --
++  agent
  |*  =config
  |=  =(pull-hook config)
  =|  state-1
  =*  state  -
  ^-  agent:gall
  =<
    |_  =bowl:gall
    +*  this  .
        og   ~(. pull-hook bowl)
        hc   ~(. +> bowl)
        def  ~(. (default-agent this %|) bowl)
    ++  on-init
      ^-  [(list card:agent:gall) agent:gall]
      =^  cards  pull-hook
        on-init:og
      [cards this]
    ++  on-load
      |=  =old=vase
      =/  old
        !<(versioned-state old-vase)
      =|  cards=(list card:agent:gall)
      |^ 
      ?-  -.old
          %1  
        =^  og-cards   pull-hook
          (on-load:og inner-state.old)
        [(weld cards og-cards) this(state old)]
        ::
          %0
        %_    $
            -.old  %1
          ::
            cards
          (weld cards (missing-subscriptions tracking.old))
        ==
      ==
      ++  missing-subscriptions
        |=  tracking=(map resource ship)
        ^-  (list card:agent:gall)
        %+  murn
          ~(tap by tracking)
        |=  [rid=resource =ship]
        ^-  (unit card:agent:gall)
        =/  =path
          resource+(en-path:resource rid)
        =/  =wire
          (make-wire pull+path)
        ?:  (~(has by wex.bowl) [wire ship push-hook-name.config])
          ~
        `[%pass wire %agent [ship push-hook-name.config] %watch path]
      --
    ::
    ++  on-save
      ^-  vase
      =.  inner-state
        on-save:og
      !>(state)
    ::
    ++  on-poke
      |=  [=mark =vase]
      ^-  [(list card:agent:gall) agent:gall]
      ?>  (team:title our.bowl src.bowl)
      ?.  =(mark %pull-hook-action)
        =^  cards  pull-hook
          (on-poke:og mark vase)
        [cards this]
      =^  cards  state
        (poke-hook-action:hc !<(action vase))
      [cards this]
    ::
    ++  on-watch
      |=  =path
      ^-  [(list card:agent:gall) agent:gall]
      ?>  (team:title our.bowl src.bowl)
      ?.  ?=([%tracking ~] path)
        =^  cards  pull-hook
          (on-watch:og path)
        [cards this]
      :_  this
      ~[give-update]
    ::
    ++  on-agent
      |=  [=wire =sign:agent:gall]
      ^-  [(list card:agent:gall) agent:gall]
      ?.  ?=([%helper %pull-hook @ *] wire)
        =^  cards  pull-hook
          (on-agent:og wire sign)
        [cards this]
      ?.  ?=([%pull %resource *] t.t.wire)
        (on-agent:def wire sign)
      =/  rid=resource
        (de-path:resource t.t.t.t.wire)
      ?+   -.sign  (on-agent:def wire sign)
          %kick
        =/  pax=(unit path)
          (on-pull-kick:og rid)
        ?^  pax
          :_  this
          ~[(watch-resource:hc rid u.pax)]
        =.  tracking
          (~(del by tracking) rid)
        :_  this
        ~[give-update]
      ::
          %watch-ack
        ?~  p.sign
          [~ this]
        =.  tracking
          (~(del by tracking) rid)
        =^  cards  pull-hook
          (on-pull-nack:og rid u.p.sign)
        :_  this
        [give-update cards]
      ::
          %fact
        ?.  =(update-mark.config p.cage.sign)
          =^  cards  pull-hook
            (on-agent:og wire sign)
          [cards this]
        :_  this
        ~[(update-store:hc q.cage.sign)]
      ==
      ++  on-leave
        |=  =path
        ^-  [(list card:agent:gall) agent:gall]
        =^  cards  pull-hook
          (on-leave:og path)
        [cards this]
      ::
      ++  on-arvo
        |=  [=wire =sign-arvo]
        ^-  [(list card:agent:gall) agent:gall]
        =^  cards  pull-hook
          (on-arvo:og wire sign-arvo)
        [cards this]
      ++  on-fail
        |=  [=term =tang]
        ^-  [(list card:agent:gall) agent:gall]
        =^  cards  pull-hook
          (on-fail:og term tang)
        [cards this]
      ++  on-peek   
        |=  =path
        ^-  (unit (unit cage))
        (on-peek:og path)
    --
  |_  =bowl:gall
  +*  og   ~(. pull-hook bowl)
  ::
  ++  poke-hook-action
    |=  =action
    ^-  [(list card:agent:gall) _state]
    |^
    ?-  -.action
      %add  (add +.action)
      %remove  (remove +.action)
    ==
    ++  add
      |=  [=ship =resource]
      ~|  resource
      ?<  |(=(our.bowl ship) =(our.bowl entity.resource))
      ?:  (~(has by tracking) resource)
        [~ state]
      =.  tracking
        (~(put by tracking) resource ship)
      :_  state
      ~[(watch-resource resource /)]
    ::
    ++  remove
      |=  =resource
      :-  ~[(leave-resource resource)]
      state(tracking (~(del by tracking) resource))
    --
  ::
  ++  leave-resource
    |=  rid=resource
    ^-  card
    =/  =ship
      (~(got by tracking) rid)
    =/  =wire
      (make-wire pull+resource+(en-path:resource rid))
    [%pass wire %agent [ship push-hook-name.config] %leave ~]

  ++  watch-resource
    |=  [rid=resource pax=path]
    ^-  card
    =/  =ship
      (~(got by tracking) rid)
    =/  =path
      (welp resource+(en-path:resource rid) pax)
    =/  =wire
      (make-wire pull+path)
    [%pass wire %agent [ship push-hook-name.config] %watch path]
  ::
  ++  make-wire
    |=  =wire
    ^+  wire
    %+  weld
      /helper/pull-hook
    wire
  ::
  ++  give-update
    ^-  card
    [%give %fact ~[/tracking] %pull-hook-update !>(tracking)]
  ::
  ++  update-store
    |=  =vase
    ^-  card
    =/  =wire
      (make-wire /store)
    [%pass wire %agent [our.bowl store-name.config] %poke update-mark.config vase]
  --
--
