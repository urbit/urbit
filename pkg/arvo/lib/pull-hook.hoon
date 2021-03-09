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
/+  default-agent, resource, versioning
|%
::  JSON conversions
++  dejs
  =,  dejs:format
  |%
  ++  action
    %-  of
    :~  add+add
    ==
  ++  add
    %-  ot
    :~  ship+(su ;~(pfix sig fed:ag))
        resource+dejs:resource
    ==
  --
--
::
::
|%
+$  card   card:agent:gall
::
::  $config: configuration for the pull hook
::
::    .store-name: name of the store to send subscription updates to.
::    .update-mark: mark that updates will be tagged with, without
::    version number
::    .push-hook-name: name of the corresponding push-hook
::    .no-validate: If true, don't validate that resource/wire/src match
::    up
::
+$  config
  $:  store-name=term
      update=mold
      update-mark=term
      push-hook-name=term
      version=@ud
      no-validate=_|
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
+$  base-state-1
  $:  base-state-0
      failed-kicks=(map resource ship)
  ==
::
+$  state-0  [%0 base-state-0]
::
+$  state-1  [%1 base-state-0]
::
+$  state-2  [%2 base-state-1]
::
+$  versioned-state 
  $%  state-0
      state-1
      state-2
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
  ::  +resource-for-update: get resources from vase
  ::
  ::    This should be identical to the +resource-for-update arm in the
  ::    corresponding push-hook
  ::
  ++  resource-for-update
    |~  vase
    *(list resource)
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
  =|  state-2
  =*  state  -
  ^-  agent:gall
  =<
    |_  =bowl:gall
    +*  this  .
        og   ~(. pull-hook bowl)
        hc   ~(. +> bowl)
        def  ~(. (default-agent this %|) bowl)
        ver  ~(. versioning [bowl update-mark.config version.config])
    ::
    ++  on-init
      ^-  [(list card:agent:gall) agent:gall]
      =^  cards  pull-hook
        on-init:og
      [cards this]
    ::
    ++  on-load
      |=  =old=vase
      =/  old
        !<(versioned-state old-vase)
      =|  cards=(list card:agent:gall)
      |^ 
      ?-  -.old
          %2  
        =^  og-cards   pull-hook
          (on-load:og inner-state.old)
        =.  state  old
        =^  retry-cards  state
          retry-failed-kicks
        :_  this
        :(weld cards og-cards retry-cards) 
        ::
          %1  $(old [%2 +.old ~])
        ::
          %0
        %_    $
            -.old  %1
          ::
            cards
          (weld cards (missing-subscriptions tracking.old))
        ==
      ==
      ::
      ++  retry-failed-kicks
        =|  acc-cards=(list card)
        =/  failures=(list [rid=resource =ship])
          ~(tap by failed-kicks)
        =.  tracking
          (~(uni by tracking) failed-kicks)
        =.  failed-kicks  ~
        |-  ^-  (quip card _state)
        ?~  failures
          [acc-cards state]
        =,  failures
        =^  crds  state
          (handle-kick:hc i)
        $(failures t, acc-cards (weld acc-cards crds))
      ::
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
      ?+   mark
        =^  cards  pull-hook
          (on-poke:og mark vase)
        [cards this]
      ::
          %sane
        ?>  (team:title [our src]:bowl)
        =^  cards  state
          poke-sane:hc
        [cards this]
      ::
          %pull-hook-action
        ?>  (team:title [our src]:bowl)
        =^  cards  state
          (poke-hook-action:hc !<(action vase))
        [cards this]
      ==
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
        =^  cards  state
          (handle-kick:hc rid src.bowl)
        [cards this]
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
        ?.  (is-root:ver p.cage.sign)
          =^  cards  pull-hook
            (on-agent:og wire sign)
          [cards this]
        =/  =vase
          (convert:ver cage.sign)
        :_  this
        ~[(update-store:hc rid q.cage.sign)]
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
        ?:  =(/x/dbug/state path)
          ``noun+(slop !>(state(inner-state *vase)) on-save:og)
        ?.  =(/x/tracking path)
          (on-peek:og path)
        ``noun+!>(~(key by tracking))
    --
  |_  =bowl:gall
  +*  og   ~(. pull-hook bowl)
  ++  poke-sane
    ^-  (quip card:agent:gall _state)
    =/  cards
      restart-subscriptions
    ~?  >  ?=(^ cards)
      "Fixed subscriptions in {<dap.bowl>}"
    :_  state
    restart-subscriptions
  ::
  ++  check-subscription
    |=  [rid=resource =ship]
    ^-  ?
    %+  lien
      ~(tap in ~(key by wex.bowl))
    |=  [=wire her=^ship app=term]
    ^-  ?
    ?&  =(app push-hook-name.config)
        =(ship her)
        =((scag 4 wire) /helper/pull-hook/pull/resource)
        =(`rid (de-path-soft:resource (slag 4 wire)))
    ==
  ::
  ++  restart-subscriptions
    ^-  (list card:agent:gall)
    %-  zing
    %+  turn
      ~(tap by tracking)
    |=  [rid=resource =ship] 
    ^-  (list card:agent:gall)
    ?:  (check-subscription rid ship)  ~
    ~&  >>  "restarting: {<rid>}"
    =/  pax=(unit path)
      (on-pull-kick:og rid)
    ?~  pax  ~
    (watch-resource rid u.pax)
  ::
  ++  mule-scry
    |=  [ref=* raw=*]
    =/  pax=(unit path)
      ((soft path) raw)
    ?~  pax  ~
    ?.  ?=([@ @ @ @ *] u.pax)  ~
    =/  ship
      (slaw %p i.t.u.pax)
    =/  ved
      (slay i.t.t.t.u.pax)
    =/  dat
      ?~  ved  now.bowl
      =/  cas=(unit case)
        ((soft case) p.u.ved)
      ?~  cas  now.bowl
      ?:  ?=(%da -.u.cas)
        p.u.cas
      now.bowl
    ::  catch bad gall scries early
    ?:  ?&  =((end 3 i.u.pax) %g)
            ?|  !=(`our.bowl ship)
                !=(dat now.bowl)
            ==
        ==
      ~
    ``.^(* u.pax)
  ::
  ++  handle-kick
    |=  [rid=resource =ship]
    ^-  (quip card _state)
    =/  res=toon
      (mock [|.((on-pull-kick:og rid)) %9 2 %0 1] mule-scry)
    =/  pax=(unit path)
      !<  (unit path) 
      :-  -:!>(*(unit path)) 
      ?:(?=(%0 -.res) p.res ~)
    =?  failed-kicks  !?=(%0 -.res)
      =/  =tang
        :+  leaf+"failed kick handler, please report" 
          leaf+"{<rid>} in {(trip dap.bowl)}"
        ?:  ?=(%2 -.res)
          p.res
        ?>  ?=(%1 -.res)
        =/  maybe-path=(unit path)  ((soft path) p.res)
        ?~  maybe-path  ~
        [(smyt u.maybe-path) ~]
      %-  (slog tang)
      (~(put by failed-kicks) rid ship)
    ?^  pax
      :_  state
      (watch-resource rid u.pax)
    =.  tracking
      (~(del by tracking) rid)
    :_  state
    ~[give-update]
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
      (watch-resource resource /)
    ::
    ++  remove
      |=  =resource
      :-  (leave-resource resource)
      state(tracking (~(del by tracking) resource))
    --
  ::
  ++  leave-resource
    |=  rid=resource
    ^-  (list card)
    =/  ship=(unit ship)
      (~(get by tracking) rid)
    ?~  ship  ~
    =/  =wire
      (make-wire pull+resource+(en-path:resource rid))
    [%pass wire %agent [u.ship push-hook-name.config] %leave ~]~
  ::
  ++  watch-resource
    |=  [rid=resource pax=path]
    ^-  (list card)
    =/  ship=(unit ship)
      (~(get by tracking) rid)
    ?~  ship  ~
    =/  =path
      (welp (snoc resource+(en-path:resource rid) (scot %ud version.config)) pax)
    =/  =wire
      (make-wire pull+resource+(en-path:resource rid))
    [%pass wire %agent [u.ship push-hook-name.config] %watch path]~
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
  ++  check-src
    |=  resources=(set resource)
    ^-  ?
    %+  roll  ~(tap in resources)
    |=  [rid=resource out=_|]
    ?:  out  %.y
    ?~  ship=(~(get by tracking) rid)
      %.n
    =(src.bowl u.ship)
  ::
  ++  update-store
    |=  [wire-rid=resource =vase]
    ^-  card
    =/  =wire
      (make-wire /store)
    =+  resources=(~(gas in *(set resource)) (resource-for-update:og vase))
    ?>  ?|  no-validate.config
        ?&  (check-src resources)
            (~(has in resources) wire-rid)
        ==  ==
    [%pass wire %agent [our.bowl store-name.config] %poke update-mark.config vase]
  --
--
