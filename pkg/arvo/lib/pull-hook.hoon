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
/+  default-agent, resource, versioning, agentio, pull-hook-virt
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
      min-version=@ud
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
+$  track
  [=ship =status]
::
+$  status
  $%  [%active ~]
      [%failed-kick ~]
      [%pub-ver @ud]
      [%sub-ver @ud]
  ==
::
+$  base-state-2
  $:  tracking=(map resource track)
      inner-state=vase
  ==
      
::
+$  state-0  [%0 base-state-0]
::
+$  state-1  [%1 base-state-0]
::
+$  state-2  [%2 base-state-1]
::
+$  state-3  [%3 base-state-2]
::
+$  versioned-state 
  $%  state-0
      state-1
      state-2
      state-3
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
  =|  state-3
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
          %3  
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
          ::  cards
          :: (weld cards (missing-subscriptions tracking.old))
        ==
      ==
      ::
      ++  retry-failed-kicks
        =|  acc-cards=(list card)
        =/  fail=(list resource)
          ~(tap in ~(key by tracking))
        |-  ^-  (quip card _state)
        ?~  fail
          [acc-cards state]
        =*  rid  i.fail
        =^  [crds=(list card) hook=_pull-hook]  state
          tr-abet:tr-restart-if-failed:(tr-abed:track-engine:hc rid)
        =.  pull-hook  hook
        $(fail t.fail, acc-cards (weld acc-cards crds))
      --
    ::
    ++  on-save
      ^-  vase
      =.  inner-state
        on-save:og
      !>(-.state)
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
        =^  [cards=(list card) hook=_pull-hook]  state
          tr-abet:(tr-hook-act:track-engine:hc action)
        =.  pull-hook  hook
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
      =^  [cards=(list card) hook=_pull-hook]  state
        tr-abet:(tr-sign:(tr-abed:track-engine:hc rid) sign)
      =.  pull-hook  hook
      [cards this]
    ::
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
    ::
    ++  on-fail
      |=  [=term =tang]
      ^-  [(list card:agent:gall) agent:gall]
      =^  cards  pull-hook
        (on-fail:og term tang)
      [cards this]
    ::
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
      io   ~(. agentio bowl)
      pass  pass:io
      virt  ~(. pull-hook-virt bowl)
      ver  ~(. versioning [bowl update-mark.config version.config])
  ::
  ++  track-engine
    |_  [cards=(list card) rid=resource =ship =status gone=_|]
    ::  +|  %init: state machine setup and manipulation
    ::
    ++  tr-core  .
    ++  tr-abed
      |=  r=resource
      =/  [s=^ship sta=^status]
        (~(got by tracking) r)
      tr-core(rid r, ship s, status sta)
    ::
    ++  tr-abet
      ^-  [[(list card) _pull-hook] _state]
      =.  tracking
        ?:  gone
          (~(del by tracking) rid)
        (~(put by tracking) rid [ship status])
      [(flop cards) state]
    ::
    ++  tr-emit
      |=  =card
      tr-core(cards [card cards])
    ::
    ++  tr-emis
      |=  caz=(list card)
      tr-core(cards (welp (flop cards) cards))
    ::
    ++  tr-ap-og
      |=  [caz=(list card) hook=_pull-hook]
      =.  pull-hook  hook
      (tr-emis caz)
    ::  +|  %sign: sign handling
    ::
    ::
    ++  tr-sign
      |=  =sign:agent:gall
      ?+   -.sign  !!
        %kick       tr-kick
        %watch-ack  (tr-wack +.sign)
        %fact       (tr-fact +.sign)
      ==
    ::
    ++  tr-wack
      |=  tan=(unit tang)
      ?~  tan  tr-core
      (tr-ap-og:tr-cleanup (on-pull-nack:og rid u.tan))
    ::
    ++  tr-kick
      ?.  ?=(%active -.status)  tr-core
      =/  pax=(unit (unit path))
        (kick-mule:virt rid |.((on-pull-kick:og rid)))
      ?~  pax  tr-failed-kick
      ?~  u.pax  tr-cleanup
      (tr-watch u.u.pax)
    ::
    ++  tr-fact
      |=  =cage
      ?:  ?=(%hook-meta-update p.cage)
        (tr-suspend-sub-ver !<(@ud q.cage))
      ?>  (is-root:ver p.cage)
      =/  fact-ver=@ud
        (parse:ver p.cage)
      ?.  (lth fact-ver min-version.config)
        (tr-suspend-pub-ver fact-ver)
      =/  =vase
        (convert-to:ver cage)
      =/  =wire
        (make-wire /store)
      =+  resources=(~(gas in *(set resource)) (resource-for-update:og vase))
      ?>  ?|  no-validate.config
          ?&  (check-src resources)
              (~(has in resources) rid)
          ==  ==
      (tr-emit (~(poke-our pass wire) store-name.config update-mark.config vase))
    ::  +|  %lifecycle: lifecycle management for tracked resource
    ::
    ::
    ++  tr-add
      |=  [s=^ship r=resource]
      =:  ship  s
          rid   r
          status  [%active ~]
        ==
      (tr-watch /)
    ::
    ++  tr-remove
      tr-leave:tr-cleanup
    ::
    ++  tr-hook-act
      |=  =action
      ^+  tr-core
      ?-  -.action
        %add  (tr-add +.action)
        %remove  tr-remove:(tr-abed resource.action)
      ==
    ::
    ++  tr-cleanup
      =.  gone  %.y
      (tr-emit give-update)
    ::
    ++  tr-failed-kick
      tr-core(status [%failed-kick ~])
    ::
    ++  tr-suspend-pub-ver
      |=  ver=@ud
      =.  status  [%pub-ver ver]
      tr-leave:tr-watch-ver
    ::
    ++  tr-suspend-sub-ver
      |=  ver=@ud
      tr-core(status [%sub-ver ver])
    ::
    ++  tr-restart-if-failed
      ?.  ?=(%failed-kick -.status)
        tr-core
      tr-restart
    ::
    ++  tr-restart
      =.  status  [%active ~]
      tr-kick
    ::
    ::
    ::  +|  %subscription: subscription cards
    ::
    ::
    ++  tr-ver-wire
      (make-wire /version)
    ::
    ++  tr-watch-ver
      (tr-emit (~(watch pass tr-ver-wire) tr-sub-dock /version))
    ::
    ++  tr-leave-ver
      (tr-emit (~(leave pass tr-ver-wire) tr-sub-dock))
    ++  tr-sub-wire
      (make-wire pull+resource+(en-path:resource rid))
    ::
    ++  tr-sub-dock
      ^-  dock
      [ship push-hook-name.config]
    ::
    ++  tr-check-sub
      ?:  (~(has by wex.bowl) [tr-sub-wire tr-sub-dock])
        tr-core
      tr-kick
    ::
    ++  tr-watch
      |=  pax=path
      ^+  tr-core
      =/  =path
        (welp (snoc resource+(en-path:resource rid) (scot %ud version.config)) pax)
      (tr-emit (~(watch pass tr-sub-wire) tr-sub-dock path))
    ::
    ++  tr-leave
      (tr-emit (~(leave pass tr-sub-wire) tr-sub-dock))
    --
  ++  poke-sane
    ^-  (quip card:agent:gall _state)
    =/  cards
      ~ :: restart-subscriptions
    ~?  >  ?=(^ cards)
      "Fixed subscriptions in {<dap.bowl>}"
    :_  state
    ~  :: restart-subscriptions
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
  --
--
