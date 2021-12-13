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
      [%pub-ver ver=@ud]
      [%sub-ver ver=@ud]
  ==
::
+$  base-state-2
  $:  tracking=(map resource track)
      inner-state=vase
  ==
::
+$  base-state-3
  $:  prev-version=@ud
      prev-min-version=@ud
      base-state-2
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
+$  state-4  [%4 base-state-3]
::
+$  versioned-state 
  $%  state-0
      state-1
      state-2
      state-3
      state-4
  ==
::  +diplomatic: only renegotiate if versions changed
::    
::    If %.n please leave note as to why renegotiation necessary
::    
::
++  diplomatic
  ^-  ?
  %.y
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
  =|  state-4
  =*  state  -
  ^-  agent:gall
  =<
    |_  =bowl:gall
    +*  this  .
        og   ~(. pull-hook bowl)
        hc   ~(. +> bowl)
        def  ~(. (default-agent this %|) bowl)
        ver  ~(. versioning [bowl [update-mark version min-version]:config])
        io   ~(. agentio bowl)
        pass  pass:io
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
          %4
        =^  og-cards   pull-hook
          (on-load:og inner-state.old)
        =.  state  old
        =/  kick=(list card)
          ?:  ?&  =(min-version.config prev-min-version.old)
                  =(version.config prev-version.old)
                  diplomatic
              ==
            ~
          (poke-self:pass kick+!>(%.n))^~
        :_  this
        :(weld cards og-cards kick)
       ::
          %3  $(old [%4 0 0 +.old])
          %2  $(old (state-to-3 old))
          %1  $(old [%2 +.old ~])
          %0  !!  :: pre-breach
      ==
      ::
      ++  state-to-3
        |=  old=state-2
        %*  .  *state-3
          tracking     (tracking-to-3 tracking.old)
          inner-state  inner-state.old
        ==
      ::
      ++  tracking-to-3
        |=  trk=(map resource ship)
        %-  ~(gas by *(map resource track))
        %+  turn  ~(tap by trk)
        |=  [=resource =ship]
        :-  resource
        [ship %active ~]
      ::
      --
    ::
    ++  on-save
      ^-  vase
      =:  inner-state       on-save:og
          prev-min-version  min-version.config
          prev-version      version.config
        ==
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
          %kick
        ?>  (team:title [our src]:bowl)
        =+  !<(nice=? vase)
        =^  [cards=(list card:agent:gall) hook=_pull-hook]  state
          (restart-subs:hc nice)
        =.  pull-hook  hook
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
          tr-abet:(tr-hook-act:track-engine:hc !<(action vase))
        =.  pull-hook  hook
        [cards this]
      ==
    ::
    ++  on-watch
      |=  =path
      ^-  [(list card:agent:gall) agent:gall]
      ?>  (team:title our.bowl src.bowl)
      ?+    path
      ::  forward by default
        =^  cards  pull-hook
          (on-watch:og path)
        [cards this]
      ::
        [%nack ~]  `this
      ::
          [%tracking ~]
        :_  this
        ~[give-update]
      ==
    ::
    ++  on-agent
      |=  [=wire =sign:agent:gall]
      ^-  [(list card:agent:gall) agent:gall]
      ?.  ?=([%helper %pull-hook @ *] wire)
        =^  cards  pull-hook
          (on-agent:og wire sign)
        [cards this]
      ?:  ?=([%version ~] t.t.wire)
        =^  [cards=(list card) hook=_pull-hook]  state
          (take-version:hc src.bowl sign)
        =.  pull-hook  hook
        [cards this]
      ?.  ?=([%pull ?(%unver-resource %resource) *] t.t.wire)
        (on-agent:def wire sign)
      =/  rid=resource
        (de-path:resource t.t.t.t.wire)
      =/  versioned=?
        ?=(%resource i.t.t.t.wire)
      =^  [cards=(list card) hook=_pull-hook]  state
        tr-abet:(tr-sign:(tr-abed:track-engine:hc rid) sign versioned)
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
      ver  ~(. versioning [bowl [update-mark version min-version]:config])
  ::
  ++  restart-subs
    |=  nice=?
    =|  acc-cards=(list card)
    =/  subs=(list resource)
      ~(tap in ~(key by tracking))
    |-  ^-  [[(list card) _pull-hook] _state]
    ?~  subs
      [[acc-cards pull-hook] state]
    =*  rid  i.subs
    =^  [crds=(list card) hook=_pull-hook]  state
      tr-abet:(tr-on-load:(tr-abed:track-engine rid) nice)
    =.  pull-hook  hook
    $(subs t.subs, acc-cards (weld acc-cards crds))

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
      [[(flop cards) pull-hook] state]
    ::
    ++  tr-emit
      |=  =card
      tr-core(cards [card cards])
    ::
    ++  tr-emis
      |=  caz=(list card)
      tr-core(cards (welp (flop caz) cards))
    ::
    ++  tr-ap-og
      |=  ap=_^?(|.(*(quip card _pull-hook)))
      =^  caz  pull-hook
        (ap)
      (tr-emis caz)
    ::
    ++  tr-sane
      ^-  ?
      ?+  -.status  %.y
        %active  (~(has by wex.bowl) [tr-sub-wire tr-sub-dock])
      ==
    ::  +|  %sign: sign handling
    ::
    ::
    ++  tr-sign
      |=  [=sign:agent:gall versioned=?]
      |^
      ?+   -.sign  !!
        %kick       tr-kick
        %watch-ack  (tr-wack +.sign)
        %fact       (tr-fact +.sign)
      ==
      ::
      ++  tr-wack
        |=  tan=(unit tang)
        ?~  tan  tr-core
        ?.  versioned
          %-  tr-ap-og:tr-cleanup:tr-give-nack 
          |.((on-pull-nack:og rid u.tan))
        %-  (slog leaf+"versioned nack for {<rid>} in {<dap.bowl>}" u.tan)
        =/  pax
          (kick-mule:virt rid |.((on-pull-kick:og rid)))
        ?~  pax  tr-failed-kick
        ?~  u.pax  tr-cleanup
        (tr-watch-unver u.u.pax)
      ::
      ++  tr-fact
        |=  =cage
        ?:  ?=(%version p.cage)
          =/  req-ver=@ud
            !<(@ud q.cage)
          ?:  (lth req-ver min-version.config)
            (tr-suspend-pub-ver min-version.config)
          (tr-suspend-sub-ver req-ver)
        ?>  (is-root:ver p.cage)
        =/  fact-ver=@ud
          (read-version:ver p.cage)
        ?.  (gte fact-ver min-version.config)
          ?.  versioned
            ::  don't process unversioned, unsupported facts
            ::  just wait for publisher to upgrade and kick the
            ::  subscription
            tr-core
          (tr-suspend-pub-ver min-version.config)
        =/  =^cage
          (convert-to:ver cage)
        =/  =wire
          (make-wire /store)
        =+  resources=(~(gas in *(set resource)) (resource-for-update:og q.cage))
        ?>  ?|  no-validate.config
            ?&  (check-src resources)
                (~(has in resources) rid)
            ==  ==
        =/  =mark
          (append-version:ver version.config)
        (tr-emit (~(poke-our pass wire) store-name.config cage))
      --
    ::
    ++  tr-kick
      ?.  ?=(%active -.status)  tr-core
      =/  pax
        (kick-mule:virt rid |.((on-pull-kick:og rid)))
      ?~  pax  tr-failed-kick
      ?~  u.pax  tr-cleanup
      (tr-watch u.u.pax)
    ::  +|  %lifecycle: lifecycle management for tracked resource
    ::
    ::
    ++  tr-add
      |=  [s=^ship r=resource]
      ?<  =(s our.bowl)
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
      ::
          %remove
        ?.  (~(has by tracking) resource.action)
          tr-core
        tr-remove:(tr-abed resource.action)
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
    ::  
    ++  tr-suspend-sub-ver
      |=  ver=@ud
      tr-core(status [%sub-ver ver])
    ::
    ++  tr-on-load
      |=  nice=?
      ?+  -.status  tr-core
        %failed-kick  tr-restart
        %active       ?:(&(tr-sane nice) tr-core tr-rewatch)
      ::
          %sub-ver
        ?.  (supported:ver (append-version:ver ver.status))
          tr-core
        tr-restart
      ==
    ::
    ++  tr-restart
      =.  status  [%active ~]
      tr-kick
    ::
    ++  tr-rewatch
      tr-kick:tr-leave
    ::
    ::
    ::  +|  %subscription: subscription cards
    ::
    ::
    ++  tr-give-nack
      (tr-emit (fact:io resource+!>(rid) /nack ~))
    ::
    ++  tr-ver-wire
      (make-wire /version)
    ::
    ++  tr-watch-ver
      (tr-emit (watch-version ship))
    ::
    ++  tr-leave-ver
      (tr-emit (~(leave pass tr-ver-wire) tr-sub-dock))
    ++  tr-sub-wire
      (make-wire pull+resource+(en-path:resource rid))
    ++  tr-unver-sub-wire
      (make-wire pull+unver-resource+(en-path:resource rid))
    ::
    ++  tr-sub-dock
      ^-  dock
      [ship push-hook-name.config]
    ::
    ++  tr-watch-unver
      |=  pax=path
      =/  =path
        :-  %resource
        (weld (en-path:resource rid) pax)
      (tr-emit (~(watch pass tr-unver-sub-wire) tr-sub-dock path))
    ::
    ++  tr-watch
      |=  pax=path
      ^+  tr-core
      =/  =path
        :+  %resource  %ver
        %+  weld
          (snoc (en-path:resource rid) (scot %ud version.config))
        pax
      (tr-emit (~(watch pass tr-sub-wire) tr-sub-dock path))
    ::
    ++  tr-leave
      (tr-emit (~(leave pass tr-sub-wire) tr-sub-dock))
    --
  ::
  ++  take-version
    |=  [who=ship =sign:agent:gall]
    ^-  [[(list card) _pull-hook] _state]
    ?+  -.sign  !!
        %watch-ack
      ?~  p.sign  [~^pull-hook state]
      =/  =tank  leaf+"subscribe failed from {<dap.bowl>} on wire {<wire>}"
      %-  (slog tank u.p.sign)
      [~^pull-hook state]
      ::
        %kick
      :_  state
      [(watch-version who)^~ pull-hook]
      ::
        %fact
      ?.  =(%version p.cage.sign)
        [~^pull-hook state]
      =+  !<(version=@ud q.cage.sign)
      =/  tracks=(list [rid=resource =track])
        ~(tap by tracking)
      =|  cards=(list card)
      =|  leave=_&
      |-
      ?~  tracks
        =?  cards  leave
          :_(cards (leave-version who))
        [[cards pull-hook] state]
      ?.  ?=(%pub-ver -.status.track.i.tracks)
        $(tracks t.tracks)
      ?.  =(who ship.track.i.tracks)
        $(tracks t.tracks)
      ?.  =(ver.status.track.i.tracks version)
        =.  leave  %.n
        $(tracks t.tracks)
      =^  [caz=(list card) hook=_pull-hook]  state
        tr-abet:tr-restart:(tr-abed:track-engine rid.i.tracks)
      =.  pull-hook  hook
      $(tracks t.tracks, cards (weld cards caz))
    ==
  ::
  ++  version-wir 
    (make-wire /version)
  ::
  ++  version-dock
    |=  =ship
    ^-  dock
    [ship push-hook-name.config]
  ::
  ++  watch-version
    |=  =ship
    (~(watch pass version-wir) [ship push-hook-name.config] /version)
  ::
  ++  leave-version
    |=  =ship
    (~(leave pass version-wir) [ship push-hook-name.config])
  ::
  ++  poke-sane
    ^-  (quip card:agent:gall _state)
    =/  cards
      ::  TODO revive
      ~ :: restart-subscriptions
    ~?  >  ?=(^ cards)
      "Fixed subscriptions in {<dap.bowl>}"
    [cards state]

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
    ?~  status=(~(get by tracking) rid)
      %.n
    =(src.bowl ship.u.status)
  --
--
