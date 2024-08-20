::  lib/push-hook: helper for creating a push hook
::  
::   lib/push-hook is a helper for automatically pushing data from a
::   local store to the corresponding pull-hook on remote ships. It also
::   proxies remote pokes to the store.
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
::   /resource/[resource]: Receive initial state and updates to
::   .resource. .resource should be encoded with en-path:resource from
::   /lib/resource. Facts on this path will be of mark
::   update-mark.config
::
::   ##  Pokes
::
::   %push-hook-action: Add/remove a resource from pushing.
::   [update-mark.config]: A poke to proxy to the local store or a
::   foreign push-hook
::
/-  *push-hook
/+  default-agent, resource, verb, versioning, agentio
~%  %push-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
::  $config: configuration for the push hook
::  
::    .store-name: name of the store to proxy pokes and
::    subscriptions to
::    .store-path: subscription path to receive updates on
::    .update-mark: mark that updates will be tagged with
::    .pull-hook-name: name of the corresponding pull-hook
::
+$  config
  $:  store-name=term
      store-path=path
      update=mold
      update-mark=term
      pull-hook-name=term
      version=@ud
      min-version=@ud
  ==
::
::  $base-state-0: state for the push hook
::
::    .sharing: resources that the push hook is proxying
::    .inner-state: state given to internal door
::
+$  base-state-0
  $:  sharing=(set resource)
      inner-state=vase
  ==
::
+$  base-state-1
  $:  prev-version=@ud
      prev-min-version=@ud
      base-state-0
  ==
::
+$  state-0  [%0 base-state-0]
::
+$  state-1  [%1 base-state-0]
+$  state-2  [%2 base-state-1]
::
+$  versioned-state
  $%  state-0
      state-1
      state-2
  ==
::  +diplomatic: only renegotiate if versions changed
::    
::    If %.n please leave note as to why renegotiation necessary
::
++  diplomatic
  ^-  ?
  %.y
::
++  push-hook
  ~/  %push-hook
  |*  =config
  $_  ^|
  |_  bowl:gall
  ::
  ::  +resource-for-update: get affected resources from an update
  ::  
  ::    Given a vase of the update, the mark of which is
  ::    update-mark.config, produce the affected resources, if any.
  ::
  ++  resource-for-update
    |~  vase
    *(list resource)
  ::
  ::  +take-update: handle update from store
  ::
  ::    Given an update from the store, do other things after proxying
  ::    the update
  ::
  ++  take-update
    |~  vase
    *[(list card) _^|(..on-init)]
  ::  +transform-proxy-update: optionally transform update
  ::
  ::  If ^ is produced, then the update is forwarded to the local
  ::  store. If ~ is produced, the update is not forwarded and the
  ::  poke fails.
  ::
  ++  transform-proxy-update
    |~  vase
    *[(list card) (unit vase)]
  ::  +initial-watch: produce initial state for a subscription
  ::
  ::    .resource is the resource being subscribed to.
  ::    .path is any additional information in the subscription wire.
  ::    This would typically be used to encode state that the subscriber
  ::    already has. For example, a chat client might encode
  ::    the number of messages that it already has, or the date it last
  ::    received an update.
  ::
  ::    If +initial-watch crashes, the subscription fails.
  ::
  ++  initial-watch
    |~  [path resource]
    *vase
  ::  from agent:gall
  ::
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
    |~  cage
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
  |=  =(push-hook config)
  =|  state-2
  =*  state  -
  ^-  agent:gall
  =<
    ~%  %push-agent-lib  ..poke-hook-action  ~
    |_  =bowl:gall
    +*  this  .
        og   ~(. push-hook bowl)
        hc   ~(. +> bowl)
        def  ~(. (default-agent this %|) bowl)
        io   ~(. agentio bowl)
        pass  pass:io
        ver  ~(. versioning [bowl [update-mark version min-version]:config])
    ::
    ++  on-init
      =^  cards  push-hook
        on-init:og
      :_  this
      [watch-store:hc cards]
    ::
    ++  on-load
      |=  =old=vase
      =/  old
        !<(versioned-state old-vase)
      =|  cards=(list card:agent:gall)
      |^ 
      ?-  -.old
          %2
        =^  og-cards   push-hook
          (on-load:og inner-state.old)
        =/  old-subs
          (find-old-subs [prev-version prev-min-version]:old)
        =/  version-cards
          :-  (fact:io version+!>(version.config) /version ~)
          ?~  old-subs  ~
          (kick:io old-subs)^~
        [:(weld cards og-cards version-cards) this(state old)]
        ::
          %1
        %_    $
          old  [%2 0 0 +.old]
        ==
        ::
        ::
          %0
        %_    $
            -.old  %1
          ::
            cards
          =/  paths=(list path)
            kicked-watches
          ?~  paths  cards
          :_   cards
          [%give %kick paths ~]
        ==
      ==
      ::
      ++  find-old-subs
        |=  [prev-min-version=@ud prev-version=@ud]
        ?:  ?&  =(min-version.config prev-min-version)
                =(prev-version version.config)
                diplomatic
            ==
          ::  bail on kick if we didn't change versions
          ~
        %~  tap  in
        %+  roll
          ~(val by sup.bowl)
        |=  [[=ship =path] out=(set path)]
        ?.  ?=([%resource *] path)  out
        ?.  ?=([%resource %ver] path)
          (~(put in out) path)
        =/  path-ver=@ud
          (ver-from-path:hc path)
        ?:  (supported:ver (append-version:ver path-ver))  out
        (~(put in out) path)
      ::
      ++  kicked-watches
        ^-  (list path)
        %~  tap  in
        %+  roll
          ~(val by sup.bowl)
        |=  [[=ship =path] out=(set path)]
        ?~  path   out
        ?.  (lth 4 (lent path))
          out
        (~(put in out) path)
      --
    ::
    ++  on-save
      =:  prev-version      version.config
          prev-min-version  min-version.config
          inner-state       on-save:og
        ==
      !>(state)
    ::
    ++  on-poke
      ~/  %on-poke
      |=  [=mark =vase]
      ^-  (quip card:agent:gall agent:gall)
      ?:  =(mark %kick)
        ?>  (team:title [our src]:bowl)
        :_  this
        (kick:io (turn ~(val by sup.bowl) tail))^~
      ?:  =(mark %push-hook-action)
        ?>  (team:title our.bowl src.bowl)
        =^  cards  state
          (poke-hook-action:hc !<(action vase))
        [cards this]
      ::
      ?:  (is-root:ver mark)
        :_  this
        (forward-update:hc mark vase)
      =^  cards  push-hook
        (on-poke:og mark vase)
      [cards this]
    ::
    ++  on-watch
      ~/  %on-watch
      |=  =path
      ^-  (quip card:agent:gall agent:gall)
      ?:  ?=([%version ~] path)
        :_  this
        (fact-init:io version+!>(min-version.config))^~
      ?.  ?=([%resource *] path)
        =^  cards  push-hook
          (on-watch:og path)
        [cards this]
      |^
      ?.  ?=([%ver %ship @ @ @ *] t.path)
        unversioned
      =/  =resource
        (de-path:resource t.t.path)
      =/  requested=@ud
        (slav %ud i.t.t.t.t.t.path)
      =/  =mark
        (append-version:ver (min requested version.config))
      ?.  (supported:ver mark)
        :_  this
        (fact-init-kick:io version+!>(min-version.config))
      :_  this
      =-  [%give %fact ~ -]~
      (convert-to:ver mark (initial-watch:og t.t.t.t.t.t.path resource))
      ::
      ++  unversioned
        ?>  ?=([%ship @ @ *] t.path)
        =/  =resource
          (de-path:resource t.path)
        =/   =vase 
          (initial-watch:og t.t.t.t.path resource)
        :_  this
        ?.  =(min-version.config 0)
           ~&  >>>  "unversioned req from: {<src.bowl>}, nooping"
           ~
        [%give %fact ~ (convert-to:ver update-mark.config vase)]~
      --
    ::
    ++  on-agent
      ~/  %on-agent
      |=  [=wire =sign:agent:gall]
      ^-  (quip card:agent:gall agent:gall)
      ?.  ?=([%helper %push-hook @ *] wire)
        =^  cards  push-hook
          (on-agent:og wire sign)
        [cards this]
      ?.  ?=(%store i.t.t.wire)
        (on-agent:def wire sign)
      ?+   -.sign  (on-agent:def wire sign)
        %kick  [~[watch-store:hc] this]
      ::
          %fact
        ?.  (is-root:ver p.cage.sign)
          =^  cards  push-hook
            (on-agent:og wire sign)
          [cards this]
        =^  cards  push-hook
          (take-update:og q.cage.sign)
        :_  this
        %+  weld
          (push-updates:hc cage.sign)
        cards
      ==
      ::
      ++  on-leave
        |=  =path
        =^  cards  push-hook
          (on-leave:og path)
        [cards this]
      ::
      ++  on-arvo
        |=  [=wire =sign-arvo]
        =^  cards  push-hook
          (on-arvo:og wire sign-arvo)
        [cards this]
      ::
      ++  on-fail
        |=  [=term =tang]
        =^  cards  push-hook
          (on-fail:og term tang)
        [cards this]
      ::
      ++  on-peek   
        |=  =path
        ^-  (unit (unit cage))
        ?:  =(/x/dbug/state path)
          ``noun+(slop !>(state(inner-state *vase)) on-save:og)
        ?+  path  (on-peek:og path)
          [%x %sharing ~]  ``noun+!>(sharing)
          [%x %version ~]  ``version+!>(version.config)
          [%x %min-version ~]  ``version+!>(version.config)
        ==
    --
  ~%  %push-helper-lib  ..card  ~
  |_  =bowl:gall
  +*  og   ~(. push-hook bowl)
      ver  ~(. versioning [bowl [update-mark version min-version]:config])
      io   ~(. agentio bowl)
      pass  pass:io
  ::
  ++  poke-hook-action
    ~/  %poke-hook-action
    |=  =action
    ^-  (quip card:agent:gall _state)
    |^
    ?-  -.action
      %add     (add +.action)
      %remove  (remove +.action)
      %revoke  (revoke +.action)
    ==
    ::
    ++  add
      |=  rid=resource
      =.  sharing
        (~(put in sharing) rid)
      `state
    ::
    ++  remove
      |=  rid=resource
      =/  pax=path
        [%resource (en-path:resource rid)]
      =/  paths=(set path)
        %-  silt
        %+  turn
          (incoming-subscriptions pax)
        |=([ship pox=path] pox)
      =.  sharing
        (~(del in sharing) rid)
      :_  state
      [%give %kick ~(tap in paths) ~]~
    ::
    ++  revoke
      |=  [ships=(set ship) rid=resource]
      =/  ver-pax=path
        [%resource %ver (en-path:resource rid)]
      =/  unver-pax=path
        [%resource (en-path:resource rid)]
      :_  state
      %+  murn
        %+  welp  (incoming-subscriptions unver-pax)
        (incoming-subscriptions ver-pax)
      |=  [her=ship =path]
      ^-  (unit card)
      ?.  (~(has in ships) her)
        ~
      `[%give %kick ~[path] `her]
    --
  ::
  ++  incoming-subscriptions
    |=  prefix=path
    ^-  (list (pair ship path))
    %+  skim
      ~(val by sup.bowl)
    |=  [him=ship pax=path]
    =/  idx=(unit @)
      (find prefix pax)
    ?~  idx  %.n
    =(u.idx 0)
  ::
  ++  make-wire
    |=  =wire
    ^+  wire
    %+  weld
      /helper/push-hook
    wire
  ::
  ++  watch-store
    ^-  card:agent:gall
    =/  =wire
      (make-wire /store)
    [%pass wire %agent [our.bowl store-name.config] %watch store-path.config]
  ::
  ++  push-updates
    ~/  %push-updates
    |=  =cage
    ^-  (list card:agent:gall)
    %+  roll  (resource-for-update q.cage)
    |=  [rid=resource cards=(list card)]
    |^
    :(weld cards versioned unversioned)
    ::
    ++  versioned
      ^-  (list card:agent:gall)
      =/  prefix=path
        resource+ver+(en-path:resource rid)
      =/  paths=(jug @ud path)
        %+  roll
          (incoming-subscriptions prefix)
        |=  [[ship =path] out=(jug @ud path)]
        =/  path-ver=@ud
          (ver-from-path path)
        (~(put ju out) path-ver path)
      %+  turn  ~(tap by paths)
      |=  [fact-ver=@ud paths=(set path)]
      =/  =mark
        (append-version:ver (min version.config fact-ver))
      (fact:io (convert-from:ver mark q.cage) ~(tap in paths))
    ::  TODO: deprecate
    ++  unversioned
      ?.  =(min-version.config 0)  ~
      =/  prefix=path
        resource+(en-path:resource rid)
      =/  unversioned=(set path)
        %-  ~(gas in *(set path))
        (turn (incoming-subscriptions prefix) tail)
      ?:  =(0 ~(wyt in unversioned))  ~
      (fact:io (convert-from:ver update-mark.config q.cage) ~(tap in unversioned))^~
    --
  ::
  ++  forward-update
    ~/  %forward-update
    |=  =cage
    ^-  (list card:agent:gall)
    =-  lis
    =/  vas=vase
      q:(convert-to:ver cage)
    %+  roll  (resource-for-update q.cage)
    |=  [rid=resource [lis=(list card:agent:gall) tf-vas=(unit vase)]]
    ^-  [(list card:agent:gall) (unit vase)]
    =/  =path
      resource+(en-path:resource rid)
    =*  ship   entity.rid
    =/  out=(pair (list card:agent:gall) (unit vase))
      ?.  =(our.bowl ship)
        ::  do not transform before forwarding
        ::
        ``vas
      ::  use cached transform
      ::
      ?^  tf-vas  `tf-vas
      ::  transform before poking store
      ::
      (transform-proxy-update:og vas)
    ~|  "forwarding failed during transform. mark: {<p.cage>} rid: {<rid>}"
    ?>  ?=(^ q.out)
    :_  q.out
    :_  (weld lis p.out)
    =/  =wire  (make-wire path)
    =-  [%pass wire %agent - %poke [current-version:ver u.q.out]]
    :-  ship
    ?.  =(our.bowl ship)
      ::  forward to host
      ::
      dap.bowl
    ::  poke our store
    ::
    store-name.config
  ::
  ++  ver-from-path
    |=  =path
    =/  extra=^path
      (slag 5 path)
    ?>  ?=(^ extra)
    (slav %ud i.extra)
  ::
  ++  resource-for-update
    ~/  %resource-for-update
    |=  =vase
    ^-  (list resource)
    %~  tap  in
    %-  silt
    (resource-for-update:og vase)
  --
--
