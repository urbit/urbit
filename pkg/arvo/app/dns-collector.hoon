/-  dns
::
::  app types and boilerplate
::
=>  |%
    +$  app-state
      $:  %0
          requested=(map ship address:dns)
          completed=(map ship binding:dns)
      ==
    +$  peek-data  [%noun (list (pair ship address:dns))]
    +$  in-poke-data
      $%  [%dns-address =address:dns]
          [%dns-complete =ship =binding:dns]
      ==
    +$  out-poke-data
      $%  [%drum-unlink =dock]
      ==
    +$  out-peer-data
      $%  [%dns-binding =binding:dns]
          [%dns-request =request:dns]
      ==
    +$  card
      $%  [%diff out-peer-data]
          [%poke wire =dock out-poke-data]
      ==
    +$  move  [bone card]
    --
::
=|  moves=(list move)
|_  [=bowl:gall state=app-state]
::
++  this  .
::
++  abet
  ^-  (quip move _this)
  [(flop moves) this(moves ~)]
::
++  emit
  |=  mov=move
  ^+  this
  this(moves [mov moves])
::
++  emil
  |=  moz=(list move)
  |-  ^+  this
  ?~  moz
    this
  $(moz t.moz, ..this (emit i.moz))
::
++  poke-app
  |=  [=wire =dock =out-poke-data]
  ^+  this
  (emit [ost.bowl %poke wire dock out-poke-data])
::
++  give-result
  |=  [=the=path =out-peer-data]
  ^+  this
  %-  emil
  %+  turn
    ^-  (list bone)
    %+  murn  ~(tap by sup.bowl)
    |=  [ost=bone =ship =sub=path]
    `(unit bone)`?.(=(the-path sub-path) ~ (some ost))
  |=  =bone
  [bone %diff out-peer-data]
::
++  prep
  |=  old=(unit app-state)
  ^-  (quip move _this)
  =<  abet
  ?~  old
    (poke-app /unlink [[our %hood] [%drum-unlink our dap]]:bowl)
  this(state u.old)
::
++  poke
  |=  =in-poke-data
  ^-  (quip move _this)
  =<  abet
  ?-  -.in-poke-data
      %dns-address
    =*  who  src.bowl
    =*  adr  address.in-poke-data
    =/  rac  (clan:title who)
    ?.  ?=(?(%king %duke) rac)
      ~|  [%dns-collector-bind-invalid who]  !!
    ?:  (reserved:eyre if.adr)
      ~|  [%dns-collector-reserved-address who if.adr]  !!
    ::
    =/  req=(unit address:dns)  (~(get by requested.state) who)
    =/  dun=(unit binding:dns)  (~(get by completed.state) who)
    ?:  &(?=(^ dun) =(adr address.u.dun))
      =.  requested.state  (~(del by requested.state) who)
      (give-result /(scot %p who) %dns-binding u.dun)
    ::
    ?:  &(?=(^ req) =(adr u.req))
      this
    ::  XX check address?
    =/  =request:dns  [who adr]
    =.  requested.state  (~(put by requested.state) request)
    (give-result /requests %dns-request request)
  ::
      %dns-complete
    ::  XX or confirm valid binding?
    ::
    ?.  (team:title [our src]:bowl)
      ~|  %complete-yoself  !!
    =*  who  ship.in-poke-data
    =*  adr  address.binding.in-poke-data
    =*  tuf  turf.binding.in-poke-data
    =/  req=(unit address:dns)  (~(get by requested.state) who)
    ::  ignore established bindings that don't match requested
    ::
    ?:  ?&  ?=(^ req)
            !=(adr u.req)
        ==
      this
    =:  requested.state  (~(del by requested.state) who)
        completed.state  (~(put by completed.state) who [adr tuf])
      ==
    (give-result /(scot %p who) %dns-binding adr tuf)
  ==
::
++  peek
  |=  =path
  ^-  (unit (unit peek-data))
  ~&  path
  ?+  path  [~ ~]
      [%x %requested ~]
    [~ ~ %noun ~(tap by requested.state)]
  ==
::
++  peer
  |=  =path
  ^-  (quip move _this)
  =<  abet
  ::  will be immediately unlinked, see +prep
  ::
  ?:  ?=([%sole *] path)
    this
  ?.  ?=([@ ~] path)
    ~|  %invalid-path  !!
  ?:  ?=(%requests i.path)
    =/  requests  ~(tap by requested.state)
    |-  ^+  this
    =*  loop  $
    ?~  requests
      this
    =.  ..this  (give-result path %dns-request i.requests)
    loop(requests t.requests)
  ::
  =/  who  (slaw %p i.path)
  ?~  who
    ~|  %invalid-path  !!
  ?~  dun=(~(get by completed.state) who)
    this
  (give-result path %dns-binding u.dun)
--
