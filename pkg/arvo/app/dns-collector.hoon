/-  dns
/+  writer
::
::  app types and boilerplate
::
=>  |%
    +$  app-state
      $:  %0
          requested=(map ship address:dns)
          completed=(map ship binding:dns)
      ==
    +$  peek-data
      $%  [%requested (list (pair ship address:dns))]
          [%completed (list (pair ship binding:dns))]
      ==
    +$  in-poke-data
      $%  [%dns-address =address:dns]
          [%dns-complete =ship =binding:dns]
          [%noun noun=*]
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
=/  move-writer  (writer ,move)
|_  [=bowl:gall state=app-state]
::
++  this  .
::
++  poke-app
  |=  [=wire =dock =out-poke-data]
  ^-  (move-writer ~)
  (tell:move-writer [ost.bowl %poke wire dock out-poke-data])
::
++  give-result
  |=  [=the=path =out-peer-data]
  ^-  (move-writer ~)
  %-  rant:move-writer
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
  =/  m  (move-writer ,_this)
  ^-  (quip move _this)
  ?~  old
    ;<  ~  bind:m  (poke-app /unlink [[our %hood] [%drum-unlink our dap]]:bowl)
    (pure:m this)
  (pure:m this(state u.old))
::
++  poke
  |=  =in-poke-data
  =/  m  (move-writer ,_this)
  ^-  (quip move _this)
  ?-  -.in-poke-data
      %noun
    ?:  ?=(%debug noun.in-poke-data)
      ~&  bowl
      ~&  state
      (pure:m this)
    ::
    ~&  %poke-unknown
    (pure:m this)
  ::
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
      ;<  ~  bind:m  (give-result /(scot %p who) %dns-binding u.dun)
      (pure:m this)
    ::
    ?:  &(?=(^ req) =(adr u.req))
      (pure:m this)
    ::  XX check address?
    =/  =request:dns  [who adr]
    =.  requested.state  (~(put by requested.state) request)
    ;<  ~  bind:m  (give-result /requests %dns-request request)
    (pure:m this)
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
    ?:  ?|  ?=(~ req)
            !=(adr u.req)
        ==
      ~&  %unknown-complete
      (pure:m this)
    =:  requested.state  (~(del by requested.state) who)
        completed.state  (~(put by completed.state) who [adr tuf])
      ==
    ;<  ~  bind:m  (give-result /(scot %p who) %dns-binding adr tuf)
    (pure:m this)
  ==
::
++  peek
  |=  =path
  ^-  (unit (unit peek-data))
  ?+  path  [~ ~]
      [%x %requested ~]
    [~ ~ %requested ~(tap by requested.state)]
  ::
      [%x %completed ~]
    [~ ~ %completed ~(tap by completed.state)]
  ==
::
++  peer
  |=  =path
  =/  m  (move-writer ,_this)
  ^-  (quip move _this)
  ::  will be immediately unlinked, see +prep
  ::
  ?:  ?=([%sole *] path)
    (pure:m this)
  ?.  ?=([@ ~] path)
    ~|  %invalid-path  !!
  ?:  ?=(%requests i.path)
    =/  requests  ~(tap by requested.state)
    |-  ^-  m
    =*  loop  $
    ?~  requests
      (pure:m this)
    ;<  ~  bind:m  (give-result path %dns-request i.requests)
    loop(requests t.requests)
  ::
  =/  who=(unit @p)  (slaw %p i.path)
  ?~  who
    ~|  %invalid-path  !!
  ?~  dun=(~(get by completed.state) u.who)
    (pure:m this)
  ;<  ~  bind:m  (give-result path %dns-binding u.dun)
  (pure:m this)
--
