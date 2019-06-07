/-  dns
/+  tapp, stdio
::
::  tapp types and boilerplate
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
    +$  out-poke-data  ~
    +$  in-peer-data   ~
    +$  out-peer-data
      $%  [%dns-binding =binding:dns]
          [%dns-request =request:dns]
      ==
    ++  tapp   (^tapp app-state peek-data in-poke-data out-poke-data in-peer-data out-peer-data)
    ++  stdio  (^stdio out-poke-data out-peer-data)
    --
::
=*  tapp-async    tapp-async:tapp
=*  default-tapp  default-tapp:tapp
%-  create-tapp-all:tapp
^-  tapp-core-all:tapp
|_  [=bowl:gall state=app-state]
::
++  handle-init  handle-init:default-tapp
++  handle-diff  handle-diff:default-tapp
++  handle-take  handle-take:default-tapp
::
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?-  -.in-poke-data
      %dns-address
    =*  who  src.bowl
    =*  adr  address.in-poke-data
    =/  rac  (clan:title who)
    ?.  ?=(?(%king %duke) rac)
      ~&  [%dns-collector-bind-invalid who]
      (pure:m state)
    ?:  (reserved:eyre if.adr)
      ~&  [%dns-collector-reserved-address who if.adr]
      (pure:m state)
    ::
    =/  req=(unit address:dns)  (~(get by requested.state) who)
    =/  dun=(unit binding:dns)  (~(get by completed.state) who)
    ?:  &(?=(^ dun) =(adr address.u.dun))
      ;<  ~  bind:m  (give-result:stdio /(scot %p who) %dns-binding u.dun)
      =.  requested.state  (~(del by requested.state) who)
      (pure:m state)
    ?:  &(?=(^ req) =(adr u.req))
      (pure:m state)
    ::  XX check address?
    =/  =request:dns  [who adr]
    =.  requested.state  (~(put by requested.state) request)
    ;<  ~  bind:m  (give-result:stdio /requests %dns-request request)
    (pure:m state)
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
      (pure:m state)
    =:  requested.state  (~(del by requested.state) who)
        completed.state  (~(put by completed.state) who [adr tuf])
      ==
    ;<  ~  bind:m  (give-result:stdio /(scot %p who) %dns-binding adr tuf)
    (pure:m state)
  ==
::
++  handle-peek
  |=  =path
  ^-  (unit (unit peek-data))
  ~&  path
  ?+  path  [~ ~]
      [%x %requested ~]
    [~ ~ %noun ~(tap by requested.state)]
  ==
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ?:  ?=([%sole *] path)
    ~|  %default-tapp-no-sole  !!
  ?.  ?=([@ ~] path)
    ~|  %invalid-path  !!
  ?:  ?=(%requests i.path)
    =/  requests  ~(tap by requested.state)
    |-  ^-  form:m
    =*  loop  $
    ?~  requests
      (pure:m state)
    ;<  ~  bind:m  (give-result:stdio path %dns-request i.requests)
    loop(requests t.requests)
  ::
  =/  who  (slaw %p i.path)
  ?~  who
    ~|  %invalid-path  !!
  ?~  dun=(~(get by completed.state) who)
    (pure:m state)
  ;<  ~  bind:m  (give-result:stdio path %dns-binding u.dun)
  (pure:m state)
--
