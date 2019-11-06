/-  dns
/+  default-agent
::
::  app types and boilerplate
::
=>  |%
    +$  card  card:agent:mall
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
    +$  out-peer-data
      $%  [%dns-binding =binding:dns]
          [%dns-request =request:dns]
      ==
    --
|%
++  give-result
  |=  [=the=path =cage]
  ^-  card
  [%give %subscription-update `the-path cage]
--
::
^-  agent:mall
=|  state=app-state
|_  =bowl:mall
+*  this  .
    def   ~(. default-agent bowl this)
::
++  handle-init
  ^-  (quip card _this)
  :_  this

  ^-  (list card)
  [%pass /unlink %agent [our.bowl %hood] %poke %drum-unlink !>([our dap]:bowl)]~
::
++  handle-extract-state   !>(state)
++  handle-upgrade-state
  |=  old=vase
  ~&  >  'grand'
  `this(state !<(app-state old))
::
++  handle-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?+  mark  (handle-poke:def mark vase)
    %noun          (handle-noun !<(noun vase))
    %dns-address   (handle-dns-address !<(address:dns vase))
    %dns-complete  (handle-dns-complete !<([ship binding:dns] vase))
  ==
  ::
  ++  handle-noun
    |=  noun=*
    ^-  (quip card _this)
    ?:  ?=(%debug noun)
      ~&  bowl=bowl
      ~&  state=state
      `this
    ::
    ~&  %poke-unknown
    `this
  ::
  ++  handle-dns-address
    |=  adr=address:dns
    ^-  (quip card _this)
    =*  who  src.bowl
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
      :_  this  :_  ~
      (give-result /(scot %p who) %dns-binding !>(u.dun))
    ::
    ?:  &(?=(^ req) =(adr u.req))
      `this
    ::  XX check address?
    =/  =request:dns  [who adr]
    =.  requested.state  (~(put by requested.state) request)
    :_  this  :_  ~
    (give-result /requests %dns-request !>(request))
  ::
  ++  handle-dns-complete
    |=  [who=ship =binding:dns]
    ^-  (quip card _this)
    ::  XX or confirm valid binding?
    ::
    ?.  (team:title [our src]:bowl)
      ~|  %complete-yoself  !!
    =*  adr  address.binding
    =*  tuf  turf.binding
    =/  req=(unit address:dns)  (~(get by requested.state) who)
    ::  ignore established bindings that don't match requested
    ::
    ?:  ?|  ?=(~ req)
            !=(adr u.req)
        ==
      ~&  %unknown-complete
      `this
    =:  requested.state  (~(del by requested.state) who)
        completed.state  (~(put by completed.state) who [adr tuf])
      ==
    :_  this  :_  ~
    (give-result /(scot %p who) %dns-binding !>([adr tuf]))
  --
::
++  handle-subscribe
  |=  =path
  ^-  (quip card _this)
  ::  will be immediately unlinked, see +prep
  ::
  ?:  ?=([%sole *] path)
    `this
  ?.  ?=([@ ~] path)
    ~|  %invalid-path  !!
  ?:  ?=(%requests i.path)
    =/  requests  ~(tap by requested.state)
    |-  ^-  (quip card _this)
    =*  loop  $
    ?~  requests
      `this
    =/  card  (give-result path %dns-request !>(i.requests))
    =^  cards  this  loop(requests t.requests)
    [[card cards] this]
  ::
  =/  who=(unit @p)  (slaw %p i.path)
  ?~  who
    ~|  %invalid-path  !!
  ?~  dun=(~(get by completed.state) u.who)
    `this
  :_  this  :_  ~
  (give-result path %dns-binding !>(u.dun))
::
++  handle-unsubscribe     handle-unsubscribe:def
++  handle-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  [~ ~]
      [%x %requested ~]  [~ ~ %requested !>(~(tap by requested.state))]
      [%x %completed ~]  [~ ~ %completed !>(~(tap by completed.state))]
  ==
::
++  handle-agent-response  handle-agent-response:def
++  handle-arvo-response   handle-arvo-response:def
++  handle-error           handle-error:def
--
