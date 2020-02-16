/+  default-agent, verb, eth-abi, *server
::
/=  erc20-abi
  /;  parse-contract:eth-abi
  /:  /===/app/eth-wallet/erc20  /json/
::
::  TODO: uncomment for web interface
::
::/=  index
::  /^  $-(json manx)
::  /:  /===/app/eth-wallet/index  /!noun/
::::
::/=  script
::  /^  octs
::  /;  as-octs:mimes:html
::  /|  /:  /===/app/eth-wallet/js/index  /js/
::      /~  ~
::  ==
::::
::/=  style
::  /^  octs
::  /;  as-octs:mimes:html
::  /|  /:  /===/app/eth-wallet/css/index  /css/
::      /~  ~
::  ==
::::
::/=  tile-js
::  /^  octs
::  /;  as-octs:mimes:html
::  /|  /:  /===/app/eth-wallet/js/tile  /js/
::      /~  ~
::  ==
::
=*  card  card:agent:gall
=*  eth  ethereum-types
=*  eth-rpc  rpc:ethereum
=>
|%
+$  state-0
  $:  %0
      key-path=path
      node-url=_'http://eth-mainnet.urbit.org:8545'
      balances=(map contract-id [=address:eth balance=@ud])
      pending=(map contract-id (map txn txn-raw))
  ==
::
+$  poke
  $%  [%send-erc20 =contract-id to=address:eth amount=@ud]
      [%add-erc20 =contract-id =address:eth]
      [%set-key key-path=path]
  ==
::  TODO: fill in
::
+$  txn  ~
+$  txn-raw  ~
+$  contract-id  @t
--
::
=|  state-0
=*  state  -
=<
%+  verb  |
^-  agent:gall
|_  bol=bowl:gall
+*  this  .
    do    ~(. +> bol)
    def   ~(. (default-agent this %|) bol)
::
++  on-init  on-init:def
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ::
  ?+    mark  (on-poke:def mark vase)
      %handle-http-request
    ?>  (team:title our.bol src.bol)
    =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
    :_  this
    %+  give-simple-payload:app  eyre-id
    %+  require-authorization:app  inbound-request
    |=  =inbound-request:eyre
    ^-  simple-payload:http
    =+  url=(parse-request-line url.request.inbound-request)
    ::?+  site.url  not-found:gen
    ::  [%'~eth-wallet' %css %index ~]  (css-response:gen style)
    ::  [%'~eth-wallet' %js %tile ~]    (js-response:gen tile-js)
    ::  [%'~eth-wallet' %js %index ~]   (js-response:gen script)
    ::  [%'~eth-wallet' *]  (html-response:gen index)
    ::==
    !!
  ::
      %noun
    =+  !<(=poke vase)
    ?-    -.poke
        %send-erc20
      =/  [contract=address:eth balance=@ud]
        (~(got by balances) contract-id.poke)
      ?>  (gte balance amount.poke)
      ::  TODO figure out key and value for .pending
      ::
      :_  this
      (start-txn-send:do contract [contract-id to amount]:poke)
    ::
        %add-erc20
      !!
    ::
        %set-key
      =/  =path  (en-beam:format byk.bol(q %home) `path`key-path.poke)
      ?>  ?=(^ =<(fil .^(arch %cy path)))
      `this(key-path key-path.poke)
    ==
  ==
::  +on-agent: handle response from another agent
::
::    Spider logic mostly duplicated with :eth-sender.
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?.  ?=([%xfer *] wire)
    (on-agent:def wire sign)
  ?-  -.sign
      %poke-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"{(trip dap.bol)} couldn't start thread" u.p.sign)
    :_  this
    [(leave-spider:do wire)]~
  ::
      %watch-ack
    ?~  p.sign
      [~ this]
    =/  =tank  leaf+"{(trip dap.bol)} couldn't start listen to thread"
    %-  (slog tank u.p.sign)
    [~ this]
  ::
      %kick
    [~ this]
  ::
      %fact
    ?+  p.cage.sign  (on-agent:def wire sign)
        %thread-fail
      =+  !<([=term =tang] q.cage.sign)
      %-  (slog leaf+"{(trip dap.bol)} failed" leaf+<term> tang)
      [~ this]
    ::
        %thread-done
      ::  TODO: update state with successful txn; needs id
      ::
      ~&  ['transaction submitted to' t.wire]
      [~ this]
    ==
  ==
::
++  on-save   !>(state)
++  on-load   on-load:def
++  on-watch  on-watch:def
++  on-arvo   on-arvo:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
::
|_  bol=bowl:gall
++  start-txn-send
  |=  [contract=address:eth contract-id=@t to=address:eth amount=@ud]
  ^-  (list card)
  =/  tid=@ta
    :((cury cat 3) dap.bol '--' contract-id '--' (scot %uv eny.bol))
  =/  private-key  .^(@ %cx byk.bol(q %home) key-path)
  =/  args
    :^  ~  `tid  %eth-erc20-transfer
    !>([node-url contract-id contract to amount private-key])
  :~  (watch-spider /xfer/[tid] /thread-result/[tid])
      (poke-spider /xfer/[tid] %spider-start !>(args))
  ==
::
++  poke-spider
  |=  [=path =cage]
  ^-  card
  [%pass path %agent [our.bol %spider] %poke cage]
::
++  watch-spider
  |=  [=path =sub=path]
  ^-  card
  [%pass path %agent [our.bol %spider] %watch sub-path]
::
++  leave-spider
  |=  =path
  ^-  card
  [%pass path %agent [our.bol %spider] %leave ~]
--
