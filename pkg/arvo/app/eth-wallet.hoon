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
      pending=[next=@ud txns=(map wire txn)]
  ==
::
+$  poke
  $%  [%send-erc20 txn]
      [%add-erc20 =contract-id =address:eth]
      [%set-key key-path=path]
  ==
+$  txn  [=contract-id to=address:eth amount=@ud]
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
      =/  =txn  +.poke
      =/  [contract=address:eth balance=@ud]
        (~(got by balances) contract-id.poke)
      ::  TODO: also assert balance after subtracting pending
      ::
      ?>  (gte balance amount.poke)
      ::
      =/  =wire  /xfer/(scot %ud next.pending)/[contract-id.poke]
      =.  txns.pending  (~(put by txns.pending) wire txn)
      =.  next.pending  +(next.pending)
      ::
      :_  this
      (start-txn-send:do wire contract txn)
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
    =.  txns.pending  (~(del by txns.pending) wire)
    :_  this
    [(leave-spider:do wire)]~
  ::
      %watch-ack
    ?~  p.sign
      [~ this]
    ::  TODO: this should probably retry the listen poke
    ::
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
      =/  =txn  (~(got by txns.pending) wire)
      =.  txns.pending  (~(del by txns.pending) wire)
      =+  !<([=term =tang] q.cage.sign)
      %-  =-  (slog (welp - tang))
          :~  leaf+"eth-wallet: transaction failed"
              leaf+<txn>
              leaf+<term>
          ==
      [~ this]
    ::
        %thread-done
      ?>  ?=([%xfer @ @ ~] wire)
      =/  =contract-id  i.t.t.wire
      =/  =txn  (~(got by txns.pending) wire)
      =.  txns.pending  (~(del by txns.pending) wire)
      =.  balances
        %+  ~(jab by balances)  contract-id
        |=  [=address:eth balance=@ud]
        [address (sub balance amount.txn)]
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
  |=  [=wire contract=address:eth txn]
  ^-  (list card)
  =/  tid=@ta
    :((cury cat 3) dap.bol '--' contract-id '--' (scot %uw (mug wire)))
  =/  private-key  .^(@ %cx byk.bol(q %home) key-path)
  =/  args
    :^  ~  `tid  %eth-erc20-transfer
    !>([node-url contract-id contract to amount private-key])
  :~  (watch-spider wire /thread-result/[tid])
      (poke-spider wire %spider-start !>(args))
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
