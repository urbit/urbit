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
::
+$  txn  _!!
+$  txn-raw  @ux
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
      =/  [=address:eth balance=@ud]  (~(got by balances) contract-id.poke)
      ?>  (gte balance amount.poke)
      ::  TODO: switch to spider?
      ::  TODO: use real transaction types
      ::
      =|  =txn
      =|  =txn-raw
      =.  pending  (~(put by pending) contract-id.poke [txn txn-raw]^~^~)
      :_  this
      =/  =cage  [%noun !>([%send txn-raw])]
      [%pass /send %agent [our.bol %eth-sender] %poke cage]~
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
++  on-watch
  |=  =path
  ^-  (quip card _this)
  !!
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  !!
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  !!
::
++  on-save   !>(state)
++  on-load   on-load:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
::
|_  bol=bowl:gall
++  read-key
  !!
--
