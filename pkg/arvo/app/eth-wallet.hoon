/+  default-agent, verb, eth-abi, *server
::
/=  erc20-abi
  /;  parse-contract:eth-abi
  /:  /===/app/eth-wallet/erc20  /json/
::
=*  card  card:agent:gall
=*  eth  ethereum-types
=>
|%
+$  state-0
  $:  %0
      key-path=path
      balances=(map contract-id [=address:eth balance=@ud])
      pending=(map contract-id transaction)
  ==
::
+$  transaction
  $:  amount=@ud
      ~  ::  TODO
  ==
::
+$  command
  $%  [%send-erc20 =contract-id to=address:eth amount=@ud]
      [%add-erc20 =contract-id =address:eth]
      [%set-key key-path=path]
  ==
::
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
    :_  this  ::  TODO mutate state
    %+  give-simple-payload:app  eyre-id
    %+  require-authorization:app  inbound-request
    |=  =inbound-request:eyre
    ^-  simple-payload:http
    !!
  ::
      %noun
    =+  !<(=command vase)
    ?-    -.command
        %send-erc20
      !!
    ::
        %add-erc20
      !!
    ::
        %set-key
      =/  =path  (en-beam:format byk.bol(q %home) `path`key-path.command)
      ?>  ?=(^ =<(fil .^(arch %cy path)))
      `this(key-path key-path.command)
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
