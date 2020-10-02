::  btc-node-hook: send JSON rpc requests to bitcoin full node
::  and poke the responses into the btc-node-store
::
/-  *btc-node-hook, *btc-node-store
/+  default-agent, base64, lib=btc-node-json, verb, dbug
::
=>  |%
    +$  card  card:agent:gall
    +$  versioned-state
      $%  [%0 state-zero]
      ==
    ::
    +$  state-zero
      $:  user=@t
          pass=@t
          endpoint=@t
          watched-calls=(set term)
      ==
    --
::
=|  state-zero
=*  state  -
::  Main
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<  |_  =bowl:gall
    +*  this      .
        btc-core  +>
        bc        ~(. btc-core bowl)
        def       ~(. (default-agent this %|) bowl)
    ::
    ++  on-init
      ^-  (quip card _this)
      [~ this(user '', pass '', endpoint '', watched-calls *(set term))]
    ::
    ++  on-save   !>(state)
    ++  on-load
      |=  old=vase
      `this(state !<(state-zero old))
    ::
    ++  on-poke
      |=  [=mark =vase]
      ^-  (quip card _this)
      ?>  (team:title our.bowl src.bowl)
      =^  cards  state
        ?+    mark    (on-poke:def mark vase)
            %btc-node-hook-action
          (handle-action:bc !<(btc-node-hook-action vase))
        ::
            %btc-node-hook-command
          (handle-command:bc !<(btc-node-hook-command vase))
        ==
      [cards this]
    ::
    ++  on-watch
      |=  pax=path
      ^-  (quip card _this)
      ::  We restrict access to the local ship and its moons,
      ::  because we handle permissioning at higher-level agents.
      ::
      ?>  (team:title our.bowl src.bowl)
      ?+  pax  (on-watch:def pax)
          [%response ~]
        ~&  >  "%btc-node-hook: subscription on {pax}"
        `this
      ==
    ++  on-leave  on-leave:def
    ++  on-peek   on-peek:def
    ++  on-agent  on-agent:def
    ++  on-arvo
      |=  [=wire =sign-arvo]
      ^-  (quip card _this)
      =*  response  client-response.sign-arvo
      =^  cards  state
        ?+    +<.sign-arvo    (on-arvo:def wire sign-arvo)
            %http-response    (http-response:bc wire response)
        ==
      [cards this]
    ::
    ++  on-fail   on-fail:def
    --
::
|_  =bowl:gall
::
++  handle-action
  |=  act=btc-node-hook-action
  ^-  (quip card _state)
  =/  body=request:rpc:jstd
    (request-to-rpc:btc-rpc:lib act)
  =/  =header-list:http
    :~  ['Content-Type' 'application/json']
        :-  'Authorization'
        ;:  (cury cat 3)
            'Basic '
            %-  ~(en base64 | &)
            (as-octs:mimes:html :((cury cat 3) user ':' pass))
    ==  ==
  =/  req=request:http
    :*  %'POST'
        (endpoint-url act)
        header-list
        =,  html
        %-  some
          %-  as-octt:mimes
            (en-json (request-to-json:rpc:jstd body))
    ==
  =/  out  *outbound-config:iris
  :_  state
  [%pass /[(scot %da now.bowl)] %arvo %i %request req out]~
::
++  handle-command
  |=  comm=btc-node-hook-command
  ^-  (quip card _state)
  ?+    -.comm  ~|  [%unsupported-hook-command -.comm]  !!
      %credentials
    :_  state(endpoint url.comm, user user.comm, pass pass.comm)
    [%pass / %arvo %d %flog [%text "credentials updated..."]]~
  ==
::
++  httr-to-rpc-response
  |=  hit=httr:eyre
  ^-  response:rpc:jstd
  ~|  hit
  =/  jon=json  (need (de-json:html q:(need r.hit)))
  ?.  =(%2 (div p.hit 100))
    (parse-error jon)
  =,  dejs-soft:format
  ^-  response:rpc:jstd
  =;  dere
    =+  res=((ar dere) jon)
    ?~  res  (need (dere jon))
    [%batch u.res]
  |=  jon=json
  ^-  (unit response:rpc:jstd)
  =/  res=[id=(unit @t) res=(unit json) err=(unit json)]
    %.  jon
    =,  dejs:format
    =-  (ou -)
    :~  ['id' (uf ~ (mu so))]
        ['result' (uf ~ (mu same))]
        ['error' (uf ~ (mu same))]
    ==
  ?:  ?=([^ * ~] res)
    `[%result [u.id.res ?~(res.res ~ u.res.res)]]
  ~|  jon
  `(parse-error jon)
::
++  parse-error
  |=  =json
  ^-  response:rpc:jstd
  :-  %error
  ?~  json  ['' '' '']
  %.  json
  =,  dejs:format
  =-  (ou -)
  :~  =-  ['id' (uf '' (cu - (mu so)))]
      |*(a=(unit) ?~(a '' u.a))
      :-  'error'
      =-  (uf ['' ''] -)
      =-  (cu |*(a=(unit) ?~(a ['' ''] u.a)) (mu (ou -)))
      :~  ['code' (uf '' no)]
          ['message' (uf '' so)]
  ==  ==
::
++  http-response
  |=  [=wire response=client-response:iris]
  ^-  (quip card _state)
  ?.  ?=(%finished -.response)
    [~ state]
  =*  status    status-code.response-header.response
  =/  rpc-resp=response:rpc:jstd
    %-  httr-to-rpc-response
    %+  to-httr:iris
      response-header.response
    full-file.response
  ?.  ?=([%result *] rpc-resp)
    ~&  [%error +.rpc-resp]
    [~ state]
  %-  handle-btc-response
  (parse-response:btc-rpc:lib rpc-resp)
::
++  handle-btc-response
  |=  btc-resp=btc-node-hook-response
  ^-  (quip card _state)
  :_  state
  ::  If the head term is a type of call we are watching, then
  ::  broadcast it to subscribers on the response path
  ::
  =/  broadcast-response=(list card)
    ?:  (~(has in watched-calls) -.btc-resp)
      ~[[%give %fact ~[/response] %noun !>(3)]]
    ~
  %+  weld
    broadcast-response
  ^-  (list card)
  ?+    -.btc-resp
      ::  By default we just print all RPC responses that are not
      ::  considered here explicitly for proper format printing or
      ::  for being passed on to the store app.
      ::
      ~&(btc-resp ~)
  ::
      :: %abandon-transaction
      :: %abort-rescan
      :: %add-multisig-address
      :: %backup-wallet
      :: %bump-fee
      %create-wallet
    =/  btc-store-req=btc-node-store-action
      :+  %add-wallet   name.btc-resp
      ?:(=('' warning.btc-resp) ~ (some warning.btc-resp))
    [(btc-node-store-poke /store btc-store-req)]~
  ::
      :: %dump-privkey
      :: %dump-wallet
      :: %encrypt-wallet
      :: %get-addresses-by-label
  ::
      %get-address-info
    ~&([%address-info +.btc-resp] ~)
  ::
      %get-balance
    ~&([%amount (trip +.btc-resp)] ~)
  ::
      :: %get-balance
      :: %get-new-address
      :: %get-raw-change-address
      :: %get-received-by-address
      :: %get-received-by-label
      :: %get-transaction
      :: %get-unconfirmed-balance
  ::
      %get-wallet-info
    ^-  (list card)
    =/  btc-store-req=btc-node-store-action
      [%update-wallet wallet-name.btc-resp +>:btc-resp]
    [(btc-node-store-poke /update btc-store-req)]~
  ::
      :: %import-address
      :: %import-multi
      :: %import-privkey
      :: %import-pruned-funds
      :: %import-pubkey
      :: %import-wallet
      :: %key-pool-refill
      :: %list-address-groupings
      :: %list-labels
      :: %list-lock-unspent
      :: %list-received-by-address
      :: %list-received-by-label
      :: %lists-in-ceblock
  ::
      %list-transactions
    ~&([%transactions +.btc-resp] ~)
  ::
      :: %list-unspent
      :: %list-wallet-dir
  ::
      %list-wallets
    ^-  (list card)
    :~  (btc-node-store-poke /list-wallets [%list-wallets ~])
        :*  %pass  /  %arvo  %d  %flog
            %text  "remote-wallets={<`wain`wallets.btc-resp>}"
    ==  ==
  ::
      %load-wallet
    [(btc-node-store-poke /load [%load-wallet name.btc-resp])]~
  ::
      :: %lock-unspent
      :: %remove-pruned-funds
      :: %rescan-blockchain
      :: %send-many
      :: %send-to-address
      :: %set-hd-seed
      :: %set-label
      :: %set-tx-fee
      :: %sign-message
      :: %sign-raw-transaction-with-wallet
      :: %unload-wallet
      :: %wallet-create-fundedpsbt
      :: %wallet-lock
      :: %wallet-passphrase
      :: %wallet-passphrase-change
      :: %wallet-process-psbt
  ==
::
++  btc-node-store-poke
  |=  [=wire act=btc-node-store-action]
  ^-  card
  :*  %pass
      wire
      %agent
      [our.bowl %btc-node-store]
      %poke
      [%btc-node-store-action !>(act)]
  ==
::
++  default-wallet
  .^  @t
      %gx
      (scot %p our.bowl)
      %btc-node-store
      (scot %da now.bowl)
      /default-wallet/noun
  ==
::
++  n-wallets
  .^  @ud
    %gx
    (scot %p our.bowl)
    %btc-node-store
    (scot %da now.bowl)
    /n-wallets/noun
  ==
::
++  endpoint-url
  |=  [act=btc-node-hook-action]
  ^-  @t
  ?.  ?|  ?=(%abandon-transaction -.act)
          ?=(%abort-rescan -.act)
          ?=(%add-multisig-address -.act)
          ?=(%backup-wallet -.act)
          ?=(%bump-fee -.act)
          ?=(%dump-privkey -.act)
          ?=(%dump-wallet -.act)
          ?=(%encrypt-wallet -.act)
          ?=(%fund-raw-transaction -.act)
          ?=(%get-balance -.act)
          ?=(%get-balances -.act)
          ?=(%get-addresses-by-label -.act)
          ?=(%get-address-info -.act)
          ?=(%get-new-address -.act)
          ?=(%get-raw-change-address -.act)
          ?=(%get-received-by-address -.act)
          ?=(%get-received-by-label -.act)
          ?=(%get-transaction -.act)
          ?=(%get-unconfirmed-balance -.act)
          ?=(%get-wallet-info -.act)
          ?=(%import-address -.act)
          ?=(%import-multi -.act)
          ?=(%import-privkey -.act)
          ?=(%import-pruned-funds -.act)
          ?=(%import-pubkey -.act)
          ?=(%import-wallet -.act)
          ?=(%key-pool-refill -.act)
          ?=(%list-address-groupings -.act)
          ?=(%list-labels -.act)
          ?=(%list-lock-unspent -.act)
          ?=(%list-received-by-address -.act)
          ?=(%list-received-by-label -.act)
          ?=(%lists-in-ceblock -.act)
          ?=(%list-transactions -.act)
          ?=(%list-unspent -.act)
          ?=(%lock-unspent -.act)
          ?=(%remove-pruned-funds -.act)
          ?=(%rescan-blockchain -.act)
          ?=(%send-many -.act)
          ?=(%send-to-address -.act)
          ?=(%set-hd-seed -.act)
          ?=(%set-label -.act)
          ?=(%set-tx-fee -.act)
          ?=(%sign-message -.act)
          ?=(%sign-raw-transaction-with-wallet -.act)
          ?=(%wallet-create-fundedpsbt -.act)
          ?=(%wallet-lock -.act)
          ?=(%wallet-passphrase -.act)
          ?=(%wallet-passphrase-change -.act)
          ?=(%wallet-process-psbt -.act)
      ==
    endpoint
  ;:  (cury cat 3)
      endpoint
      'wallet/'
    ::
      ?:  ?=([?(%dump-wallet %import-wallet) filename=@t] act)
        filename.act
      default-wallet
  ==
--
