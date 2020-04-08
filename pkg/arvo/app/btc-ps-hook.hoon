/-  *btc-ps-hook
/+  bip32, bip39, *server, default-agent, dbug
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero  [%0 base-state]
+$  base-state
  $:  entropy=byts
      store-id=@t
      token=@t
  ==
::
--
=|  state-zero
=*  state  -
%-  agent:dbug
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this       .
      api-core  +>
      ac         ~(. api-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    [~ this]
  ++  on-save   !>(state)
  ++  on-load
    |=  old-vase=vase
    ^-  (quip card _this)
    [~ this(state !<(state-zero old-vase))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          %btc-ps-admin-action  (poke-btc-ps-admin-action:ac !<(btc-ps-admin-action vase))
          %btc-ps-action        (poke-btc-ps-action:ac !<(btc-ps-action vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path  (on-watch:def path)
        [%http-response *]
      [~ this]
        [%events @ ~]
      [~ this]
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    (on-agent:def wire sign)
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  +<.sign-arvo  (on-arvo:def wire sign-arvo)
        %http-response
      =^  cards  state
        (http-response:ac wire client-response.sign-arvo)
      [cards this]
    ==
  ::
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
++  poke-btc-ps-admin-action
  |=  act=btc-ps-admin-action
  ^-  (quip card _state)
  ?>  (team:title our.bol src.bol)
  ?-  -.act
      %set-store-id
    [~ state(store-id store-id.act)]
      %pair-client
    [[(pair-client pairing-code.act)]~ state]
      %get-mnemonic
    :_  state
    [%give %fact [/primary]~ %btc-ps-update !>([%mnemonic mnemonic])]~
    ::   %generate-private-key
    :: [~ state(entropy `byts`[64 eny.bol])]
  ==
::
++  poke-btc-ps-action
  |=  act=btc-ps-action
  ^-  (quip card _state)
  ?-  -.act
      %get-rates
    [[(get-rates currency-pair.act store-id '4AEuwNXZt9jNbPYFf9W8Nh8mjVVQME9kQV7rD45RXfsp')]~ state]
  ==
::
++  http-response
  |=  [=wire response=client-response:iris]
  ~&  response
  ^-  (quip card _state)
  ::  ignore all but %finished
  ?.  ?=(%finished -.response)
    [~ state]
  ?<  (gth 200 status-code.response-header.response)
  =/  data=mime-data:iris  (need full-file.response)
  [~ state]
::  =/  =json  (need (de-json:html q.data.data))
::  ~&  json
::  =/  =broadcast-result  (parse-broadcast-result json)
::  ~&  broadcast-result+broadcast-result
::  :_  state
::  [%give %fact ~[wire] %broadcast-result !>(broadcast-result)]~
::
:: ++  parse-broadcast-result
::   =,  dejs:format
::   %-  ot:dejs:format
::   :~  [%success bo]
::       [%error (mu so)]
::       [%result (mu (ot [%txid so]~))]
::   ==
::
::  +utilities
::
++  bip32-core
  ~+  ^+  bip32
  (from-seed:bip32 entropy)
::
++  mnemonic  (crip (from-entropy:bip39 entropy))
::
++  pair-client
  |=  pairing-code=@t
  =/  =request:http
    %+  post-request  'tokens'
    %-  json-to-octs
    %-  pairs:enjs:format
    :~  [%id s+identity:bip32]
        [%pairingCode s+pairing-code]
    ==
  (http-request /token/(scot %da now.bol) request *outbound-config:iris)
::
++  get-rates
  |=  [currency-pair=@t store-id=@t token=@t]
  ^-  card
  =/  =request:http
    %+  signed-get-request  'rates'
    :~  ['currencyPairs' currency-pair]
        ['storeID' store-id]
        ['token' token]
    ==
  (http-request /events/(scot %p src.bol) request *outbound-config:iris)
::
++  http-request
  |=  [=wire =request:http =outbound-config:iris]
  ^-  card
  [%pass wire %arvo %i %request request outbound-config]
::
++  create-signed-headers
  |=  msg=@t
  ^-  (list [@t @t])
  =,  crypto

  =/  msg-sha=@uvI  (hmac-sha256t:hmac (met 3 msg) msg)
  =/  signed-msg
    (ecdsa-raw-sign:secp256k1:secp msg-sha private-key:bip32-core)
  ::   return type: [v=@ r=@ s=@]
  ~&  signed-msg
  :~
    ::  derive public key from private-key
    ['X-Identity' public-key:bip32-core]
    ::  sign msg with private-key
    ['X-Signature' msg]
  ==
::
++  signed-get-request
  |=  [endpoint=@t params=(list [@t @t])]
  ^-  request:http
  =/  hed=header-list:http
    :~  ['accept' 'application/json']
        ['content-type' 'application/json']
        ['User-Agent' 'urbit-btcpay']
        ['X-Accept-Version' '2.0.0']
    ==
  =/  base-url  "https://btcpay464279.lndyn.com/"
  =/  qs  (stringify params)
  =/  url
    (crip (weld (weld base-url (trip endpoint)) (trip qs)))
  =/  signed-hed  (weld hed (create-signed-headers url))
  ~&  signed-hed
  [%'GET' url hed *(unit octs)]
::
++  post-request
  |=  [endpoint=@t body-octs=octs]
  ^-  request:http
  =/  hed=header-list:http
    :~  ['accept' 'application/json']
        ['content-type' 'application/json']
        ['User-Agent' 'urbit-btcpay']
        ['X-Accept-Version' '2.0.0']
    ==
  =/  base-url  "https://btcpay464279.lndyn.com/" :: base url need to be in state
  =/  url  (crip (weld base-url (trip endpoint)))
  [%'POST' url hed [~ body-octs]]
::
++  stringify
  |=  params=(list [@t @t])
  ^-  @t
  =/  query-string=tape  ""
  |-
  ^-  @t
  ?~  params  (crip query-string)
  =.  query-string
    %-  zing
    :~  query-string
        ?:(=(query-string "") "?" "&")
        (trip -.i.params)
        "="
        (trip +.i.params)
    ==
  $(params t.params)
--
