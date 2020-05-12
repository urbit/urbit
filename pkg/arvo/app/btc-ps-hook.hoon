/-  *btc-ps-hook, asn1
/+  bip32, bip39, *server, der, default-agent, dbug
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
      %generate-private-key
    [~ state(entropy `byts`[64 eny.bol])]
  ==
::
++  poke-btc-ps-action
  |=  act=btc-ps-action
  ^-  (quip card _state)
  ?-  -.act
      %get-rates
    [[(get-rates currency-pair.act store-id 'Hn37sEESNBedGt5ZKrVKHKLyQvCztLAA7WRRfJcTZ3UB')]~ state]
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
        [%'pairingCode' s+pairing-code]
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
  (http-request /events/(scot %da now.bol) request *outbound-config:iris)
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
  =/  msg-sha=@uvI  (sha-256:sha (swp 3 msg))
  ~&  `@ux`private-key:bip32-core
  =/  signed-msg
    (ecdsa-raw-sign:secp256k1:secp msg-sha private-key:bip32-core)
  =/  enc=[len=@ud dat=@ux]
    %-  en:der
    ^-  spec:asn1
    :-  %seq
    :~  `spec:asn1`[%int `@u`(swp 3 r.signed-msg)]
        `spec:asn1`[%int `@u`(swp 3 s.signed-msg)]
    ==

  =/  pub=@t
    %-  crip
    :-  '0'
    %+  slag  2
    (scow %x public-key:bip32-core)

  =/  dat=@t
    %-  crip
    %-  flop
    %+  rip  4
    %-  crip
    %+  slag  2
    (scow %x dat.enc)
  =.  dat
    '3046022100ab3457e281a6609db6ea0c5a05ff17567d3c976e3dc9b48c732d27b8fdc6982a022100aa5f05a65161463beaf712ea1bf9c4a18291dff72bb26a2a263dac81e9ee0bc8'
  :~
    ::  derive public key from private-key
    ['X-Identity' pub]
    ::  sign msg with private-key
    ['X-Signature' dat]
  ==
::
:: ++  der-encode
::   |=  [r=@ s=@]
::   ^-  @
::   |^
::   =/  r-list=(list @)  (flop (rip 3 r))
::   =/  s-list=(list @)  (flop (rip 3 s))
::   ?>  ?=(^ r-list)
::   ?>  ?=(^ s-list)
::   =:  r-list
::     ?:  (gte i.r-list 128)
::       [0 r-list]
::     r-list
::   ::
::       s-list
::     ?:  (gte i.s-list 128)
::       [0 s-list]
::     s-list
::   ::
::   ==
::
::   =/  rr-list=(list @)  (rm-padding r-list)
::   =/  ss-list=(list @)  (rm-padding s-list)
::   =.  rr-list  (construct-length rr-list)
::   0
::   :: =/  back-half=(list @)  ~[2]
::   :: =.  back-half  (snoc `(list @)`back-half (lent r-list))
::   :: =.  back-half  (weld back-half r-list)
::   :: =.  back-half  (snoc `(list @)`back-half 2)
::   :: =.  back-half  (snoc `(list @)`back-half (lent s-list))
::   :: =.  back-half  (weld back-half s-list)
::   :: =/  res=(list @)  ~[48 (lent back-half)]
::   :: =.  res  (weld res back-half)
::
::   :: (rep 3 )
::   ++  rm-padding
::     |=  lis=(list @)
::     ^-  (list @)
::     =/  length=@  (lent lis)
::     ?>  (gte length 2)
::     =/  i=@  1
::     |-
::     ?:  ?&  =((snag i lis) 0)
::             (gte (snag +(i) lis) 128)
::             (lth i length)
::         ==
::       $(i +(i))
::     (slag (dec i) lis)
::   ::
::   ++  construct-length
::     |=  lis=(list @)
::     ^-  (list @)
::     ~&  `@ux`(met 3 11.111.111)
::     :: ~&  `@ux`(rsh 3 1 (xeb 700))
::     =/  octs  +((rsh 3 1 (xeb 700)))
::     ?:  (lth (lent lis) 128)
::       (snoc lis (lent lis))
::     =/  octs  (met 3 700)
::     =.  lis  (snoc lis (con 700 128))
::     [1]~
::   --
::
++  signed-get-request
  |=  [endpoint=@t params=(list [@t @t])]
  ^-  request:http
  =/  hed=header-list:http
    :~  ['Content-Type' 'application/json']
        ['Accept' 'application/json']
        ['User-Agent' 'node-btcpay']
        ['X-Accept-Version' '2.0.0']
        ['connection' 'close']
    ==
  =/  base-url  "http://127.0.0.1:23000/"
  =/  qs  (stringify params)
  =/  url
    (crip (weld (weld base-url (trip endpoint)) (trip qs)))
  =/  signed-hed  (weld hed (create-signed-headers url))
  ~&  url
  [%'GET' url signed-hed *(unit octs)]
::
++  post-request
  |=  [endpoint=@t body-octs=octs]
  ^-  request:http
  =/  hed=header-list:http
    :~  ['content-type' 'application/json']
        ['accept' 'application/json']
        ['X-Accept-Version' '2.0.0']
        ['User-Agent' 'urbit-btcpay']
    ==
  =/  base-url  "http://127.0.0.1:3000/" :: base url need to be in state
  =/  url  (crip (weld base-url (trip endpoint)))
  ~&  body-octs
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
