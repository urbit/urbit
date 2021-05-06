::  azimuth-rpc: command parsing and utilities
::
/-  rpc=json-rpc
/+  naive
::
=>  ::  Utilities
    ::
    |%
    ++  extract
      |%
      ++  keys
        |=  params=(map @t json)
        ^-  (unit [encrypt=@ auth=@ crypto-suite=@ breach=?])
        ?~  data=(~(get by params) 'data')  ~
        %.  u.data
        =,  dejs-soft:format
        %-  ot 
        :~  ['encrypt' so]
            ['auth' so]
            ['crypto-suite' so]
            ['breach' bo]
        ==
      ::
      ++  address-transfer
        |=  params=(map @t json)
        ^-  (unit [@ux ?])
        ?~  data=(~(get by params) 'data')  ~
        =;  ans=(unit [add=(unit @ux) r=?])
          ?~  ans  ~
          ?~  add.u.ans  ~
          (some [u.add.u.ans r.u.ans])
        %.  u.data
        =,  dejs-soft:format
        %-  ot 
        ~[['address' (cu to-hex so)] ['reset' bo]]
      ::
      ++  address-ship
        |=  params=(map @t json)
        ^-  (unit [@ux @p])
        ?~  data=(~(get by params) 'data')  ~
        =;  ans=(unit [add=(unit @ux) ship=@p])
          ?~  ans  ~
          ?~  add.u.ans  ~
          (some [u.add.u.ans ship.u.ans])
        %.  u.data
        =,  dejs-soft:format
        %-  ot 
        :~  ['address' (cu to-hex so)]
            ['ship' (su ;~(pfix sig fed:ag))]
        ==
      ::
      ++  address
        |=  params=(map @t json)
        ^-  (unit @ux)
        ?~  data=(~(get by params) 'data')  ~
        =;  ans=(unit (unit @ux))
          ?~  ans  ~
          ?~  u.ans  ~
          (some u.u.ans)
        =,  dejs-soft:format
        %.  u.data
        (ot ['address' (cu to-hex so)]~)
      ::    
      ++  ship
        |=  params=(map @t json)
        ^-  (unit @p)
        ?~  data=(~(get by params) 'data')  ~
        =,  dejs-soft:format
        %.  u.data
        (ot ['ship' (su ;~(pfix sig fed:ag))]~)
      ::          
      ++  sig
        |=  params=(map @t json)
        ^-  (unit @)
        =,  dejs-soft:format
        ?~  sig=(~(get by params) 'sig')  ~
        (so:dejs-soft:format u.sig)
      ::
      ++  from
        |=  params=(map @t json)
        ^-  (unit [@p proxy:naive])
        ?~  from=(~(get by params) 'from')  ~
        =,  dejs-soft:format
        %.  u.from 
        %-  ot
        :~  ['ship' (su ;~(pfix sig fed:ag))]
            ['proxy' (cu proxy:naive so)]
        == 
      --
    ::
    ++  pending-to-json
      |=  pending=(list tx:naive)
      ^-  json
      =,  enjs:format
      :-  %a
      %+  turn  pending
      |=  =tx:naive
      ^-  json
      |^
      =,  enjs:format
      %-  pairs
      :~  ['tx' (parse-tx +.tx)]
        ::
          :-  'from'
          %-  pairs
          ~[['ship' (ship ship.from.tx)] ['proxy' s+proxy.from.tx]]
      ==
      ::
      ++  parse-tx
        |=  tx=skim-tx:naive
        ^-  json
        %-  pairs
        :~  ['type' s+-.tx]
          ::
            :-  'data'
            %-  pairs
            ?-  -.tx
              %transfer-point        (en-transfer +.tx)
              %spawn                 (en-spawn +.tx)
              %configure-keys        (en-keys +.tx)
              %escape                ~[(en-ship parent.tx)]
              %cancel-escape         ~[(en-ship parent.tx)]
              %adopt                 ~[(en-ship ship.tx)]
              %reject                ~[(en-ship ship.tx)]
              %detach                ~[(en-ship ship.tx)]
              %set-management-proxy  ~[(en-address address.tx)]
              %set-spawn-proxy       ~[(en-address address.tx)]
              %set-transfer-proxy    ~[(en-address address.tx)]
        ==  ==
      ::
      ++  en-ship      |=(s=@p ship+(ship s))
      ++  en-address   |=(a=@ux address+s+(crip "0x{((x-co:co 20) a)}"))
      ++  en-spawn     |=([s=@p a=@ux] ~[(en-ship s) (en-address a)])
      ++  en-transfer  |=([a=@ux r=?] ~[(en-address a) reset+b+r])
      ++  en-keys      
        |=  [encrypt=@ auth=@ crypto-suite=@ breach=?]
        ^-  (list [@t json])
        :~  ['encrypt' (numb encrypt)]
            ['auth' (numb auth)]
            ['crypto-suite' (numb crypto-suite)]
            ['breach' b+breach]
        ==
      --
    ::
    ++  point-to-json
      |=  =point:naive
      ^-  json
      =,  enjs:format
      |^
      %-  pairs
      :~  ['dominion' s+dominion.point]
        ::
          :-  'ownership' 
          %-  pairs
          =*  own  own.point
          ^-  (list [@t json])
          :~  ['owner' (own-to-json owner.own)]
              ['spawnProxy' (own-to-json spawn-proxy.own)]
              ['managementProxy' (own-to-json management-proxy.own)]
              ['votingProxy' (own-to-json voting-proxy.own)]
              ['transferProxy' (own-to-json transfer-proxy.own)]
          ==
        ::
          :-  'network'
          %-  pairs
          =*  net  net.point
          :*  ['rift' (numb rift.net)]
            ::
              :-  'keys'
              %-  pairs
              :~  ['life' (numb life.keys.net)]
                  ['suite' (numb suite.keys.net)]
                  ['auth' (numb auth.keys.net)]
                  ['crypt' (numb crypt.keys.net)]
              ==
            ::
              ['rift' (numb rift.net)]
              :-  'sponsor'
              %-  pairs
              ~[['has' b+has.sponsor.net] ['who' (ship who.sponsor.net)]]
            ::
              ?~  escape.net  ~
              ['escape' (ship u.escape.net)]~
      ==  ==
      :: 
      ++  own-to-json
        |=  [=address:naive =nonce:naive]
        ^-  json
        %-  pairs
        :~  ['address' s+(crip "0x{((x-co:co 20) address)}")]
            ['nonce' (numb:enjs:format nonce)]
        ==
      --
    ::
    ++  txs-to-json
      ::  TODO: implement me!
      ::
      |=  txs=(list [tx:naive success=?])
      ^-  json
      ~
    ::
    ++  to-hex
      |=  =cord
      ^-  (unit @ux)
      =/  parsed=(unit (pair @ud @ux))  (de:base16:mimes:html cord)
      ?~  parsed  
        ::~|(%non-hex-cord !!)
        ~
      (some q.u.parsed)
    ::
    ++  sponsor-call
      |=  [id=@t params=(map @t json)]
      ^-  [(unit cage) response:rpc]
      ?.  =((lent ~(tap by params)) 3)  
        [~ ~(params error id)]
      =/  sig=(unit @)                  (sig:extract params)
      =/  from=(unit [@p proxy:naive])  (from:extract params)
      =/  data=(unit [@ux @p])          (address-ship:extract params)
      ?.  &(?=(^ sig) ?=(^ from) ?=(^ data))  
        [~ ~(parse error id)]
      :_  [%result id s+'ok']
      %-  some 
      noun+!>([u.sig u.from u.data])
    ::
    ++  proxy-call
      |=  [id=@t params=(map @t json)]
      ^-  [(unit cage) response:rpc]
      ?.  =((lent ~(tap by params)) 3)  
        [~ ~(params error id)]
      =/  sig=(unit @)                  (sig:extract params)
      =/  from=(unit [@p proxy:naive])  (from:extract params)
      =/  data=(unit @ux)               (address:extract params)
      ?.  &(?=(^ sig) ?=(^ from) ?=(^ data))  
        [~ ~(parse error id)]
      :_  [%result id s+'ok']
      %-  some 
      noun+!>([u.sig u.from u.data])
    ::
    ++  error
      |_  id=@t
      ::  https://www.jsonrpc.org/specification#error_object
      ::
      ++  parse      [%error id '-32700' 'Failed to parsed']
      ++  request    [%error id '-32600' 'Invalid Request']
      ++  method     [%error id '-32601' 'Method not found']
      ++  params     [%error id '-32602' 'Invalid params']
      ++  internal   [%error id '-32603' 'Internal error']
      ++  not-found  [%error id '-32000' 'Resource not found']
      --
    --
|%
++  get-point
  |=  [id=@t params=(map @t json) scry=$-(ship (unit point:naive))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)    
    ~(params error id)
  ?~  ship=(~(get by params) 'ship')  
    ~(params error id)
  ?~  ship=(rush (so:dejs:format u.ship) ;~(pfix sig fed:ag))
    ~(params error id)
  ?~  point=(scry u.ship)  
    ~(params error id)
  [%result id (point-to-json u.point)]
::
++  transfer-point
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  =((lent ~(tap by params)) 3)  
    [~ ~(params error id)]
  =/  sig=(unit @)           (sig:extract params)
  =/  from=(unit [ship @t])  (from:extract params)
  =/  data=(unit [@ux ?])    (address-transfer:extract params)
  ?:  |(?=(~ sig) ?=(~ from) ?=(~ data))  
    [~ ~(parse error id)]
  :_  [%result id s+'ok']
  %-  some 
  noun+!>([u.sig u.from u.data])
::
++  configure-keys
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  =((lent ~(tap by params)) 3)  
    [~ ~(params error id)]
  =/  sig=(unit @)            (sig:extract params)
  =/  from=(unit [ship @t])   (from:extract params)
  =/  data=(unit [encrypt=@ auth=@ crypto-suite=@ breach=?])
    (keys:extract params)
  ?.  &(?=(^ sig) ?=(^ from) ?=(^ data))  
    [~ ~(parse error id)]
  :_  [%result id s+'ok']
  %-  some 
  noun+!>([u.sig u.from u.data])
::
++  spawn             sponsor-call
++  escape            sponsor-call
++  cancel-escape     sponsor-call
++  adopt             sponsor-call
++  detach            sponsor-call
++  reject            sponsor-call
++  management-proxy  proxy-call
++  spawn-proxy       proxy-call
++  transfer-proxy    proxy-call
:: - readNonce(from=[ship proxy]) -> @  :: automatically increment for pending wraps
::
++  read-nonce
  |=  [id=@t params=(map @t json) scry=$-([ship proxy:naive] (unit @))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 3)  
    ~(params error id)
  ?~  from=(from:extract params)
    ~(parse error id)
  ?~  nonce=(scry u.from)  
    ~(params error id)
  [%result id (numb:enjs:format u.nonce)]
::
++  pending
  ::  FIXME: send raw-tx (i.e. tx with signature) instead?
  ::
  |%
  :: - readPendingRoll() -> (list tx)
  ::
  ++  all
    |=  [id=@t params=(map @t json) pending=(list tx:naive)]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 0)  
      ~(params error id)
    [%result id (pending-to-json pending)]
  ::
  :: - readPendingByShip(ship) -> (list tx)
  ::
  ++  ship
    |=  [id=@t params=(map @t json) scry=$-(@p (list tx:naive))]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 1)  
      ~(params error id)
    ?~  ship=(ship:extract params)
      ~(parse error id)
    [%result id (pending-to-json (scry u.ship))]
  ::
  :: - readPendingByAddress(address) -> (list tx)
  ::
  ++  addr
    |=  [id=@t params=(map @t json) scry=$-(@ux (list tx:naive))]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 1)  
      ~(params error id)
    ?~  address=(address:extract params)
      ~(parse error id)
    [%result id (pending-to-json (scry u.address))]
  --
::
++  history
  |=  $:  id=@t 
          params=(map @t json) 
          ::  FIXME: use proper type from aggregator/index
          ::
          scry=$-([@p proxy:naive] (list [tx:naive success=?]))
      ==
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)  
    ~(params error id)
  ?~  from=(from:extract params)
    ~(parse error id)
  [%result id (txs-to-json (scry u.from))]
--
