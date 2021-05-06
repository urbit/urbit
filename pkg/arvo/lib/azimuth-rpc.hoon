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
      ++  address-ship
        |=  params=(map @t json)
        ^-  (unit [@ux ship])
        ?~  data=(~(get by params) 'data')  ~
        =;  ans=(unit [add=(unit @ux) =ship])
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
      ++  sig
        |=  params=(map @t json)
        ^-  (unit @)
        =,  dejs-soft:format
        ?~  sig=(~(get by params) 'sig')  ~
        (so:dejs-soft:format u.sig)
      ::
      ++  from
        |=  params=(map @t json)
        ^-  (unit [ship proxy:naive])
        ?~  from=(~(get by params) 'from')  ~
        =,  dejs-soft:format
        %.  u.from 
        %-  ot
        :~  ['ship' (su ;~(pfix sig fed:ag))]
            ['proxy' (cu proxy:naive so)]
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
      =/  sig=(unit @)            (sig:extract params)
      =/  from=(unit [ship @t])   (from:extract params)
      =/  data=(unit [@ux ship])  (address-ship:extract params)
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
      =/  sig=(unit @)           (sig:extract params)
      =/  from=(unit [ship @t])  (from:extract params)
      =/  data=(unit @ux)        (address:extract params)
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
      ++  parse     [%error id '-32700' 'Failed to parsed']
      ++  request   [%error id '-32600' 'Invalid Request']
      ++  method    [%error id '-32601' 'Method not found']
      ++  params    [%error id '-32602' 'Invalid params']
      ++  internal  [%error id '-32603' 'Internal error']
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
  =/  data=(unit [@ux ?])
    ?~  data=(~(get by params) 'data')  ~
    =;  [add=(unit @ux) r=?]
      ?~  add  ~
      (some [u.add r])
    %.  u.data
    =,  dejs:format
    %-  ot 
    ~[['address' (cu to-hex so)] ['reset' bo]]
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
--
