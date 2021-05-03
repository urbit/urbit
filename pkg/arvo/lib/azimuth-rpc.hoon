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
        =,  dejs:format
        %-  some
        %.  u.data
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
        =,  dejs:format
        =;  [add=(unit @ux) =ship]
          ?~  add  ~
          (some [u.add ship])
        %.  u.data
        %-  ot 
        :~  ['address' (cu to-hex so)]
            ['ship' (su ;~(pfix sig fed:ag))]
        ==
      ::
      ++  address
        |=  params=(map @t json)
        ^-  (unit @ux)
        ?~  data=(~(get by params) 'data')  ~
        =,  dejs:format
        %.  u.data
        (ot ['address' (cu to-hex so)]~)
      ::
      ++  sig
        |=  params=(map @t json)
        ^-  (unit @)
        ?~  sig=(~(get by params) 'sig')  ~
        (some (so:dejs:format u.sig))
      ::
      ++  from
        |=  params=(map @t json)
        ^-  (unit [ship @t])
        ?~  from=(~(get by params) 'from')  ~
        %-  some
        =,  dejs:format
        %.  u.from 
        %-  ot
        :~  ['ship' (su ;~(pfix sig fed:ag))]
            ['proxy' so]
        == 
      --
    ::
    ++  point-to-json
      |=  =point:naive
      ^-  json
      |^
      :-  %o
      %-  molt
      ^-  (list [@t json])
      :~  ['dominion' s+dominion.point]
        ::
          :-  'ownership' 
          :-  %o
          %-  molt
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
          :-  %o
          =,  enjs:format
          %-  molt  ^-  (list [@t json])
          =*  net  net.point
          :*  ['rift' (numb rift.net)]
            ::
              :-  'keys'
              :-  %o
              %-  molt  ^-  (list [@t json])
              :~  ['life' (numb life.keys.net)]
                  ['suite' (numb suite.keys.net)]
                  ['auth' (numb auth.keys.net)]
                  ['crypt' (numb crypt.keys.net)]
              ==
            ::
              ['rift' (numb rift.net)]
              :-  'sponsor'
              :-  %o
              %-  molt  ^-  (list [@t json])
              ~[['has' b+has.sponsor.net] ['who' (ship who.sponsor.net)]]
            ::
              ?~  escape.net  ~
              ['escape' (ship u.escape.net)]~
      ==  ==
      :: 
      ++  own-to-json
        |=  [=address:naive =nonce:naive]
        ^-  json
        :-  %o
        %-  molt  ^-  (list [@t json])
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
      ?.  =((lent ~(tap by params)) 3)
        [~ [%error id 'X' 'Params missing']]
      ?>  =((lent ~(tap by params)) 3)
      =/  sig=(unit @)            (sig:extract params)
      =/  from=(unit [ship @t])   (from:extract params)
      =/  data=(unit [@ux ship])  (address-ship:extract params)
      ?.  &(?=(^ sig) ?=(^ from) ?=(^ data))

        [~ [%error id 'X' 'Failed to parsed']]
      :_  [%result id s+'ok']
      %-  some 
      noun+!>([u.sig u.from u.data])
    ::
    ++  proxy-call
      |=  [id=@t params=(map @t json)]
      ^-  [(unit cage) response:rpc]
      ?.  =((lent ~(tap by params)) 3)
        [~ [%error id 'X' 'Params missing']]
      =/  sig=(unit @)           (sig:extract params)
      =/  from=(unit [ship @t])  (from:extract params)
      =/  data=(unit @ux)        (address:extract params)
      ?.  &(?=(^ sig) ?=(^ from) ?=(^ data))
        [~ [%error id 'X' 'Failed to parsed']]
      :_  [%result id s+'ok']
      %-  some 
      noun+!>([u.sig u.from u.data])
    --
|%
::  TODO: move error handling to separate core
::
++  get-point
  |=  [id=@t params=(map @t json) scry=$-(ship (unit point:naive))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    [%error id 'X' 'Params missing']
  ?~  ship=(~(get by params) 'ship')
    [%error id 'X' 'A "ship" key must exist']
  ?~  ship=(rush (so:dejs:format u.ship) ;~(pfix sig fed:ag))
    [%error id 'X' 'Ship @p invalid']
  ?~  point=(scry u.ship)
    [%error id 'X' 'Ship @p not found']
  [%result id (point-to-json u.point)]
::
++  transfer-point
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  =((lent ~(tap by params)) 3)
    [~ [%error id 'X' 'Params missing']]
  =/  sig=(unit @)            (sig:extract params)
  =/  from=(unit [ship @t])   (from:extract params)
  =/  data=(unit [@ux ?])
    ?~  data=(~(get by params) 'data')  ~
    =,  dejs:format
    =;  [add=(unit @ux) r=?]
      ?~  add  ~
      (some [u.add r])
    %.  u.data
    %-  ot 
    ~[['address' (cu to-hex so)] ['reset' bo]]
  ?.  &(?=(^ sig) ?=(^ from) ?=(^ data))
    [~ [%error id 'X' 'Failed to parsed']]
  :_  [%result id s+'ok']
  %-  some 
  noun+!>([u.sig u.from u.data])
::
++  configure-keys
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  =((lent ~(tap by params)) 3)
    [~ [%error id 'X' 'Params missing']]
  =/  sig=(unit @)            (sig:extract params)
  =/  from=(unit [ship @t])   (from:extract params)
  =/  data=(unit [encrypt=@ auth=@ crypto-suite=@ breach=?])
    (keys:extract params)
  ?.  &(?=(^ sig) ?=(^ from) ?=(^ data))
    [~ [%error id 'X' 'Failed to parsed']]
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
