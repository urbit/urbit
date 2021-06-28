::  azimuth-rpc: command parsing and utilities
::
/-  rpc=json-rpc, *aggregator
/+  naive, json-rpc, lib=naive-transactions
::
=>  ::  Utilities
    ::
    |%
    +$  spawn-action
      $?  %escape
          %cancel-escape
          %adopt
          %reject
          %detach
      ==
    ::
    +$  proxy-action
      $?  %set-management-proxy
          %set-spawn-proxy
          %set-transfer-proxy
      ==
    ::
    ++  parse-ship
      |=  jon=json
      ^-  (unit @p)
      ?:  ?=([%n *] jon)
        `(rash p.jon dem)
      ?.  ?=([%s *] jon)  ~
      `(rash p.jon ;~(pfix sig fed:ag))
    ::
    ++  from-json
      =,  dejs-soft:format
      |%
      ++  keys
        |=  params=(map @t json)
        ^-  (unit [encrypt=@ auth=@ crypto-suite=@ breach=?])
        ?~  data=(~(get by params) 'data')  ~
        %.  u.data
        %-  ot
        :~  ['encrypt' so]
            ['auth' so]
            ['crypto-suite' so]
            ['breach' bo]
        ==
      ::
      ++  data
        |%
        ++  address-transfer
          |=  params=(map @t json)
          ^-  (unit [@ux ?])
          ?~  data=(~(get by params) 'data')  ~
          =;  ans=(unit [add=(unit @ux) r=?])
            ?~  ans  ~
            ?~  add.u.ans  ~
            (some [u.add.u.ans r.u.ans])
          %.  u.data
          %-  ot
          ~[['address' (cu to-hex so)] ['reset' bo]]
        ::
        ++  address-ship
          |=  params=(map @t json)
          ^-  (unit [@p @ux])
          ?~  data=(~(get by params) 'data')  ~
          =;  ans=(unit [ship=@p add=(unit @ux)])
            ?~  ans    ~
            ?~  add.u.ans  ~
            (some [ship.u.ans u.add.u.ans])
          %.  u.data
          %-  ot
          :~  ['ship' parse-ship]
              ['address' (cu to-hex so)]
          ==
        ::
        ++  address
          |=  params=(map @t json)
          ^-  (unit @ux)
          ?~  data=(~(get by params) 'data')  ~
          =;  ans=(unit (unit @ux))
            ?~(ans ~ u.ans)
          %.  u.data
          (ot ['address' (cu to-hex so)]~)
        ::
        ++  ship
          |=  params=(map @t json)
          ^-  (unit @p)
          ?~  data=(~(get by params) 'data')  ~
          %.  u.data
          (ot ['ship' parse-ship]~)
        --
      ::
      ++  ship
        |=  params=(map @t json)
        ^-  (unit @p)
        ?~  data=(~(get by params) 'ship')  ~
        (parse-ship u.data)
      ::
      ++  address
        |=  params=(map @t json)
        ^-  (unit @ux)
        ?~  data=(~(get by params) 'address')  ~
        ?~  ans=((cu to-hex so) u.data)  ~
        u.ans
      ::
      ++  sig
        |=  params=(map @t json)
        ^-  (unit @)
        ?~  sig=(~(get by params) 'sig')   ~
        ?~  ans=((cu to-hex so) u.sig)  ~
        u.ans
      ::
      ++  from
        |=  params=(map @t json)
        ^-  (unit [@p proxy:naive])
        ?~  from=(~(get by params) 'from')  ~
        %.  u.from
        %-  ot
        :~  ['ship' parse-ship]
            ['proxy' (cu proxy:naive so)]
        ==
      ::
      ++  hash
        |=  params=(map @t json)
        ^-  (unit @ux)
        ?~  hash=(~(get by params) 'hash')  ~
        ?~  ans=((cu to-hex so) u.hash)  ~
        u.ans
      ::
      ++  raw
        |=  params=(map @t json)
        ^-  (unit octs)
        ?~  raw=(~(get by params) 'raw')  ~
        ?~  ans=((cu to-hex so) u.raw)  ~
        ?~  u.ans  ~
        (some (as-octs:mimes:html u.u.ans))
      ::
      ++  l2-tx
        |=  params=(map @t json)
        ^-  (unit ^l2-tx)
        ?~  type=(~(get by params) 'type')  ~
        %.  u.type
        (cu ^l2-tx so)
      --
    ::
    ++  to-json
      =,  enjs:format
      |%
      ++  pending
        |=  pending=(list pend-tx)
        ^-  json
        :-  %a
        %+  turn  pending
        |=  pend-tx
        ^-  json
        %-  pairs
        :~  ['force' b+force]
          ::
            :-  'raw-tx'
            %-  pairs
            :~  ['sig' (numb sig.raw-tx)]
                ['tx' (tx:to-json tx.raw-tx)]
        ==  ==
      ::
      ++  tx
        |=  =tx:naive
        ^-  json
        |^
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
      ++  txs
        |=  txs=(list tx:naive)
        ^-  json
        a+(turn txs |=(=tx:naive (tx:to-json tx)))
      ::
      ++  roller-txs
        |=  txs=(list roller-tx)
        ^-  json
        :-  %a
        %+  turn  txs
        |=  roller-tx
        ^-  json
        %-  pairs
        :: [status=tx-status hash=keccak type=l2-tx]
        :~  ['status' s+status.status]
            ['hash' s+(crip "0x{((x-co:co 20) hash)}")]
            ['type' s+type]
        ==
      ::
      ++  point
        |=  =point:naive
        ^-  json
        %-  pairs
        :~  ['dominion' s+dominion.point]
          ::
            :-  'ownership'
            %-  pairs
            =*  own  own.point
            ^-  (list [@t json])
            :~  ['owner' (ownership owner.own)]
                ['spawnProxy' (ownership spawn-proxy.own)]
                ['managementProxy' (ownership management-proxy.own)]
                ['votingProxy' (ownership voting-proxy.own)]
                ['transferProxy' (ownership transfer-proxy.own)]
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
      ++  points
        |=  points=(list [@p point:naive])
        ^-  json
        :-  %a
        %+  turn  points
        |=  [ship=@p =point:naive]
        %-  pairs
        :~  ['ship' (^ship ship)]
            ['point' (^point point)]
        ==
      ::
      ++  ownership
        |=  [=address:naive =nonce:naive]
        ^-  json
        %-  pairs
        :~  ['address' s+(crip "0x{((x-co:co 20) address)}")]
            ['nonce' (numb nonce)]
        ==
      ::
      ++  spawned
        |=  children=(list [@p @ux])
        ^-  json
        :-  %a
        %+  turn  children
        |=  [child=@p addr=@ux]
        %-  pairs
        :~  ['ship' (ship child)]
            ['address' s+(crip "0x{((x-co:co 20) addr)}")]
        ==
      ::
      ++  tx-status
        |=  =^tx-status
        ^-  json
        %-  pairs
        :~  ['status' s+status.tx-status]
          ::
            :-  'pointer'
            ?~  pointer.tx-status  ~
            =*  pointer  u.pointer.tx-status
            (ownership address.pointer nonce.pointer)
        ==
      --
    ::
    ++  to-hex
      |=  =cord
      ^-  (unit @ux)
      (rush (rsh [3 2] cord) hex)
    ::
    ++  rpc-res
      |%
      ++  sponsor
        |=  [id=@t params=(map @t json) action=spawn-action]
        ^-  [(unit cage) response:rpc]
        ?.  (params:validate params)
          [~ ~(params error:json-rpc id)]
        =/  sig=(unit @)                  (sig:from-json params)
        =/  from=(unit [@p proxy:naive])  (from:from-json params)
        =/  raw=(unit octs)               (raw:from-json params)
        =/  data=(unit @p)                (ship:data:from-json params)
        ?.  &(?=(^ sig) ?=(^ from) ?=(^ raw) ?=(^ data))
          [~ ~(parse error:json-rpc id)]
        :_  [%result id s+'ok']
        %-  some
        :-  %aggregator-action
        !>
        =;  =skim-tx:naive
          [%submit | u.sig %ful u.raw u.from skim-tx]
        ?-  action
          %escape         [%escape u.data]
          %cancel-escape  [%cancel-escape u.data]
          %adopt          [%adopt u.data]
          %reject         [%reject u.data]
          %detach         [%detach u.data]
        ==
      ::
      ++  proxy
        |=  [id=@t params=(map @t json) action=proxy-action]
        ^-  [(unit cage) response:rpc]
        ?.  (params:validate params)
          [~ ~(params error:json-rpc id)]
        =/  sig=(unit @)                  (sig:from-json params)
        =/  from=(unit [@p proxy:naive])  (from:from-json params)
        =/  raw=(unit octs)               (raw:from-json params)
        =/  data=(unit @ux)               (address:data:from-json params)
        ?.  &(?=(^ sig) ?=(^ from) ?=(^ raw) ?=(^ data))
          [~ ~(parse error:json-rpc id)]
        :_  [%result id s+'ok']
        %-  some
        :-  %aggregator-action
        !>
        =;  =skim-tx:naive
          [%submit | u.sig %ful u.raw u.from skim-tx]
        ?-  action
          %set-management-proxy  [%set-management-proxy u.data]
          %set-spawn-proxy       [%set-spawn-proxy u.data]
          %set-transfer-proxy    [%set-transfer-proxy u.data]
        ==
      --
    ::
    ++  validate
      |%
      ++  params
        |=  params=(map @t json)
        ^-  ?
        =((lent ~(tap by params)) 4)
      --
    --
|%
++  get-point
  |=  [id=@t params=(map @t json) scry=$-(ship (unit point:naive))]
  ^-  response:rpc
  ?.  =(~(wyt by params) 1)
    ~(params error:json-rpc id)
  ?~  ship=(~(get by params) 'ship')
    ~(params error:json-rpc id)
  ?~  ship=(parse-ship u.ship)
    ~(params error:json-rpc id)
  ?~  point=(scry u.ship)
    ~(not-found error:json-rpc id)
  [%result id (point:to-json u.point)]
::
++  get-points
  |=  [id=@t params=(map @t json) scry=$-(@ux (list [@p point:naive]))]
  ^-  response:rpc
  ~&  ~(wyt by params)
  ?.  =(~(wyt by params) 1)
    ~(params error:json-rpc id)
  ?~  address=(address:from-json params)
    ~(parse error:json-rpc id)
  [%result id (points:to-json (scry u.address))]
::
++  transfer-point
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  (params:validate params)
    [~ ~(params error:json-rpc id)]
  =/  sig=(unit @)                    (sig:from-json params)
  =/  from=(unit [ship proxy:naive])  (from:from-json params)
  =/  raw=(unit octs)                 (raw:from-json params)
  =/  data=(unit [@ux ?])             (address-transfer:data:from-json params)
  ?:  |(?=(~ sig) ?=(~ from) ?=(~ raw) ?=(~ data))
    [~ ~(parse error:json-rpc id)]
  :_  [%result id s+'ok']
::
++  cancel-tx
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  =(~(wyt by params) 3)
    [~ ~(params error:json-rpc id)]
  =/  sig=(unit @)       (sig:from-json params)
  =/  keccak=(unit @ux)  (hash:from-json params)
  =/  l2=(unit l2-tx)    (l2-tx:from-json params)
  ?.  &(?=(^ sig) ?=(^ keccak) ?=(^ l2))
    [~ ~(parse error:json-rpc id)]
  :_  [%result id s+'ok']
  %-  some
  :-  %aggregator-action
  !>([%cancel u.sig u.keccak u.l2])
::
++  get-spawned
  |=  [id=@t params=(map @t json) scry=$-(ship (list [ship @ux]))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(~(get by params) 'ship')
    ~(params error:json-rpc id)
  ?~  ship=(parse-ship u.ship)
    ~(params error:json-rpc id)
  [%result id (spawned:to-json (scry u.ship))]
::
++  configure-keys
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  (params:validate params)
    [~ ~(params error:json-rpc id)]
  =/  sig=(unit @)                    (sig:from-json params)
  =/  from=(unit [ship proxy:naive])  (from:from-json params)
  =/  raw=(unit octs)                 (raw:from-json params)
  =/  data=(unit [encrypt=@ auth=@ crypto-suite=@ breach=?])
    (keys:data:from-json params)
  ?.  &(?=(^ sig) ?=(^ from) ?=(^ raw) ?=(^ data))
    [~ ~(parse error:json-rpc id)]
  :_  [%result id s+'ok']
  %-  some
  :-  %aggregator-action
  !>([%submit | u.sig %ful u.raw u.from %configure-keys u.data])
::
++  spawn
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  (params:validate params)
    [~ ~(params error:json-rpc id)]
  =/  sig=(unit @)                  (sig:from-json params)
  =/  from=(unit [@p proxy:naive])  (from:from-json params)
  =/  raw=(unit octs)               (raw:from-json params)
  =/  data=(unit [@p @ux])          (address-ship:data:from-json params)
  ?.  &(?=(^ sig) ?=(^ from) ?=(^ raw) ?=(^ data))
    [~ ~(parse error:json-rpc id)]
  :_  [%result id s+'ok']
  %-  some
  :-  %aggregator-action
  !>([%submit | u.sig %ful u.raw u.from %spawn u.data])
::
++  escape            sponsor:rpc-res
++  cancel-escape     sponsor:rpc-res
++  adopt             sponsor:rpc-res
++  detach            sponsor:rpc-res
++  reject            sponsor:rpc-res
++  management-proxy  proxy:rpc-res
++  spawn-proxy       proxy:rpc-res
++  transfer-proxy    proxy:rpc-res
::
++  nonce
  |=  [id=@t params=(map @t json) scry=$-([ship proxy:naive] (unit @))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  from=(from:from-json params)
    ~(parse error:json-rpc id)
  ?~  nonce=(scry u.from)
    ~(not-found error:json-rpc id)
  [%result id (numb:enjs:format u.nonce)]
::
++  pending
  |%
  ::
  ++  all
    |=  [id=@t params=(map @t json) pending=(list pend-tx)]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 0)
      ~(params error:json-rpc id)
    [%result id (pending:to-json pending)]
  ::
  ++  ship
    |=  [id=@t params=(map @t json) scry=$-(@p (list pend-tx))]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 1)
      ~(params error:json-rpc id)
    ?~  ship=(ship:from-json params)
      ~(parse error:json-rpc id)
    [%result id (pending:to-json (scry u.ship))]
  ::
  ++  addr
    |=  [id=@t params=(map @t json) scry=$-(@ux (list pend-tx))]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 1)
      ~(params error:json-rpc id)
    ?~  address=(address:from-json params)
      ~(parse error:json-rpc id)
    [%result id (pending:to-json (scry u.address))]
  --
::
++  status
  |=  [id=@t params=(map @t json) scry=$-(@ tx-status)]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  hash=(hash:from-json params)
    ~(parse error:json-rpc id)
  [%result id (tx-status:to-json (scry u.hash))]
::
++  next-batch
  |=  [id=@t params=(map @t json) when=time]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 0)
    ~(params error:json-rpc id)
  [%result id (time:enjs:format when)]
::
++  history
  |=  [id=@t params=(map @t json) scry=$-(address:naive (list roller-tx))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  address=(address:from-json params)
    ~(parse error:json-rpc id)
  [%result id (roller-txs:to-json (scry u.address))]
--
