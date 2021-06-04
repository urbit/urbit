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
          :~  ['ship' (su ;~(pfix ^sig fed:ag))]
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
          (ot ['ship' (su ;~(pfix ^sig fed:ag))]~)
        --
      ::
      ++  ship
        |=  params=(map @t json)
        ^-  (unit @p)
        ?~  data=(~(get by params) 'ship')  ~
        %.  u.data
        (su ;~(pfix ^sig fed:ag))
      ::
      ++  address
        |=  params=(map @t json)
        ^-  (unit @ux)
        ?~  data=(~(get by params) 'address')  ~
        =;  ans=(unit (unit @ux))
          ?~(ans ~ u.ans)
        ((cu to-hex so) u.data)
      ::
      ++  sig
        |=  params=(map @t json)
        ^-  (unit @ux)
        ?~  sig=(~(get by params) 'sig')   ~
        =;  ans=(unit (unit @ux))
          ?~(ans ~ u.ans)
        %.  u.sig
        (cu to-hex so)
      ::
      ++  from
        |=  params=(map @t json)
        ^-  (unit [@p proxy:naive])
        ?~  from=(~(get by params) 'from')  ~
        %.  u.from
        %-  ot
        :~  ['ship' (su ;~(pfix ^sig fed:ag))]
            ['proxy' (cu proxy:naive so)]
        ==
      ::
      ++  keccak
        |=  params=(map @t json)
        ^-  (unit @ux)
        ?~  keccak=(~(get by params) 'keccak')  ~
        =;  ans=(unit (unit @ux))
          ?~(ans ~ u.ans)
        ((cu to-hex so) u.keccak)
      ::
      ++  raw
        |=  params=(map @t json)
        ^-  (unit octs)
        ?~  raw=(~(get by params) 'raw')  ~
        =;  ans=(unit (unit @ux))
          ?~  ans  ~
          ?~  u.ans  ~
          (some (as-octs:mimes:html u.u.ans))
        ((cu to-hex so) u.raw)
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
      =/  parsed=(unit (pair @ud @ux))  (de:base16:mimes:html cord)
      ?~  parsed
        ::~|(%non-hex-cord !!)
        ~
      (some q.u.parsed)
    ::
    ++  rpc-res
      |%
      ++  sponsor
        |=  [id=@t params=(map @t json) action=spawn-action]
        ^-  [(unit cage) response:rpc]
        ?.  (params:validate params)
          [~ ~(params error:json-rpc id)]
        =/  sig=(unit @ux)                (sig:from-json params)
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
        =/  sig=(unit @ux)                (sig:from-json params)
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
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(~(get by params) 'ship')
    ~(params error:json-rpc id)
  ?~  ship=(rush (so:dejs:format u.ship) ;~(pfix sig fed:ag))
    ~(params error:json-rpc id)
  ?~  point=(scry u.ship)
    ~(params error:json-rpc id)
  [%result id (point:to-json u.point)]
::
++  get-spawned
  |=  [id=@t params=(map @t json) scry=$-(ship (list [ship @ux]))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(~(get by params) 'ship')
    ~(params error:json-rpc id)
  ?~  ship=(rush (so:dejs:format u.ship) ;~(pfix sig fed:ag))
    ~(params error:json-rpc id)
  [%result id (spawned:to-json (scry u.ship))]
::
++  transfer-point
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  (params:validate params)
    [~ ~(params error:json-rpc id)]
  =/  sig=(unit @ux)         (sig:from-json params)
  =/  from=(unit [ship @t])  (from:from-json params)
  =/  raw=(unit octs)        (raw:from-json params)
  =/  data=(unit [@ux ?])    (address-transfer:data:from-json params)
  ?:  |(?=(~ sig) ?=(~ from) ?=(~ raw) ?=(~ data))
    [~ ~(parse error:json-rpc id)]
  :_  [%result id s+'ok']
  %-  some
  :-  %aggregator-action
  !>([%submit | u.sig %ful u.raw u.from %transfer-point u.data])
::
++  configure-keys
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  (params:validate params)
    [~ ~(params error:json-rpc id)]
  =/  sig=(unit @ux)          (sig:from-json params)
  =/  from=(unit [ship @t])   (from:from-json params)
  =/  raw=(unit octs)         (raw:from-json params)
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
  =/  sig=(unit @ux)                (sig:from-json params)
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
:: - readNonce(from=[ship proxy]) -> @  :: automatically increment for pending wraps
::
++  read-nonce
  |=  [id=@t params=(map @t json) scry=$-([ship proxy:naive] (unit @))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 3)
    ~(params error:json-rpc id)
  ?~  from=(from:from-json params)
    ~(parse error:json-rpc id)
  ?~  nonce=(scry u.from)
    ~(params error:json-rpc id)
  [%result id (numb:enjs:format u.nonce)]
::
++  pending
  ::  FIXME: send raw-tx (i.e. tx with signature) instead?
  ::
  |%
  :: - readPendingRoll() -> (list pend-tx)
  ::
  ++  all
    |=  [id=@t params=(map @t json) pending=(list pend-tx)]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 0)
      ~(params error:json-rpc id)
    [%result id (pending:to-json pending)]
  :: - readPendingByShip(ship) -> (list pend-tx)
  ::
  ++  ship
    |=  [id=@t params=(map @t json) scry=$-(@p (list pend-tx))]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 1)
      ~(params error:json-rpc id)
    ?~  ship=(ship:from-json params)
      ~(parse error:json-rpc id)
    [%result id (pending:to-json (scry u.ship))]
  :: - readPendingByAddress(address) -> (list pend-tx)
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
  ?~  keccak=(keccak:from-json params)
    ~(parse error:json-rpc id)
  [%result id (tx-status:to-json (scry u.keccak))]
::
:: ++  history
::   |=  $:  id=@t
::           params=(map @t json)
::           ::  FIXME: use proper type from aggregator/index
::           ::
::           scry=$-([@p proxy:naive] (list tx:naive))
::       ==
::   ^-  response:rpc
::   ?.  =((lent ~(tap by params)) 1)
::     ~(params error:json-rpc id)
::   ?~  from=(from:from-json params)
::     ~(parse error:json-rpc id)
::   [%result id (txs:to-json (scry u.from))]
--
