::  azimuth-rpc: command parsing and utilities
::
/-  rpc=json-rpc, *dice
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
        (rush p.jon dem)
      ?.  ?=([%s *] jon)  ~
      (rush p.jon ;~(pfix sig fed:ag))
    ::  TODO: from /lib/group-store (move to zuse?)
    ++  enkebab
      |=  str=cord
      ^-  @tas
      ~|  str
      =-  (fall - str)
      %+  rush  str
      =/  name
        %+  cook
          |=  part=tape
          ^-  tape
          ?~  part  part
          :-  (add i.part 32)
          t.part
        ;~(plug hig (star low))
      %+  cook
        |=(a=(list tape) (crip (zing (join "-" a))))
      ;~(plug (star low) (star name))
    ::
    ++  from-json
      =,  dejs-soft:format
      |%
      ++  data
        |%
        ++  keys
          |=  params=(map @t json)
          ^-  (unit [encrypt=@ auth=@ crypto-suite=@ breach=?])
          ?~  data=(~(get by params) 'data')  ~
          =;  ans=(unit [cryp=(unit @ux) auth=(unit @ux) suit=@ brec=?])
            ?~  ans  ~
            ?:  |(?=(~ cryp.u.ans) ?=(~ auth.u.ans))  ~
            (some [u.cryp.u.ans u.auth.u.ans suit.u.ans brec.u.ans])
          %.  u.data
          %-  ot
          :~  ['encrypt' (cu to-hex so)]
              ['auth' (cu to-hex so)]
              ['cryptoSuite' no]
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
        ::
        ++  cancel
          |=  params=(map @t json)
          ^-  (unit [l2-tx @p])
          ?~  data=(~(get by params) 'data')  ~
          %.  u.data
          %-  ot
          :~  ['type' (cu l2-tx so)]
              ['ship' parse-ship]
          ==
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
      ++  tx
        |=  params=(map @t json)
        ^-  (unit l2-tx)
        ?~  data=(~(get by params) 'tx')  ~
        ?~  tx=(so u.data)  ~
        =/  method=@tas  (enkebab u.tx)
        ?.  ?=(l2-tx method)  ~
        `method
      ::
      ++  nonce
        |=  params=(map @t json)
        ^-  (unit @ud)
        ?~  nonce=(~(get by params) 'nonce')  ~
        (ni u.nonce)
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
            (en-address address)
          ::
            :-  'rawTx'
            %-  pairs
            :~  ['tx' (tx:to-json tx.raw-tx)]
                ['sig' (hex (as-octs:mimes:html sig.raw-tx))]
        ==  ==
      ::
      ++  en-address   |=(a=@ux address+(hex 20 a))
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
        ++  en-spawn     |=([s=@p a=@ux] ~[(en-ship s) (en-address a)])
        ++  en-transfer  |=([a=@ux r=?] ~[(en-address a) reset+b+r])
        ++  en-keys
          |=  [encrypt=@ auth=@ crypto-suite=@ breach=?]
          ^-  (list [@t json])
          :~  ['encrypt' (numb encrypt)]
              ['auth' (numb auth)]
              ['cryptoSuite' (numb crypto-suite)]
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
        :~  ['status' s+status]
            ['hash' (hex (as-octs:mimes:html hash))]
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
                =,  mimes:html
                :-  'keys'
                %-  pairs
                :~  ['life' (numb life.keys.net)]
                    ['suite' (numb suite.keys.net)]
                    ['auth' (hex (as-octs auth.keys.net))]
                    ['crypt' (hex (as-octs crypt.keys.net))]
                ==
              ::
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
      ++  ships
        |=  ships=(list @p)
        ^-  json
        a+(turn ships ship)
      ::
      ++  ownership
        |=  [=address:naive =nonce:naive]
        ^-  json
        %-  pairs
        :~  (en-address address)
            ['nonce' (numb nonce)]
        ==
      ::
      ++  spawned
        |=  children=(list [@p @ux])
        ^-  json
        :-  %a
        %+  turn  children
        |=  [child=@p address=@ux]
        %-  pairs
        :~  ['ship' (ship child)]
            (en-address address)
        ==
      ::
      ++  tx-status  |=(=^tx-status ^-(json s+status.tx-status))
      ::
      ++  config
        |=  roller-config
        ^-  json
        %-  pairs
        :~  ['nextBatch' (time next-batch)]
            ['frequency' (numb (div frequency ~s1))]
            ['refreshTime' (numb (div refresh-time ~s1))]
            ['contract' (hex 20 contract)]
            ['chainId' (numb chain-id)]
        ==
      ::
      ++  hex
        |=  [p=@ q=@]
        ^-  json
        s+(crip ['0' 'x' ((x-co:co (mul 2 p)) q)])
      --
    ::
    ++  to-hex
      |=  =cord
      ^-  (unit @ux)
      ?.  =((end [3 2] cord) '0x')  ~
      (rush (rsh [3 2] cord) hex)
    ::
    ++  build-l2-tx
      |=  [=l2-tx from=[@p proxy:naive] params=(map @t json)]
      ^-  (unit tx:naive)
      ?:  =(l2-tx %transfer-point)
        ?~  data=(address-transfer:data:from-json params)
          ~
        `[from %transfer-point u.data]
      ?:  =(l2-tx %spawn)
        ?~  data=(address-ship:data:from-json params)
          ~
        `[from %spawn u.data]
      ?:  =(l2-tx %configure-keys)
        ?~  data=(keys:data:from-json params)
          ~
        `[from %configure-keys u.data]
      ?:  ?=(spawn-action l2-tx)
        ?~  data=(ship:data:from-json params)
          ~
        ?-  l2-tx
          %escape         `[from %escape u.data]
          %cancel-escape  `[from %cancel-escape u.data]
          %adopt          `[from %adopt u.data]
          %reject         `[from %reject u.data]
          %detach         `[from %detach u.data]
        ==
      ?.  ?=(proxy-action l2-tx)
        ~
      ?~  data=(address:data:from-json params)
        ~
      ?-  l2-tx
        %set-management-proxy  `[from %set-management-proxy u.data]
        %set-spawn-proxy       `[from %set-spawn-proxy u.data]
        %set-transfer-proxy    `[from %set-transfer-proxy u.data]
      ==
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
++  get-ships
  |=  [id=@t params=(map @t json) scry=$-(@ux (list @p))]
  ^-  response:rpc
  ?.  =(~(wyt by params) 1)
    ~(params error:json-rpc id)
  ?~  address=(address:from-json params)
    ~(parse error:json-rpc id)
  [%result id (ships:to-json (scry u.address))]
::
++  get-dns
  |=  [id=@t params=(map @t json) dns=(list @t)]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 0)
    ~(params error:json-rpc id)
  [%result id a+(turn dns (cork same (lead %s)))]
::
++  cancel-tx
  |=  [id=@t params=(map @t json)]
  ^-  [(unit cage) response:rpc]
  ?.  =(~(wyt by params) 3)
    [~ ~(params error:json-rpc id)]
  =/  sig=(unit @)              (sig:from-json params)
  =/  keccak=(unit @ux)         (hash:from-json params)
  =/  data=(unit [l2-tx ship])  (cancel:data:from-json params)
  ?.  &(?=(^ sig) ?=(^ keccak) ?=(^ data))
    [~ ~(parse error:json-rpc id)]
  :_  [%result id s+'ok']
  %-  some
  aggregator-action+!>([%cancel u.sig u.keccak u.data])
::
++  get-spawned
  |=  [id=@t params=(map @t json) scry=$-(ship (list [ship @ux]))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(ship:from-json params)
    ~(params error:json-rpc id)
  [%result id (spawned:to-json (scry u.ship))]
::
++  process-rpc
  |=  [id=@t params=(map @t json) action=l2-tx]
  ^-  [(unit cage) response:rpc]
  ?.  =((lent ~(tap by params)) 4)
    [~ ~(params error:json-rpc id)]
  =+  ^-  $:  sig=(unit @)
              from=(unit [ship proxy:naive])
              addr=(unit @ux)
          ==
    =,  from-json
    [(sig params) (from params) (address params)]
  ?:  |(?=(~ sig) ?=(~ from) ?=(~ addr))
    [~ ~(parse error:json-rpc id)]
  =/  tx=(unit tx:naive)  (build-l2-tx action u.from params)
  ?~  tx  [~ ~(parse error:json-rpc id)]
  =+  (gen-tx-octs:lib u.tx)
  :_  [%result id (hex:to-json 32 (hash-tx:lib p q))]
  %-  some
  aggregator-action+!>([%submit | u.addr u.sig %don u.tx])
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
::
++  get-config
  |=  [id=@t params=(map @t json) =roller-config]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 0)
    ~(params error:json-rpc id)
  [%result id (config:to-json roller-config)]
::
++  hash-transaction
  |=  [id=@t params=(map @t json) chain-id=@]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 4)
    ~(params error:json-rpc id)
  =+  ^-  $:  l2-tx=(unit l2-tx)
              nonce=(unit @ud)
              from=(unit [@p proxy:naive])
          ==
    =,  from-json
    [(tx params) (nonce params) (from params)]
  ?:  |(?=(~ nonce) ?=(~ from) ?=(~ l2-tx))
    ~(parse error:json-rpc id)
  =/  tx=(unit tx:naive)  (build-l2-tx u.l2-tx u.from params)
  ?~  tx  ~(parse error:json-rpc id)
  :+  %result  id
  =-  (hex:to-json 32 (hash-tx:lib p q))
  (unsigned-tx:lib chain-id u.nonce (gen-tx-octs:lib u.tx))
--
