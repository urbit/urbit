::  azimuth/roll rpc: command parsing and utilities
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
              ['cryptoSuite' (su dem)]
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
      ::
      ++  force
        |=  params=(map @t json)
        ^-  (unit ?)
        ?~  force=(~(get by params) 'force')  ~
        (bo u.force)
      --
    ::
    ++  to-json
      =,  enjs:format
      |%
      ++  pending-tx
        |=  pend-tx
        ^-  json
        %-  pairs
        :~  ['force' b+force]
            ['time' (^time time)]
            ['rawTx' (^raw-tx raw-tx)]
            (en-address address)
        ==
      ::
      ++  pending-txs
        |=  pending=(list pend-tx)
        ^-  json
        a+(turn pending pending-tx)
      ::
      ++  en-address   |=(a=@ux address+(hex 20 a))
      ::
      ++  raw-tx
        |=  raw-tx:naive
        ^-  json
        |^
        %-  pairs
        :~  ['tx' (parse-tx +.tx)]
            ['sig' (hex (as-octs:mimes:html sig))]
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
        ++  en-ship      |=(s=@p ship+(numb `@ud`s))
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
      ++  hist-txs
        |=  txs=(list hist-tx)
        ^-  json
        :-  %a
        %+  turn  txs
        |=  hist-tx
        ^-  json
        %-  pairs
        :~  ['time' (time p)]
            ['status' s+status.q]
            ['hash' (hex (as-octs:mimes:html hash.q))]
            ['type' s+type.q]
            ['ship' (ship ship.q)]
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
            :*  ['rift' s+(json-number rift.net)]
              ::
                :-  'keys'
                %-  pairs
                :~  ['life' s+(json-number life.keys.net)]
                    ['suite' s+(json-number suite.keys.net)]
                    ['auth' (hex 32 auth.keys.net)]
                    ['crypt' (hex 32 crypt.keys.net)]
                ==
              ::
                :-  'sponsor'
                %-  pairs
                ~[['has' b+has.sponsor.net] ['who' (numb `@ud`who.sponsor.net)]]
              ::
                ?~  escape.net  ~
                ['escape' (numb `@ud`u.escape.net)]~
        ==  ==
      ::
      ++  json-number
        |=  num=@
        ^-  @t
        =/  jon=json  (numb num)
        ?>(?=([%n *] jon) p.jon)
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
        a+(turn ships (cork @ud numb))
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
      ++  sponsored
        |=  [res=(list @p) req=(list @p)]
        ^-  json
        %-  pairs
        :~  ['residents' (ships res)]
            ['requests' (ships req)]
        ==
      ::
      ++  tx-status  |=(=^tx-status ^-(json s+status.tx-status))
      ::
      ++  roller-config
        |=  [az=^azimuth-config ro=^roller-config]
        ^-  json
        %-  pairs
        :~  ['azimuthRefreshRate' (numb (div refresh-rate.az ~s1))]
            ['nextBatch' (time next-batch.ro)]
            ['frequency' (numb (div frequency.ro ~s1))]
            ['rollerResendTime' (numb (div resend-time.ro ~s1))]
            ['rollerUpdateRate' (numb (div update-rate.ro ~s1))]
            ['contract' (hex 20 contract.ro)]
            ['chainId' (numb chain-id.ro)]
            ['timeSlice' (numb (div slice.ro ~s1))]
            ['rollerQuota' (numb quota.ro)]
        ==
      ::
      ++  azimuth-config
        |=  config=^azimuth-config
        ^-  json
        %-  pairs
        ['refreshRate' (numb (div refresh-rate.config ~s1))]~
      ::
      ++  hex
        |=  [p=@ q=@]
        ^-  json
        s+(crip ['0' 'x' ((x-co:co (mul 2 p)) q)])
      ::
      ++  naive-state
        |=  =^state:naive
        ^-  json
        |^
        %-  pairs
        :~  ['points' (points (tap:orp points.state))]
            ['operators' (operators operators.state)]
            ['dns' a+(turn dns.state (lead %s))]
        ==
        ::
        ++  orp  ((on ^ship point:naive) por:naive)
        ::
        ++  operators
          |=  =operators:naive
          ^-  json
          :-  %a
          %+  turn  ~(tap by operators)
          |=  [op=@ux addrs=(set @ux)]
          ^-  json
          %-  pairs
          :~  ['operator' (hex 20 op)]
              ['addresses' a+(turn ~(tap in addrs) (cury hex 20))]
          ==
        --
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
  roller-action+!>([%cancel u.sig u.keccak u.data])
::
++  get-spawned
  |=  [id=@t params=(map @t json) scry=$-(@p (list @p))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(ship:from-json params)
    ~(params error:json-rpc id)
  [%result id (ships:to-json (scry u.ship))]
::
++  spawns-remaining
  |=  [id=@t params=(map @t json) scry=$-(@p (list @p))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(ship:from-json params)
    ~(params error:json-rpc id)
  [%result id (numb:enjs:format (lent (scry u.ship)))]
::
++  sponsored-points
  |=  [id=@t params=(map @t json) scry=$-(@p [(list @p) (list @p)])]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(ship:from-json params)
    ~(params error:json-rpc id)
  [%result id (sponsored:to-json (scry u.ship))]
::
++  process-rpc
  |=  [id=@t params=(map @t json) action=l2-tx over-quota=$-(@p ?)]
  ^-  [(unit cage) response:rpc]
  ?.  ?|  =((lent ~(tap by params)) 4)
          =((lent ~(tap by params)) 5)
      ==
    [~ ~(params error:json-rpc id)]
  =?  params  =((lent ~(tap by params)) 4)
    (~(put by params) 'force' b+|)
  =+  ^-  $:  sig=(unit @)
              from=(unit [=ship proxy:naive])
              addr=(unit @ux)
              force=(unit ?)
          ==
    =,  from-json
    [(sig params) (from params) (address params) (force params)]
  ?:  |(?=(~ sig) ?=(~ from) ?=(~ addr) ?=(~ force))
    [~ ~(parse error:json-rpc id)]
  ?:  (over-quota ship.u.from)
    `[%error id '-32002' 'Max tx quota exceeded']
  =/  tx=(unit tx:naive)  (build-l2-tx action u.from params)
  ?~  tx  [~ ~(parse error:json-rpc id)]
  =+  (gen-tx-octs:lib u.tx)
  :_  [%result id (hex:to-json 32 (hash-tx:lib p q))]
  %-  some
  roller-action+!>([%submit u.force u.addr u.sig %don u.tx])
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
    [%result id (pending-txs:to-json pending)]
  ::
  ++  ship
    |=  [id=@t params=(map @t json) scry=$-(@p (list pend-tx))]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 1)
      ~(params error:json-rpc id)
    ?~  ship=(ship:from-json params)
      ~(parse error:json-rpc id)
    [%result id (pending-txs:to-json (scry u.ship))]
  ::
  ++  addr
    |=  [id=@t params=(map @t json) scry=$-(@ux (list pend-tx))]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 1)
      ~(params error:json-rpc id)
    ?~  address=(address:from-json params)
      ~(parse error:json-rpc id)
    [%result id (pending-txs:to-json (scry u.address))]
  ::
  ++  hash
    |=  [id=@t params=(map @t json) scry=$-(@ux (unit pend-tx))]
    ^-  response:rpc
    ?.  =((lent ~(tap by params)) 1)
      ~(params error:json-rpc id)
    ?~  hash=(hash:from-json params)
      ~(parse error:json-rpc id)
    ?~  tx=(scry u.hash)
      ~(not-found error:json-rpc id)
    [%result id (pending-tx:to-json u.tx)]
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
++  next-timer
  |=  [id=@t params=(map @t json) when=time]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 0)
    ~(params error:json-rpc id)
  [%result id (time:enjs:format when)]
::
++  history
  |=  [id=@t params=(map @t json) scry=$-(address:naive (list hist-tx))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  address=(address:from-json params)
    ~(parse error:json-rpc id)
  [%result id (hist-txs:to-json (scry u.address))]
::
++  get-config
  |=  [id=@t params=(map @t json) config=[azimuth-config roller-config]]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 0)
    ~(params error:json-rpc id)
  [%result id (roller-config:to-json config)]
::
++  hash-transaction
  |=  [id=@t params=(map @t json) chain-id=@ header=? reverse=?]
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
  =/  =octs
    %.  [chain-id u.nonce (gen-tx-octs:lib u.tx)]
    ?:  header
      unsigned-tx:lib
    prepare-for-sig:lib
  :+  %result  id
  %-  hex:to-json
  ?:  reverse
    p.octs^(rev 3 octs)
  32^(hash-tx:lib octs)
::
++  hash-raw-transaction
  |=  [id=@t params=(map @t json)]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 4)
    ~(params error:json-rpc id)
  =+  ^-  $:  sig=(unit @)
              l2-tx=(unit l2-tx)
              from=(unit [=ship proxy:naive])
          ==
    =,  from-json
    [(sig params) (tx params) (from params)]
  ?:  |(?=(~ sig) ?=(~ from) ?=(~ l2-tx))
    ~(parse error:json-rpc id)
  =/  tx=(unit tx:naive)  (build-l2-tx u.l2-tx u.from params)
  ?~  tx  ~(parse error:json-rpc id)
  :+  %result  id
  %+  hex:to-json  32
  (hash-raw-tx:lib u.sig (gen-tx-octs:lib u.tx) u.tx)
::
++  get-naive
  |=  [id=@t params=(map @t json) =^state:naive]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 0)
    ~(params error:json-rpc id)
  [%result id (naive-state:to-json state)]
::
++  get-refresh
  |=  [id=@t params=(map @t json) =azimuth-config]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 0)
    ~(params error:json-rpc id)
  [%result id (azimuth-config:to-json azimuth-config)]
::
++  quota-remaining
  |=  [id=@t params=(map @t json) quota-left=$-(@p @ud)]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(ship:from-json params)
    ~(params error:json-rpc id)
  [%result id (numb:enjs:format (quota-left u.ship))]
::
++  ship-allowance
  |=  [id=@t params=(map @t json) allowance=$-(@p (unit @ud))]
  ^-  response:rpc
  ?.  =((lent ~(tap by params)) 1)
    ~(params error:json-rpc id)
  ?~  ship=(ship:from-json params)
    ~(params error:json-rpc id)
  :+  %result  id
  ?^  allow=(allowance u.ship)
    (numb:enjs:format u.allow)
  s+(crip "No quota restrictions for {(scow %p u.ship)}")
--
