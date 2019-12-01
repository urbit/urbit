/-  *btc-node-hook
|%
++  rpc
    =,  btc-rpc
    ::  Utility core
    ::
  =+  |%
      ::  Ideally +fall would have worked here, but we need to supply
      ::  the type of json (%s, %b...) only when unit is non empty so
      ::  this is like a reverse +bond (applied to the non-null case)
      ::  or a +bif that doesn't return a +unit
      ::
      ++  ferm  |*([a=(unit) t=term] ?~(a ~ t^u.a))
      ::
      ::  Base-58 parser
      ::    We could have used this base-58 parser from %zuse, that produces
      ::    a raw atom.
      ::
      ::  But it seems that it has some issues,
      ::
      ::          ++  de-base-58
      ::            =-  (bass 58 (plus -))
      ::            ;~  pose
      ::               (cook |=(a=@ (sub a 56)) (shim 'A' 'H'))
      ::               (cook |=(a=@ (sub a 57)) (shim 'J' 'N'))
      ::               (cook |=(a=@ (sub a 58)) (shim 'P' 'Z'))
      ::               (cook |=(a=@ (sub a 64)) (shim 'a' 'k'))
      ::               (cook |=(a=@ (sub a 65)) (shim 'm' 'z'))
      ::               (cook |=(a=@ (sub a 49)) (shim '1' '9'))
      ::            ==
      ::  > `@uc`(de-base58:mimes:html "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy")
      ::  0cG2DwD87dhp91wdcrWHsNFcYvRbfLvUbcxDFvwtN
      ::
      ::  compared to +fim:ag, with the extra cast as seen bellow,
      ::    because it returns @ux:
      ::
      ::  > %.  s+'3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy'  =,  dejs:format
      ::    =-  (cu - (su fim:ag))
      ::    |=(a=@ux `@uc`a)
      ::  0c3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy
      ::
      ++  to-base58
        |=  c=@t
        `@uc`(rash c fim:ag)
      ::
      ++  base58-to-cord
        |=  b=@uc
        ^-  @t
        ::  Removes leading 0c
        ::
        (rsh 3 2 (scot %uc b))
      ::
      ++  hex-to-cord
        |=  h=@ux
        ^-  @t
        ::  Removes leading 0x
        ::
        =-  (rsh 4 1 -)
        ::  Removes .
        ::
        %+  scan  (scow %ux h)
          ((slug |=(a=[@ @t] (cat 3 a))) (star dot) alp)
      ::
      ++  to-hex
        |=  c=@t
        ::  Parse hexadecimal to atom
        ::
        =-  `@ux`(rash - hex)
        ::  Group by 4-size block
        ::
        =-  (rsh 4 1 -)
        ::  Add leading 00
        ::
        (lsh 4 1 c)
      ::
      ++  ux-to-base
        |=(h=@ux `@uc`h)
      ::
      ++  ip-port-to-cord
      |=  [ip=@if port=@ud]
      ^-  cord
      =/  a=tape  (scow %if ip)
      =/  b=json  (numb:enjs:format port)
      ?>  ?=([%n *] b)
      ?~  a  ''
      (cat 3 (cat 3 (crip t.a) ':') p.b)
      ::
      ++  to-wall
        |=  =tape
        ^-  wall
        %+  roll  (flop tape)
        |=  [char=@tD =wall]
        ?~  wall
          [[char ~] ~]
        ?:  =('\0a' char)
          [~ wall]
        [[char i.wall] t.wall]
      --
    |%
    ++  request-to-rpc
      =,  enjs:format
      |=  req=request
      ^-  request:rpc:jstd
      :-  -.req
      ?-  -.req
          %generate
        :-  'generate'
        :-  %list
        ^-  (list json)
        :-  (numb blocks.req)
        ?~  max-tries.req  ~
        [(numb u.max-tries.req) ~]
      ::
          %get-block-count
        :-  'getblockcount'
        list+~
      ::
          %list-wallets
        :-  'listwallets'
        list+~
      ::
          %create-wallet
        :-  'createwallet'
        :-  %list
        :~  s+name.req
            b+disable-private-keys.req
        ==
      ::
          %get-memory-info
        :-  'getmemoryinfo'
        list+~
      ::
          %get-rpc-info
        :-  'getrpcinfo'
        list+~
      ::
          %help
        :-  'help'
        :-  %list
        ^-  (list json)
        ?~  command.req  ~
        [%s u.command.req]~
      ::
          %logging
        :-  'logging'
        :-  %list
        ^-  (list json)
        :~  ?+  include.req  [%a (turn include.req |=(s=logging-category [%s s]))]
              %all   [%a [%s 'all']~]
              %none  [%a [%s 'none']~]
            ==
          ?+  exclude.req  [%a (turn exclude.req |=(s=logging-category [%s s]))]
              %all   [%a [%s 'all']~]
              %none  [%a [%s 'none']~]
          ==
       ==
      ::
          %stop
        :-  'stop'
        list+~
      ::
          %uptime
        :-  'uptime'
        list+~
      ::
          %generate-to-address
        :-  'generatetoaddress'
        :-  %list
        ^-  (list json)
        :-  (numb n-blocks.req)
        :-  s+(base58-to-cord address.req)
        ?~  max-tries.req  ~
            [(numb u.max-tries.req) ~]
      ::
          %get-block-template
        :-  'getblocktemplate'
        :-  %list
        ^-  (list json)
        :_  ~
        :-  %o
        %-  molt
        ^-  (list (pair @t json))
        :-  ['rules' [%a (turn rules.req |=(s=rule [%s s]))]]
        :-  ['capabilities' [%a (turn capabilities.req |=(s=capability [%s s]))]]
        ?~  mode.req  ~
        ?+  u.mode.req  :-  ['mode' %s mode.u.mode.req]  ~
          [%proposal *]
          ?~  workid.u.mode.req
            :+  ['mode' %s mode.u.mode.req]
                ['data' %s (hex-to-cord data.u.mode.req)]  ~
          :^  ['mode' %s mode.u.mode.req]
              ['data' %s (hex-to-cord data.u.mode.req)]
              ['workid' %s u.workid.u.mode.req]  ~
        ==
      ::
          %get-mining-info
        :-  'getmininginfo'
        list+~
      ::
          %get-network-hash-ps
        :-  'getnetworkhashps'
        :-  %list
        =-  (skip - |=(=json =(json ~)))
        ^-  (list json)
        :~  ?~  n-blocks.req  ~
          (numb u.n-blocks.req)
          ?~  height.req  ~
          (numb u.height.req)
        ==
      ::
          %prioritise-transaction
        :-  'prioritisetransaction'
        :-  %object
        ^-  (list (pair @t json))
        :~  ['txid' %s (hex-to-cord txid.req)]
            ['fee_delta' (numb fee-delta.req)]
        ==
      ::
          %submit-block
        :-  'submitblock'
        :-  %list

        ^-  (list json)
        :_  ~
        [%s (hex-to-cord hex-data.req)]
      ::
          %submit-header
        :-  'submitheader'
        :-  %list
        ^-  (list json)
        :_  ~
        [%s (hex-to-cord hex-data.req)]
      ::
          %add-node
        :-  'addnode'
        :-  %list
        ^-  (list json)
        :-  [%s (ip-port-to-cord node.req port.req)]
        :-  [%s command.req]
        ~
      ::
          %clear-banned
        :-  'clearbanned'
        list+~
      ::
          %disconnect-node
        :-  'disconnectnode'
        :-  %object
        ^-  (list (pair @t json))
        ?@  node.req
          :_  ~
          ['nodeid' %n node-id.node.req]
        :_  ~
        ['address' %s (ip-port-to-cord address.node.req port.node.req)]
      ::
          %get-added-node-info
        :-  'getaddednodeinfo'
        :-  %list
        ^-  (list json)
        ?~  node.req  ~
        :_  ~
        =/  ip=cord
          =/  a  (scow %if u.node.req)
          ?~  a  ''
          (crip t.a)
        [%s ip]
      ::
          %get-connection-count
        :-  'getconnectioncount'
        list+~
      ::
          %get-net-totals
        :-  'getnettotals'
        list+~
      ::
          %get-network-info
        :-  'getnetworkinfo'
        list+~
      ::
          %get-node-addresses
        :-  'getnodeaddresses'
        :-  %list
        ^-  (list json)
        ?~  count.req  ~
        :_  ~
        (numb u.count.req)
      ::
          %get-peer-info
        :-  'getpeerinfo'
        list+~
      ::
          %list-banned
        :-  'listbanned'
        list+~
      ::
          %ping
        :-  'ping'
        list+~
      ::
          %set-ban
        :-  'setban'
        :-  %list
        ^-  (list json)
        :-  [%s subnet.req]
        :-  [%s command.req]
        ?~  ban-time.req  ~
        ?-  -.u.ban-time.req
          %dr  [(numb (div +.u.ban-time.req ~s1)) ~]
          %da  [(numb (unt:chrono:userlib +.u.ban-time.req)) [%b %.y] ~]
        ==
      ::
          %set-network-active
        :-  'setnetworkactive'
        :-  %list
        ^-  (list json)
        :_  ~
        [%b state.req]
      ==

    ::
    ++  parse-response
      =,  dejs:format

      |=  res=response:rpc:jstd
      ^-  response
      ~|  -.res
      ?>  ?=(%result -.res)
      ?+  id.res
        ~|  [%unsupported-response id.res]
        !!
      ::
          %generate
        :-  id.res
        %.  res.res
        (ar (su hex))
      ::
          %get-block-count
        :-  id.res
        (ni res.res)
      ::
          %list-wallets
        :-  id.res
        %.  res.res
        (ar so)
      ::
          %create-wallet
        :-  id.res
        %.  res.res
        (ot name+so warning+so ~)
      ::
          %get-memory-info
        :-  id.res
        %.  res.res
        %-  ot
        :_  ~
        :-  'locked'
        %-  ot
          :~  ['used' ni]
              ['free' ni]
              ['total' ni]
              ['locked' ni]
              ['chunks_free' ni]
              ['chunks_used' ni]
          ==
      ::
          %get-rpc-info
        :-  id.res
        %.  res.res
        %-  ot
        :_  ~
        :-  'active_commands'
        %-  ar
        %-  ot
          :~  ['method' so]
              :-  'duration'
              %+  cu
                |=  a/@u
                (mul (div ~s1 1.000) a)
              ni
          ==
      ::
          %help
        :-  id.res
        %.  res.res
        (cu to-wall sa)
      ::
          %logging
        :-  id.res
        %.  res.res
        (om bo)
      ::
          %stop
        :-  id.res
        (so res.res)
      ::
          %uptime
        :-  id.res
        %+  mul  ~s1
        (ni res.res)
      ::
          %generate-to-address
        :-  id.res
        %.  res.res
        (ar (su hex))
      ::
          %get-block-template
        :-  id.res
        %.  res.res
        %-  ot
          :~  ['version' ni]
              ['rules' (ar (cu ^rule so))]
              ['vbavailable' (om ni)]
              ['vbrequired' ni]
              ['previousblockhash' (su hex)]
              :-  'transactions'
                %-  ar
                %-  ot
                  :~  ['data' (su hex)]
                      ['txid' (su hex)]
                      ['hash' (su hex)]
                      ['depends' (ar ni)]
                      ['fee' ni]
                      ['sigops' ni]
                      ['weight' ni]
                  ==
              ['coinbaseaux' (ot ~[flags+so])]
              ['coinbasevalue' ni]
              ['target' (su hex)]
              ['mintime' (cu from-unix:chrono:userlib ni)]
              ['mutable' (ar (cu mutable so))]
              ['noncerange' so]
              ['sigoplimit' ni]
              ['sizelimit' ni]
              ['weightlimit' ni]
              ['curtime' (cu from-unix:chrono:userlib ni)]
              ['bits' (su hex)]
              ['height' ni]
              ['default_witness_commitment' (su hex)]
          ==
      ::
          %get-mining-info
        :-  id.res
        %.  res.res
        %-  ot
          :~  ['blocks' ni]
              ['currentblockweight' ni]
              ['currentblocktx' ni]
              ['difficulty' ne]
              ['networkhashps' ne]
              ['pooledtx' ni]
              ['chain' (cu network so)]
              ['warnings' so]
          ==
      ::
          %get-network-hash-ps
        :-  id.res
        (ne res.res)
      ::
          %prioritise-transaction
        :-  id.res
        (bo res.res)
      ::
          %submit-block
        :-  id.res
        (so res.res)
      ::
          %submit-header
        :-  id.res
        (so res.res)
      ::
          %add-node
        :-  id.res
        (ul res.res)
      ::
          %clear-banned
        :-  id.res
        (ul res.res)
      ::
          %disconnect-node
        :-  id.res
        (ul res.res)
      ::
          %get-added-node-info
        :-  id.res
        %.  res.res
        %-  ar
        %-  ot
          :~  ['addednode' so]
              ['connected' bo]
              :-  'addresses'
                %-  ar
                %-  ot
                :~  ['address' so]
                    ['connected' (cu connection so)]
                ==
          ==
      ::
          %get-connection-count
        :-  id.res
        (ni res.res)
      ::
          %get-net-totals
        :-  id.res
        %.  res.res
        %-  ot
          :~  ['totalbytesrecv' ni]
              ['totalbytessent' ni]
              ['timemillis' (cu |=(a=@u (from-unix:chrono:userlib (div a 1.000))) ni)]
              :-  'uploadtarget'
              %-  ot
                :~  ['timeframe' (cu |=(a=@u (mul a ~s1)) ni)]
                    ['target' ni]
                    ['target_reached' bo]
                    ['serve_historical_blocks' bo]
                    ['bytes_left_in_cycle' ni]
                    ['time_left_in_cycle' (cu |=(a=@u (mul a ~s1)) ni)]
                ==
          ==
      ::
          %get-network-info
        :-  id.res
        %.  res.res
        %-  ot
          :~  ['version' ni]
              ['subversion' so]
              ['protocolversion' ni]
              ['localservices' so]
              ['localrelay' bo]
              ['timeoffset' ni]
              ['connections' ni]
              ['networkactive' bo]
              :-  'networks'
                %-  ar
                %-  ot
                  :~  ['name' (cu network-type so)]
                      ['limited' bo]
                      ['reachable' bo]
                      ['proxy' so]
                      ['proxy_randomize_credentials' bo]
                  ==
              ['relayfee' ne]
              ['incrementalfee' ne]
              :-  'localaddresses'
                %-  ar
                %-  ot
                  :~  ['address' so]
                      ['port' ni]
                      ['score' ni]
                  ==
             ['warnings' so]
          ==
      ::
          %get-node-addresses
        :-  id.res
        %.  res.res
        %-  ar
        %-  ot
          :~  ['time' (cu from-unix:chrono:userlib ni)]
              ['services' ni]
              ['address' so]
              ['port' ni]
          ==
      ::
          %get-peer-info
        :-  id.res
        %.  res.res
        %-  ar
        %-  ou
          :~  ['id' (un ni)]
              ['addr' (un so)]
              ['addrbind' (un so)]
              ['addrlocal' (uf ~ (mu so))]
              ['services' (un so)]
              ['relaytxes' (un bo)]
              ['lastsend' (un (cu from-unix:chrono:userlib ni))]
              ['lastrecv' (un (cu from-unix:chrono:userlib ni))]
              ['bytessent' (un ni)]
              ['bytesrecv' (un ni)]
              ['conntime' (un (cu from-unix:chrono:userlib ni))]
              ['timeoffset' (un ni)]
              ['pingtime' (un ne)]
              ['minping' (un ne)]
              ['pingwait' (uf ~ (mu ni))]
              ['version' (un ni)]
              ['subver' (un so)]
              ['inbound' (un bo)]
              ['addnode' (un bo)]
              ['startingheight' (un ni)]
              ['banscore' (un ni)]
              ['synced_headers' (un ni)]
              ['synced_blocks' (un ni)]
              ['inflight' (un (ar ni))]
              ['whitelisted' (un bo)]
              ['minfeefilter' (un ne)]
              ['bytessent_per_msg' (un (om ni))]
              ['bytesrecv_per_msg' (un (om ni))]
          ==
      ::
          %list-banned
        :-  id.res
        %.  res.res
        %-  ar
        %-  ot
          :~  ['address' so]
              ['banned_until' (cu from-unix:chrono:userlib ni)]
              ['ban_created' (cu from-unix:chrono:userlib ni)]
              ['ban_reason' so]
          ==
      ::
          %ping
        :-  id.res
        (ul res.res)
      ::
          %set-ban
        :-  id.res
        (ul res.res)
      ::
          %set-network-active
        :-  id.res
        (bo res.res)
      ==
    --
  --
