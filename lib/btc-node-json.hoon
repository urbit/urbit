/-  *btc-node-hook
/+  base64
=,  format
=>  =,  dejs
    |%
    ::  %ferm: Checks the unit for ~ and returns a json e.g. [%s @ta]
    ::
    ++  ferm  |*([a=(unit) t=term] ?~(a ~ t^u.a))
    ::  %feud: Checks the unit for ~ and returns a [%n @ud] json
    ::
    ++  feud  |=(a=(unit @u) ?~(a ~ (numb:enjs u.a)))
    ::  %method: Removes 'hep' (-) from a %tas producing the RPC method
    ::
    ::  (e.g. %add-multisig-address -> 'addmultisigaddress')
    ::
    ++  method
      |=  t=@t
      ^-  @t
      %+  scan  (scow %tas t)
      ((slug |=(a=[@ @t] (cat 3 a))) (star hep) alp)
    ::  %groups: used in %list-address-groupings
    ::
    ++  groups
      |=  l=(list @t)
      ^-  (list [?(@uc [%bech32 @t]) @t (unit @t)])
      ?>  ?=([@t @t *] l)
      :_  ~
      :*  (addr-type-validator i.l)
        ::
          i.t.l
        ::
          ?~  t.t.l
            ~
          (some i.t.t.l)
      ==
    ::  %base58-to-cord: parses @uc to BTC addresses (legacy and p2sh)
    ::
    ++  base58-to-cord
      |=  b=@uc
      ^-  @t
      ::  Removes leading 0c
      ::
      (rsh 3 2 (scot %uc b))
    ::  %hex-to-cord: parses hexadecimal to cords without dots and 0x
    ::
    ++  hex-to-cord
      |=  h=@ux
      ^-  @t
      %-  crip
      =-  ((x-co:co (mul 2 p)) q)
      (as-octs:mimes:html h)
    ::
    ++  hash-to-cord
      |=  h=@ux
      ^-  @t
      %-  crip
      ::  extend with zeros up to 64 bytes
      ::
      ((x-co:co 64) h)
    ::  %addr-type-validator: parses BTC addresses (legacy and p2sh) to @uc
    ::
    ::    bech32/segwit addressed are tagged separately.
    ::
    ++  addr-type-validator
      |=  addr=@t
      ^-  ?(@uc [%bech32 @t])
      =,  dejs
      =/  res=(unit @u)  (rush addr fim:ag)
      ?~  res
        ::  TODO: fim:ag doesn't parse %bech32 addresses
        ::
        bech32+addr
      `@uc`u.res
    ::  %to-hex: parses hexadecimal to @ux with separator dots and 0x
    ::
    ++  to-hex
      |=  h=@t
      ^-  @ux
      ?:  =('' h)  0x0
      ::  Add leading 00
      ::
      =+  (lsh 3 2 h)
      ::  Group by 4-size block
      ::
      =+  (rsh 3 2 -)
      ::  Parse hex to atom
      ::
      `@ux`(rash - hex)
    ::
    ++  ip-port-to-cord
      |=  [ip=@if port=@ud]
      %-  crip
      :(weld (slag 1 (scow %if ip)) ":" ((d-co:co 1) port))
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
    ::
    ++  json-parser
      |%
      ::  %vin:json-parser
      ::
      ::    Used in:
      ::    - $raw-transaction:json-parser
      ::    - %decode-psbt
      ::    - %decode-raw-transaction
      ::
      ++  vin
        =-  (ar (ou -))
        :~  ['txid' (uf ~ (mu (cu to-hex so)))]
            ['vout' (uf ~ (mu ni))]
          ::
            :-  'scriptSig'
            =-  (uf ~ (mu (ot -)))
            :~  ['asm' so]
              ::
                ['hex' (cu to-hex so)]
            ==
          ::
            :-  'txinwitness'
            =-  (uf ~ (mu -))
            (ar (cu to-hex so))
          ::
            ['sequence' (un ni)]
        ==
      ::  %vout:json-parser
      ::
      ::    Used in:
      ::    - $raw-transaction:json-parser
      ::    - %decode-psbt
      ::    - %decode-raw-transaction
      ::
      ++  vout
        =-  (ar (ot -))
        :~  ['value' no]
            ['n' ni]
          ::
            :-  'scriptPubKey'
            %-  ou
            :~  ['asm' (un so)]
                ['hex' (un (cu to-hex so))]
                ['reqSigs' (uf ~ (mu ni))]
                ['type' (un so)]
              ::
                :-  'addresses'
                =-  (uf ~ (mu -))
                (ar (cu addr-type-validator so))
        ==  ==
      ::  %script:json-parser
      ::
      ::    Used in:
      ::    - %decode-psbt
      ::
      ++  script
        =-  (uf ~ (mu (ot -)))
        :~  ['asm' so]
            ['hex' (cu to-hex so)]
            ['type' so]
        ==
      ::  %utxo:json-parser
      ::
      ::    Used in:
      ::    - %decode-psbt
      ::
      ++  utxo
        =-  (uf ~ (mu (ot -)))
        :~  ['amount' no]
          ::
            :-  'scriptPubKey'
            %-  ot
            :~  ['asm' so]
                ['hex' (cu to-hex so)]
                ['type' so]
                ['address' (cu addr-type-validator so)]
        ==  ==
      ::  %raw-transaction:json-parser
      ::
      ::    Used in:
      ::    - %get-block
      ::    - %get-raw-transaction
      ::    - %import-pruned-funds
      ::
      ++  raw-transaction
        %-  ou
        =,  json-parser
        :~  ['in_active_chain' (uf ~ (mu bo))]
            ['hex' (un (cu to-hex so))]
            ['txid' (un (cu to-hex so))]
            ['hash' (un (cu to-hex so))]
            ['size' (un ni)]
            ['vsize' (un ni)]
            ['weight' (un ni)]
            ['version' (un no)]
            ['locktime' (un ni)]
            ['vin' (un vin)]
            ['vout' (un vout)]
            ['blockhash' (uf ~ (mu (cu to-hex so)))]
            ['confirmations' (uf ~ (mu ni))]
            ['blocktime' (uf ~ (mu ni))]
            ['time' (uf ~ (mu ni))]
        ==
      ::  %mem-pool:json-parser
      ::
      ::    Used in:
      ::    - %get-raw-mempool
      ::    - %get-mempool-ancestors
      ::    - %get-mempool-descendants
      ::    - %get-mempool-entry
      ::
      ++  mem-pool
        %-  ou
        :~  ['size' (uf ~ (mu ni))]
            ['vsize' (un ni)]
            ['weight' (un ni)]
            ['fee' (un no)]
            ['modifiedfee' (un no)]
            ['time' (un ni)]
            ['height' (un ni)]
            ['descendantcount' (un ni)]
            ['descendantsize' (un ni)]
            ['descendantfees' (un no)]
            ['ancestorcount' (un ni)]
            ['ancestorsize' (un ni)]
            ['ancestorfees' (un no)]
            ['wtxid' (un (cu to-hex so))]
          ::
            :-  'fees'
            %-  un
            %-  ot
            :~  ['base' no]
                ['modified' no]
                ['ancestor' no]
                ['descendant' no]
            ==
          ::
            ['depends' (un (ar (cu to-hex so)))]
            ['spentby' (un (ar (cu to-hex so)))]
            ['bip125-replaceable' (un bo)]
        ==
      ::  %tx-in-block:json-parser
      ::
      ::    Used in:
      ::    - %get-block
      ::    - %get-raw-transaction
      ::
      ++  tx-in-block
        =-  (ar (ou -))
        :~  ['address' (uf ~ (mu (cu addr-type-validator so)))]
            ['category' (un (cu category so))]
            ['amount' (un no)]
            ['label' (uf ~ (mu so))]
            ['vout' (un ni)]
            ['fee' (uf ~ (mu no))]
            ['confirmations' (un ni)]
            ['blockhash' (uf ~ (mu (cu to-hex so)))]
            ['blockindex' (uf ~ (mu ni))]
            ['blocktime' (uf ~ (mu ni))]
            ['txid' (un (cu to-hex so))]
            ['time' (un ni)]
            ['timereceived' (uf ~ (mu ni))]
          ::
            :-  'walletconflicts'
            =-  (uf ~ (mu -))
            (ar (cu to-hex so))
          ::
            ['bip125-replaceable' (un (cu bip125-replaceable so))]
            ['abandoned' (uf ~ (mu bo))]
            ['comment' (uf ~ (mu so))]
            ['to' (uf ~ (mu so))]
        ==
      ::  %prev-txs:json-parser
      ::
      ::    Used in:
      ::    - %sign-raw-transaction-with-key
      ::    - %sign-raw-transaction-with-wallet
      ::
      ++  prev-txs
        |=  t=prev-tx
        ^-  (list (pair @t json))
        :~  ['txid' s+(hash-to-cord txid.t)]
            ['vout' (numb:enjs:format vout.t)]
            ['scriptPubKey' s+(hex-to-cord script-pubkey.t)]
          ::
            :-  'redeemScript'
            ?~  redeem-script.t
              ~
            s+(hex-to-cord u.redeem-script.t)
          ::
            :-  'witnessScript'
            ?~  witness-script.t
              ~
            s+(hex-to-cord u.witness-script.t)
          ::
            ['amount' n+amount.t]
        ==
      --
    --
|%
++  btc-rpc
  =,  ^btc-rpc
  |%
  ++  request-to-rpc
    =,  enjs:format
    |=  req=request
    ^-  request:rpc:jstd
    :^  -.req  (method -.req)  %list
    ^-  (list json)
    ?-    -.req
    ::  Blockchain
    ::
        %get-best-block-hash
      ~
    ::
        %get-block
      ~[s+(hash-to-cord blockhash.req) (feud verbosity.req)]
      ::
        %get-blockchain-info
      ~
    ::
        %get-block-count
      ~
    ::
        %get-block-filter
      ~[s+(hash-to-cord block-hash.req) (ferm filter-type.req %s)]
    ::
        %get-block-hash
      ~[(numb height.req)]
    ::
        %get-block-header
      ~[s+(hash-to-cord blockhash.req) (ferm verbose.req %b)]
    ::
        %get-block-stats
      :~  =*  h  hash-or-height.req
          ?-  -.h
            %num  (numb +.h)
            %hex  s+(hash-to-cord +.h)
          ==
        ::
          ?~  stats.req  ~
          a+(turn u.stats.req |=(a=@t s+a))
      ==
    ::
        %get-chain-tips
      ~
    ::
        %get-chain-tx-stats
      :~  (feud n-blocks.req)
        ::
          ?~  blockhash.req
            ~
          s+(hash-to-cord u.blockhash.req)
      ==
    ::
        %get-difficulty
      ~
    ::
        %get-mempool-ancestors
      ~[s+(hash-to-cord txid.req) (ferm verbose.req %b)]
    ::
        %get-mempool-descendants
      ~[s+(hash-to-cord txid.req) (ferm verbose.req %b)]
    ::
        %get-mempool-entry
      ~[s+(hash-to-cord txid.req)]
    ::
        %get-mempool-info
      ~
    ::
        %get-raw-mempool
      ~[(ferm verbose.req %b)]
    ::
        %get-tx-out
      :~  s+(hash-to-cord txid.req)
        ::
          (numb n.req)
        ::
          (ferm include-mempool.req %b)
      ==
    ::
        %get-tx-out-proof
      :~  :-  %a
          %+  turn  tx-ids.req
          |=  a=@ux
          s+(hex-to-cord a)
        ::
          ?~  blockhash.req
            ~
          s+(hex-to-cord u.blockhash.req)
      ==
    ::
        %get-tx-outset-info
      ~
    ::
        %precious-block
      ~[s+(hash-to-cord blockhash.req)]
    ::
        %prune-blockchain
      ~[(numb height.req)]
    ::
        %save-mempool
      ~
    ::
        %scan-tx-outset
      :~  s+action.req
        ::
          :-  %a
          %+  turn  scan-objects.req
          |=  s-o=scan-object
          ^-  json
          ?@  s-o
            s+s-o
          ?>  ?=([@t (unit range)] s-o)
          %-  pairs
          :~  ['desc' s+desc.object.s-o]
            ::
              :-  'range'
              ^-  json
              ?~  range.object.s-o
                ~
              =*  r  u.range.object.s-o
              ?@  r
                (numb r)
              a+~[(numb -.r) (numb +.r)]
      ==  ==
    ::
        %verify-chain
      ~[(feud check-level.req) (feud n-blocks.req)]
    ::
        %verify-tx-out-proof
      ~[s+proof.req]
    ::  Control
    ::
        %get-memory-info
      ~
    ::
        %get-rpc-info
      ~
    ::
        %help
      ?~  command.req  ~
      [%s u.command.req]~
    ::
        %logging
      :~  ?+  include.req  [%a (turn include.req |=(s=logging-category [%s s]))]
            %all   [%a [%s 'all']~]
            %none  [%a [%s 'none']~]
          ==
          ?+  exclude.req  [%a (turn exclude.req |=(s=logging-category [%s s]))]
              %all   [%a [%s 'all']~]
              %none  [%a [%s 'none']~]
      ==  ==
    ::
        %stop
      ~
    ::
        %uptime
      ~
    ::  Generating
    ::
        %generate
      :-  (numb blocks.req)
      ?~  max-tries.req
        ~
      [(numb u.max-tries.req) ~]
    ::
        %generate-to-address
      :-  (numb n-blocks.req)
      :-  [%s ?^(address.req +.address.req (base58-to-cord address.req))]
      ?~  max-tries.req
        ~
      [(numb u.max-tries.req) ~]
    ::  Mining
    ::
        %get-block-template
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
      ~
    ::
        %get-network-hash-ps
      :~  ?~  n-blocks.req  ~
        (numb u.n-blocks.req)
        ?~  height.req  ~
        (numb u.height.req)
      ==
    ::
        %prioritise-transaction
      :~  [%s (hash-to-cord txid.req)]
          :: dummy null argument, could be omitted by using named
          :: instead of positional json rpc arguments
          :: see https://bitcoincore.org/en/doc/0.18.0/rpc/mining/prioritisetransaction/
          ~
          (numb fee-delta.req)
      ==
    ::
        %submit-block
      [%s (hex-to-cord hex-data.req)]~
    ::
        %submit-header
      [%s (hex-to-cord hex-data.req)]~
    ::  Network
    ::
        %add-node
      :-  [%s (ip-port-to-cord node.req port.req)]
      :-  [%s command.req]
      ~
    ::
        %clear-banned
      ~
    ::
        %disconnect-node
      ?@  node.req
        :_  ~
        [%n node-id.node.req]
      :_  ~
      [%s (ip-port-to-cord address.node.req port.node.req)]
    ::
        %get-added-node-info
      ?~  node.req  ~
      :_  ~
      =/  ip=cord
        =/  a  (scow %if u.node.req)
        ?~  a  ''
        (crip t.a)
      [%s ip]
    ::
        %get-connection-count
      ~
    ::
        %get-net-totals
      ~
    ::
        %get-network-info
      ~
    ::
        %get-node-addresses
      ?~  count.req  ~
      :_  ~
      (numb u.count.req)
    ::
        %get-peer-info
      ~
    ::
        %list-banned
      ~
    ::
        %ping
      ~
    ::
        %set-ban
      :-  [%s subnet.req]
      :-  [%s command.req]
      ?~  ban-time.req  ~
      ?-  -.u.ban-time.req
        %dr  [(numb (div +.u.ban-time.req ~s1)) ~]
        %da  [(numb (unt:chrono:userlib +.u.ban-time.req)) [%b %.y] ~]
      ==
    ::
        %set-network-active
      :_  ~
      [%b state.req]
    ::  Raw Transactions
    ::
        %analyze-psbt
      :_  ~
      :-  %s
      ?^  (de:base64 psbt.req)
        psbt.req
      (en:base64 (as-octs:mimes:html psbt.req))
    ::
        %combine-psbt
      :_  ~
      :-  %a
      %+  turn   txs.req
      |=  a=@t
      :-  %s
      ?^  (de:base64 a)
        a
      (en:base64 (as-octs:mimes:html a))
    ::
        %combine-raw-transaction
      ~[a+(turn txs.req |=(a=@ux s+(hex-to-cord a)))]
    ::
        %convert-to-psbt
      :~  s+(hex-to-cord hex-string.req)
          (ferm permit-sig-data.req %b)
          (ferm is-witness.req %b)
      ==
    ::
        %create-psbt
      :~  :-  %a
          %+  turn  inputs.req
          |=  a=input
          ^-  json
          %-  pairs  ^-  (list (pair @t json))
          :~  ['txid' s+(hash-to-cord txid.a)]
              ['vout' (numb vout.a)]
              ['sequence' (numb sequence.a)]
          ==
        ::
          =*  out  outputs.req
          :-  %a
          %+  weld
            :_  ~
            (pairs [-.data.out s+(hex-to-cord +.data.out)]~)
          ::
            %+  turn  addresses.out
            |=  [address=?(address [%bech32 @t]) amount=@t]
            ^-  json
            %-  pairs
            :~  :-  'address'
                :-  %s
                ?^  address
                  +.address
                (base58-to-cord address)
              ::
                ['amount' n+amount]
            ==
        ::
          (feud locktime.req)
          (ferm replaceable.req %b)
      ==
    ::
        %create-raw-transaction
      :~  :-  %a
          ^-  (list json)
          %+  turn  inputs.req
          |=  a=input
          ^-  json
          %-  pairs
          ^-  (list (pair @t json))
          :~  ['txid' s+(hash-to-cord txid.a)]
              ['vout' (numb vout.a)]
              ['sequence' (numb sequence.a)]
          ==
        ::
          =*  out  outputs.req
          :-  %a
          %+  weld
            :_  ~
            (pairs [-.data.out s+(hex-to-cord +.data.out)]~)
        ::
          %+  turn  addresses.out
          |=  [address=?(address [%bech32 @t]) amount=@t]
          ^-  json
          %-  pairs
          :_  ~
          :_  n+amount
          ?^  address
            +.address
          (base58-to-cord address)
        ::
          (feud locktime.req)
          (ferm replaceable.req %b)
      ==
    ::
        %decode-psbt
      :_  ~
      :-  %s
      ?^  (de:base64 psbt.req)
        psbt.req
      (en:base64 (as-octs:mimes:html psbt.req))
    ::
        %decode-raw-transaction
      :~  s+(hex-to-cord hex-string.req)
          b+is-witness.req
      ==
    ::
        %decode-script
      ~[s+(hex-to-cord hex-string.req)]
    ::
        %finalize-psbt
      :~  :-  %s
          ?^  (de:base64 psbt.req)
            psbt.req
          (en:base64 (as-octs:mimes:html psbt.req))
        ::
          (ferm extract.req %b)
      ==
    ::
        %fund-raw-transaction
      :~  s+(hex-to-cord hex-string.req)
        ::
          %-  pairs
          ?~  options.req
            ~
          =*  opts  u.options.req
          ::  Excludes ~ elements
          ::
          =-  (skip - |=([@t a=json] =(a ~)))
          ^-  (list (pair @t json))
          :~  :-  'changeAddress'
              ?~  change-address.opts
                ~
              =*  a  u.change-address.opts
              [%s ?^(a +.a (base58-to-cord a))]
            ::
              ['changePosition' (feud change-position.opts)]
              ['change_type' (ferm change-type.opts %s)]
              ['includeWatching' (ferm include-watching.opts %b)]
              ['lockUnspents' (ferm lock-unspents.opts %b)]
              ['feeRate' (ferm fee-rate.opts %s)]
            ::
              :-  'subtractFeeFromOutputs'
              ?~  subtract-fee-from-outputs.opts
                ~
              :-  %a  ^-  (list json)
              (turn u.subtract-fee-from-outputs.opts numb)
            ::
              ['replaceable' (ferm replaceable.opts %b)]
              ['conf_target' (feud conf-target.opts)]
              ['estimate_mode' (ferm mode.opts %s)]
          ==
        ::
          b+is-witness.req
      ==
    ::
        %get-raw-transaction
      :~  s+(hash-to-cord txid.req)
          (ferm verbose.req %b)
        ::
          ?~  blockhash.req
            ~
          s+(hash-to-cord u.blockhash.req)
      ==
    ::
        %join-psbts
      ~[a+(turn txs.req |=(a=@t s+a))]
    ::
        %send-raw-transaction
      :~  s+(hex-to-cord hex-string.req)
          (ferm max-fee-rate.req %s)
      ==
    ::
        %sign-raw-transaction-with-key
      :~  s+(hex-to-cord hex-string.req)
        ::
          :-  %a  ^-  (list json)
          %+  turn   priv-keys.req
          |=  a=@t
          :-  %s
          ?^  (de:base64 a)
            a
          (en:base64 (as-octs:mimes:html a))
        ::
          ?~  prev-txs.req
            ~
          =*  txs  u.prev-txs.req
          :-  %a  ^-  (list json)
          %+  turn  txs
          |=  a=prev-tx
          (pairs (prev-txs:json-parser a))
        ::
          (ferm sig-hash-type.req %s)
      ==
    ::
        %test-mempool-accept
      :~  :-  %a  ^-  (list json)
          %+  turn  raw-txs.req
          |=(a=@ux s+(hex-to-cord a))
        ::
          (ferm max-fee-rate.req %s)
      ==
    ::
        %utxo-update-psbt
      :~  :-  %s
          ?^  (de:base64 psbt.req)
            psbt.req
          (en:base64 (as-octs:mimes:html psbt.req))
        ::
          :-  %a  ^-  (list json)
          ?~  descriptors.req  ~
          %+  turn  u.descriptors.req
          |=  =descriptor
          ?@  descriptor
            s+descriptor
          ?~  range.descriptor
            ~
          =*  range  u.range.descriptor
          ?@  range
            (numb range)
          a+~[(numb -.range) (numb +.range)]
      ==
    ::  Util
    ::
        %create-multi-sig
      :~  (numb n-required.req)
        ::
          :-  %a
          %+  turn  keys.req
          |=(a=@ux s+(hex-to-cord a))
        ::
          (ferm address-type.req %s)
      ==
    ::
        %derive-addresses
      ::  Sending a (unit range), resulting in a "null" parameter,
      ::  returns this from the RPC node
      ::  "Range should not be specified for an un-ranged descriptor"
      ::
      =-  (skip - |=(a=json =(a ~)))
      ^-  (list json)
      :~  s+descriptor.req
        ::
          ?~  range.req
            ~
          =*  range  u.range.req
          ?@  range
            (numb range)
          a+~[(numb -.range) (numb +.range)]
      ==
    ::
        %estimate-smart-fee
      =-  (skip - |=(a=json =(a ~)))
      ^-  (list json)
      :~  (numb conf-target.req)
          (ferm mode.req %s)
      ==
    ::
        %get-descriptor-info
      ~[s+descriptor.req]
    ::
        %sign-message-with-privkey
      ~[s+privkey.req s+message.req]
    ::
        %validate-address
      :_  ~
      :-  %s
      ?^  address.req
        +.address.req
      (base58-to-cord address.req)
    ::
        %verify-message
      :~  :-  %s
          ?^  address.req
            +.address.req
          (base58-to-cord address.req)
        ::
          s+signature.req
          s+message.req
      ==
    ::  Wallet
    ::
        %abandon-transaction
      ~[s+(hash-to-cord txid.req)]
    ::
        %abort-rescan
      ~
    ::
        %add-multisig-address
      :~  (numb n-required.req)
        ::
          :-  %a
          %+  turn  keys.req
          |=  a=?(address [%bech32 @t])
          :-  %s
          ?^  a
            +.a
          (base58-to-cord a)
        ::
          (ferm label.req %s)
          s+address-type.req
      ==
    ::
        %backup-wallet
      ~[s+destination.req]
    ::
        %bump-fee
      :~  s+(hash-to-cord txid.req)
        ::
          %-  pairs
          ?~  options.req
            ~
          =*  opts  u.options.req
          ::  Excludes ~ elements
          ::
          =-  (skip - |=([@t a=json] =(a ~)))
          ^-  (list (pair @t json))
          :~  ['confTarget' (feud conf-target.opts)]
              ['totalFee' (ferm total-fee.opts %n)]
              ['fee_rate' (ferm total-fee.opts %n)]
              ['replaceable' (ferm replaceable.opts %b)]
              ['estimate_mode' (ferm mode.opts %s)]
      ==  ==
    ::
        %create-wallet
      :~  s+name.req
          (ferm disable-private-keys.req %b)
          (ferm blank.req %b)
          (ferm passphrase.req %s)
          (ferm avoid-reuse.req %b)
      ==
    ::
        %dump-privkey
      :_  ~
      :-  %s
      ?^  address.req
        +.address.req
      (base58-to-cord address.req)
    ::
        %dump-wallet
      ~[s+filename.req]
    ::
        %encrypt-wallet
      ~[s+passphrase.req]
    ::
        %get-addresses-by-label
      ~[s+label.req]
    ::
        %get-address-info
      :_  ~
      :-  %s
      ?^  address.req
        +.address.req
      (base58-to-cord address.req)
    ::
        %get-balance
      ?~  +.req
        ~
      =/  req  u.+.req
      :~  (ferm dummy.req %s)
          (feud minconf.req)
          (ferm include-watch-only.req %b)
          (ferm avoid-reuse.req %b)
      ==
    ::
        %get-balances
      ~
    ::
        %get-new-address
      :~  (ferm label.req %s)
        ::
          (ferm address-type.req %s)
      ==
    ::
        %get-raw-change-address
      ~[(ferm address-type.req %s)]
    ::
        %get-received-by-address
      :~  :-  %s
          ?^  address.req
            +.address.req
          (base58-to-cord address.req)
        ::
          (numb minconf.req)
      ==
    ::
        %get-received-by-label
      ~[s+label.req (feud minconf.req)]
    ::
        %get-transaction
      :~  s+(hash-to-cord txid.req)
          (ferm include-watch-only.req %b)
          (ferm verbose.req %b)
      ==
    ::
        %get-unconfirmed-balance
      ~
    ::
        %get-wallet-info
      ~
    ::
        %import-address
      :~  :-  %s
          ?-  -.address.req
              %addr
            (base58-to-cord +.address.req)
          ::
              %bech32
            +.address.req
          ::
              %script
            (hex-to-cord +.address.req)
          ==
        ::
          (ferm label.req %s)
          (ferm rescan.req %b)
          (ferm p2sh.req %b)
      ==
    ::
        %import-multi
      :~  ?~  requests.req
            ~
          =*  reqs  requests.req
          :-  %a
          %+  turn  reqs
          |=  r=import-request
          %-  pairs
          ::  Exclude nulls
          ::
          =-  (skip - |=([@t a=json] =(a ~)))
          ^-  (list (pair @t json))
          :~  ['desc' (ferm desc.r %s)]
            ::
              :-  'scriptPubKey'
              =*  scri  script-pubkey.r
              ?-   -.scri
                  %script
                [%s s.scri]
              ::
                  %address
                %-  pairs
                :_  ~
                :-  'address'
                :-  %s
                =*  a  a.script-pubkey.r
                ?^  a
                  +.a
                (base58-to-cord a)
              ==
            ::
              :-  'timestamp'
              :-  %s
              ?:  ?=(%now timestamp.r)
                %now
              (scot %da timestamp.r)
            ::
              ['redeemScript' (ferm redeem-script.r %s)]
            ::
              ['witnessScript' (ferm witness-script.r %s)]
            ::
              :-  'pubkeys'
              ?~  pubkeys.r
                ~
              a+(turn u.pubkeys.r |=(a=@t s+a))
            ::
              :-  'keys'
              ?~  keys.r
                ~
              a+(turn u.keys.r |=(a=@t s+a))
            ::
              :-  'range'
              ?~  range.r
                ~
              =+  u.range.r
              ?@  -
                (numb -)
              a+~[(numb -<) (numb ->)]
            ::
              ['internal' (ferm internal.r %b)]
              ['watchonly' (ferm watchonly.r %b)]
              ['label' (ferm label.r %s)]
              ['keypool' (ferm keypool.r %b)]
          ==
        ::
          ?~  options.req
            ~
          (pairs ['rescan' b+u.options.req]~)
      ==
    ::
        %import-privkey
      :~  s+privkey.req
          (ferm label.req %s)
          (ferm rescan.req %b)
      ==
    ::
        %import-pruned-funds
      :~  s+(hex-to-cord raw-transaction.req)
          s+tx-out-proof.req
      ==
    ::
        %import-pubkey
      :~  s+(hex-to-cord pubkey.req)
          (ferm label.req %s)
          (ferm rescan.req %b)
      ==
    ::
        %import-wallet
      ~[s+filename.req]
    ::
        %key-pool-refill
      ~[(feud new-size.req)]
    ::
        %list-address-groupings
      ~
    ::
        %list-labels
      ~[(ferm purpose.req %s)]
    ::
        %list-lock-unspent
      ~
    ::
        %list-received-by-address
      ?~  +.req
        ~
      =/  req  u.+.req
      ::  BTC node can't take a null as the address
      ::  "JSON value is not a string as expected"
      ::  so we remove it from the parameters
      ::
      =-  (skip - |=(a=json =(a s+'null')))
      ^-  (list json)
      :~  (feud minconf.req)
          (ferm include-empty.req %b)
          (ferm include-watch-only.req %b)
        ::
          =*  addr  address-filter.req
          :-  %s
          ?~  addr  'null'
          ?^  u.addr
            +.u.addr
          (base58-to-cord u.addr)
      ==
    ::
        %list-received-by-label
      ?~  +.req
        ~
      =/  req  u.+.req
      :~  (feud minconf.req)
          (ferm include-empty.req %b)
          (ferm include-watch-only.req %b)
      ==
    ::
        %lists-in-ceblock
      ?~  +.req
        ~
      =/  req  u.+.req
      :~  ?~  blockhash.req
            ~
          s+(hash-to-cord u.blockhash.req)
        ::
          (feud target-confirmations.req)
          (ferm include-watch-only.req %b)
          (ferm include-removed.req %b)
      ==
    ::
        %list-transactions
      ?~  +.req
        ~
      =/  req  u.+.req
      :~  (ferm label.req %s)
          (feud count.req)
          (feud skip.req)
          (ferm include-watch-only.req %b)
      ==
    ::
        %list-unspent
      ?~  +.req
        ~
      =/  req  u.+.req
      :~  (feud minconf.req)
          (feud maxconf.req)
        ::
          ?~  addresses.req  ~
          =*  addrs  u.addresses.req
          :-  %a
          %+  turn  addrs
          |=  a=?(address [%bech32 @t])
          ^-  json
          :-  %s
          ?^  a
            +.a
          (base58-to-cord a)
        ::
          (ferm include-unsafe.req %b)
        ::
          ?~  query-options.req  ~
          =*  opts  u.query-options.req
          ::  Remove if all query-options are ~
          ::
          =-  ?~(- ~ (pairs -))
          ^-  (list (pair @t json))
          ::  Excludes ~ elements
          ::
          =-  (skip - |=([@t a=json] =(a ~)))
          ^-  (list (pair @t json))
          :~  ['minimumAmount' (feud minimum-amount.opts)]
              ['maximumAmount' (feud maximum-amount.opts)]
              ['minimumCount' (feud maximum-count.opts)]
              ['minimumSumAmount' (feud minimum-sum-amount.opts)]
      ==  ==
    ::
        %list-wallet-dir
      ~
    ::
        %list-wallets
      ~
    ::
        %load-wallet
      ~[s+filename.req]
    ::
        %lock-unspent
      :~  b+unlock.req
        ::
          =-  ?~(- ~ %a^-)
          ?~  transactions.req
            ~
          =*  opts  u.transactions.req
          %+  turn   opts
          |=  [t=@ux v=@ud]
          =-  ?~(- ~ (pairs -))
          ^-  (list (pair @t json))
          ~[['txid' s+(hash-to-cord t)] ['vout' (numb v)]]
      ==
    ::
        %remove-pruned-funds
      ~[s+(hash-to-cord txid.req)]
    ::
        %rescan-blockchain
      :~  (feud start-height.req)
          (feud stop-height.req)
      ==
    ::
        %send-many
      :~  s+dummy.req
        ::
          ?~  amounts.req  ~
          :-  %o
          %-  molt
          %+  turn  amounts.req
          |=  [addr=?(address [%bech32 @t]) amount=@t]
          ^-  [@t json]
          :_  n+amount
          ?^  addr
            +.addr
          (base58-to-cord addr)
        ::
          (feud minconf.req)
          (ferm comment.req %s)
        ::
          ?~  subtract-fee-from.req  ~
          =*  addrs  u.subtract-fee-from.req
          :-  %a
          %+  turn  addrs
          |=  a=?(address [%bech32 @t])
          ^-  json
          :-  %s
          ?^  a
            +.a
          (base58-to-cord a)
        ::
          (ferm replaceable.req %b)
          (feud conf-target.req)
          (ferm mode.req %s)
      ==
    ::
        %send-to-address
      :~  :-  %s
          ?^  address.req
            +.address.req
          (base58-to-cord address.req)
        ::
          n+amount.req
          (ferm comment.req %s)
          (ferm comment-to.req %s)
          (ferm subtract-fee-from-amount.req %b)
          (ferm replaceable.req %b)
          (feud conf-target.req)
          (ferm mode.req %s)
          (ferm avoid-reuse.req %b)
      ==
    ::
        %set-hd-seed
      ~
    ::
        %set-label
      :~  :-  %s
          =*  a  address.req
          ?^  a
            +.a
          (base58-to-cord a)
        ::
          s+label.req
      ==
    ::
        %set-tx-fee
      ~[n+amount.req]
    ::
        %set-wallet-flag
      ~[s+flag.req (ferm value.req %b)]
    ::
        %sign-message
      :~  :-  %s
          =*  a  address.req
          ?^  a
            +.a
          (base58-to-cord a)
        ::
          s+message.req
      ==
    ::
        %sign-raw-transaction-with-wallet
      :~  s+(hex-to-cord hex-string.req)
        ::
          ?~   prev-txs.req
            ~
          =*  txs  u.prev-txs.req
          :-  %a
          %+  turn  ^-  (list prev-tx)  txs
          |=  a=prev-tx
          ^-  json
          (pairs (prev-txs:json-parser a))
        ::
          (ferm sig-hash-type.req %s)
      ==
    ::
        %unload-wallet
      ~[(ferm wallet-name.req %s)]
    ::
        %wallet-create-fundedpsbt
      :~  :-  %a
          %+  turn  inputs.req
          |=  a=input
          ^-  json
          %-  pairs
          :~  ['txid' s+(hash-to-cord txid.a)]
              ['vout' (numb vout.a)]
              ['sequence' (numb sequence.a)]
          ==
        ::
          =*  out  outputs.req
          :-  %a
          %+  weld
          :_  ~
          (pairs [-.data.out s+(hex-to-cord +.data.out)]~)
          %+  turn  addresses.out
          |=  [address=?(@uc [%bech32 @t]) amount=@t]
          ^-  json
          %-  pairs
          :_  ~
          :_  n+amount
          ?^  address
            +.address
          (base58-to-cord address)
        ::
          (feud locktime.req)
        ::
          ?~  options.req
            ~
          =*  opts  u.options.req
          =-  (pairs -)
          =-  (skip - |=([@t a=json] =(a ~)))
          ^-  (list (pair @t json))
          :~  :-  'changeAddress'
              ?~  change-address.opts
                ~
              =*  a  u.change-address.opts
              [%s ?^(a +.a (base58-to-cord a))]
            ::
              ['changePosition' (feud change-position.opts)]
              ['change_type' (ferm change-type.opts %s)]
              ['includeWatching' (ferm include-watching.opts %b)]
              ['lockUnspents' (ferm lock-unspents.opts %b)]
              ['feeRate' (ferm fee-rate.opts %n)]
            ::
              :-  'subtractFeeFromOutputs'
              ?~  subtract-fee-from-outputs.opts
                ~
              a+(turn u.subtract-fee-from-outputs.opts numb)
            ::
              ['replaceable' (ferm replaceable.opts %b)]
              ['conf_target' (feud conf-target.opts)]
              ['estimate_mode' (ferm mode.opts %s)]
          ==
        ::
          (ferm bip32-derivs.req %b)
      ==
    ::
        %wallet-lock
      ~
    ::
        %wallet-passphrase
      ~[s+passphrase.req (numb timeout.req)]
    ::
        %wallet-passphrase-change
      :~  (ferm old-passphrase.req %s)
          (ferm new-passphrase.req %s)
      ==
    ::
        %wallet-process-psbt
      :~  :-  %s
          ?^  (de:base64 psbt.req)
            psbt.req
          (en:base64 (as-octs:mimes:html psbt.req))
        ::
          b+sign.req
          s+sig-hash.req
          (ferm bip32-derivs.req %b)
      ==
    :: ZMQ
    ::
        %get-zmq-notifications
      ~
    ==
  ::
  ++  parse-response
    =,  dejs:format
    |=  res=response:rpc:jstd
    ^-  response
    ~|  -.res
    ::  only deals with successful requests
    ::  ignores (%error, %fails and %batch)
    ::
    ?>  ?=(%result -.res)
    ?+    id.res  ~|  [%unsupported-response id.res]   !!
    ::  Blockchain
    ::
        %get-best-block-hash
      [id.res ((cu to-hex so) res.res)]
    ::
        %get-block
      :-  id.res
      %.  res.res
      ?+  -.res.res  ~|([%format-not-valid -.res.res] !!)
            %s
          (cu to-hex so)
        ::
            %o
          %-  ou
          :~  ['hash' (un (cu to-hex so))]
              ['confirmations' (un ni)]
              ['size' (un ni)]
              ['strippedsize' (un ni)]
              ['weight' (un ni)]
              ['height' (un ni)]
              ['version' (un no)]
              ['versionHex' (un (cu to-hex so))]
              ['merkleroot' (un (cu to-hex so))]
            ::
              :-  'tx'
              =-  (un (ar -))
              |=  =json
              %.  json
              ::  verbosity = 1
              ::
              ?:  =(%s -.json)
                (cu to-hex so)
              ?.  =(%o -.json)
                !!
              ::  verbosity = 2
              ::
              raw-transaction:json-parser
            ::
              ['time' (un ni)]
              ['mediantime' (un ni)]
              ['nonce' (un ni)]
              ['bits' (un (cu to-hex so))]
              ['difficulty' (un no)]
              ['chainwork' (un (cu to-hex so))]
              ['nTx' (un ni)]
              ['previousblockhash' (un (cu to-hex so))]
              ['nextblockhash' (uf ~ (mu (cu to-hex so)))]
      ==  ==
    ::
        %get-blockchain-info
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['chain' (un (cu network-name so))]
          ['blocks' (un ni)]
          ['headers' (un ni)]
          ['bestblockhash' (un (cu to-hex so))]
          ['difficulty' (un no)]
          ['mediantime' (un ni)]
          ['verificationprogress' (un no)]
          ['initialblockdownload' (un bo)]
          ['chainwork' (un (cu to-hex so))]
          ['size_on_disk' (un ni)]
          ['pruned' (un bo)]
          ['pruneheight' (uf ~ (mu ni))]
          ['automatic_pruning' (uf ~ (mu bo))]
          ['prune_target_size' (uf ~ (mu ni))]
        ::
          :-  'softforks'
          =;  softforks
            (un (om (ou softforks)))
          :~  ['type' (un (cu soft-fork-types so))]
            ::
              :-  'bip9'
              =;  bip9
                (uf ~ (mu bip9))
              %-  ou
              :~  ['status' (uf ~ (mu (cu soft-fork-status so)))]
                  ['bit' (uf ~ (mu ni))]
                ::
                  :-  'start_time'
                  =-  (un (cu - no))
                  |=  a=@t
                  ^-  ?(@ud %'-1')
                  ?:  =(a '-1')
                    %'-1'
                  (rash a dem)
                ::
                  ['timeout' (un ni)]
                  ['since' (un ni)]
                ::
                  :-  'statistics'
                  =-  (uf ~ (mu (ot -)))
                  :~  ['period' ni]
                      ['threshold' ni]
                      ['elapsed' ni]
                      ['count' ni]
                      ['possible' bo]
              ==  ==
            ::
              ['height' (uf ~ (mu ni))]
              ['active' (un bo)]
      ==  ==
    ::
        %get-block-count
      [id.res (ni res.res)]
    ::
        %get-block-filter
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['filter' (cu to-hex so)]
          ['header' (cu to-hex so)]
      ==
    ::
        %get-block-hash
      [id.res ((cu to-hex so) res.res)]
    ::
        %get-block-header
      :-  id.res
      ?:  =(%s -.res.res)
        ((cu to-hex so) res.res)
      ?.  =(%o -.res.res)  !!
      %.  res.res
      %-  ou
      :~  ['hash' (un (cu to-hex so))]
          ['confirmations' (un ni)]
          ['height' (un ni)]
          ['version' (un no)]
          ['versionHex' (un (cu to-hex so))]
          ['merkleroot' (un (cu to-hex so))]
          ['time' (un ni)]
          ['mediantime' (un ni)]
          ['nonce' (un ni)]
          ['bits' (un (cu to-hex so))]
          ['difficulty' (un no)]
          ['chainwork' (un (cu to-hex so))]
          ['nTx' (un ni)]
          ['previousblockhash' (un (cu to-hex so))]
          ['nextblockhash' (uf ~ (mu (cu to-hex so)))]
      ==
    ::
        %get-block-stats
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['avgfee' no]
          ['avgfeerate' ni]
          ['avgtxsize' ni]
          ['blockhash' (cu to-hex so)]
        ::
          :-  'feerate_percentiles'
          =-  (cu - (ar no))
          |=  p=(list @t)
          ?>  ?=([@t @t @t @t @t *] p)
          :*  i.p
              i.t.p
              i.t.t.p
              i.t.t.t.p
              i.t.t.t.t.p
          ==
        ::
          ['height' ni]
          ['ins' ni]
          ['maxfee' no]
          ['maxfeerate' no]
          ['maxtxsize' ni]
          ['medianfee' no]
          ['mediantime' ni]
          ['mediantxsize' ni]
          ['minfee' no]
          ['minfeerate' no]
          ['mintxsize' ni]
          ['outs' ni]
          ['subsidy' no]
          ['swtotal_size' ni]
          ['swtotal_weight' ni]
          ['swtxs' ni]
          ['time' ni]
          ['total_out' no]
          ['total_size' ni]
          ['total_weight' no]
          ['totalfee' no]
          ['txs' ni]
          ['utxo_increase' no]
          ['utxo_size_inc' no]
      ==
    ::
        %get-chain-tips
      :-  id.res
      %.  res.res
      =-  (ar (ot -))
      :~  ['height' ni]
          ['hash' (cu to-hex so)]
          ['branchlen' ni]
          ['status' (cu chain-status so)]
      ==
    ::
        %get-chain-tx-stats
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['time' (un ni)]
          ['txcount' (un ni)]
          ['window_final_block_hash' (un (cu to-hex so))]
          ['window_final_block_height' (un ni)]
          ['window_block_count' (un ni)]
          ['window_tx_count' (uf ~ (mu ni))]
          ['window_interval' (uf ~ (mu ni))]
          ['txrate' (uf ~ (mu no))]
      ==
    ::
        %get-difficulty
      [id.res (no res.res)]
    ::
        %get-mempool-ancestors
      :-  id.res
      ?:  =(%a -.res.res)
        %.  res.res
        (ar (cu to-hex so))
      ?.  =(%o -.res.res)  !!
      ::  The parsing rule +hex used in +om
      ::  will give a raw atom so we reparse
      ::  the keys to get a @ux
      ::
      =-  (turn ~(tap by -) |*([a=@ b=*] [`@ux`a b]))
      %.  res.res
      (op hex mem-pool:json-parser)
    ::
        %get-mempool-descendants
      :-  id.res
      ?:  =(%a -.res.res)
        %.  res.res
        (ar (cu to-hex so))
      ?.  =(%o -.res.res)  !!
      ::  The parsing rule +hex used in +om
      ::  will give a raw atom so we reparse
      ::  the keys to get a @ux
      ::
      =-  (turn ~(tap by -) |*([a=@ b=*] [`@ux`a b]))
      %.  res.res
      (op hex mem-pool:json-parser)
    ::
        %get-mempool-entry
      [id.res (mem-pool:json-parser res.res)]
    ::
        %get-mempool-info
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['size' ni]
          ['bytes' ni]
          ['usage' ni]
          ['maxmempool' ni]
          ['mempoolminfee' no]
          ['minrelaytxfee' no]
      ==
    ::
        %get-raw-mempool
      :-  id.res
      ?:  =(%a -.res.res)
        %.  res.res
        (ar (cu to-hex so))
      ?.  =(%o -.res.res)  !!
      ::  The parsing rule +hex used in +om
      ::  will give a raw atom so we reparse
      ::  the keys to get a @ux
      ::
      =-  (turn ~(tap by -) |*([a=@ b=*] [`@ux`a b]))
      %.  res.res
      (op hex mem-pool:json-parser)
    ::
        %get-tx-out
      :-  id.res
      ?~  res.res
        ~
      %-  some
      %.  res.res
      %-  ot
      :~  ['bestblock' (cu to-hex so)]
          ['confirmations' ni]
          ['value' no]
        ::
          :-  'scriptPubKey'
          %-  ou
          :~  ['asm' (un so)]
              ['hex' (un (cu to-hex so))]
              ['reqSigs' (uf ~ (mu ni))]
              ['type' (un so)]
              ['addresses' (uf ~ (mu (ar (cu addr-type-validator so))))]
          ==
        ::
          ['coinbase' bo]
      ==
    ::
        %get-tx-out-proof
      [id.res (so res.res)]
    ::
        %get-tx-outset-info
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['height' ni]
          ['bestblock' (cu to-hex so)]
          ['transactions' ni]
          ['txouts' ni]
          ['bogosize' ni]
          ['hash_serialized_2' (cu to-hex so)]
          ['disk_size' ni]
          ['total_amount' no]
      ==
    ::
        %precious-block
      [id.res ~]
    ::
        %prune-blockchain
      [id.res (ni res.res)]
    ::
        %save-mempool
      [id.res ~]
    ::
        %scan-tx-outset
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['success' (uf ~ (mu bo))]
          ['searched_items' (uf ~ (mu ni))]
          ['txouts' (uf ~ (mu ni))]
          ['height' (uf ~ (mu ni))]
          ['best-blocks' (uf ~ (mu (cu to-hex so)))]
        ::
          :-  'unspents'
          =-  (un (ar (ot -)))
          :~  ['txid' (cu to-hex so)]
              ['vout' ni]
              ['scriptPubKey' (cu to-hex so)]
              ['desc' so]
              ['amount' no]
              ['height' ni]
          ==
        ::
          ['total_amount' (un no)]
      ==
    ::
        %verify-chain
      [id.res (bo res.res)]
    ::
        %verify-tx-out-proof
      :-  id.res
      %.  res.res
      (ar (cu to-hex so))
    ::  Control
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
    ::  Generating
    ::
        %generate
      :-  id.res
      %.  res.res
      (ar (cu to-hex so))
    ::
        %generate-to-address
      :-  id.res
      %.  res.res
      (ar (su hex))
    ::  Mining
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
            ['chain' (cu network-name so)]
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
    ::  Network
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
                  ['connected' (cu ?(%inbound %outbound) so)]
        ==    ==
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
        ==    ==
    ::
        %get-network-info
      :-  id.res
      %.  res.res
      %-  ot
        :~  ['version' ni]
            ['subversion' so]
            ['protocolversion' ni]
            ['localservices' so]
            ['localservicesnames' (ar so)]
            ['localrelay' bo]
            ['timeoffset' no]
            ['connections' ni]
            ['networkactive' bo]
            :-  'networks'
              %-  ar
              %-  ot
                :~  ['name' (cu ?(%ipv4 %ipv6 %onion) so)]
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
            ['servicesnames' (un (ar so))]
            ['relaytxes' (un bo)]
            ['lastsend' (un (cu from-unix:chrono:userlib ni))]
            ['lastrecv' (un (cu from-unix:chrono:userlib ni))]
            ['bytessent' (un ni)]
            ['bytesrecv' (un ni)]
            ['conntime' (un (cu from-unix:chrono:userlib ni))]
            ['timeoffset' (un no)]
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
    ::  Raw Transactions
    ::
        %analyze-psbt
      :-  id.res
      %.  res.res
      %-  ou
      :~  :-  'inputs'
          =-  (un (ar (ou -)))
          :~  ['has_utxo' (un bo)]
              ['is_final' (un bo)]
            ::
              :-  'missing'
              =-  (uf ~ (mu (ou -)))
              :~  :-  'pubkeys'
                  =-  (uf ~ (mu -))
                  (ar (cu to-hex so))
                ::
                  :-  'signatures'
                  =-  (uf ~ (mu -))
                  (ar (cu to-hex so))
                ::
                  ['redeemscript' (uf ~ (mu (cu to-hex so)))]
                  ['witnessscript' (uf ~ (mu (cu to-hex so)))]
              ==
            ::
              ['next' (uf ~ (mu so))]
          ==
        ::
          ['estimated_vsize' (uf ~ (mu no))]
          ['estimated_feerate' (uf ~ (mu no))]
          ['fee' (uf ~ (mu no))]
          ['next' (un so)]
      ==
    ::
        %combine-psbt
      [id.res (so res.res)]
    ::
        %combine-raw-transaction
      [id.res ((cu to-hex so) res.res)]
    ::
        %convert-to-psbt
      [id.res (so res.res)]
    ::
        %create-psbt
      [id.res (so res.res)]
    ::
        %create-raw-transaction
      [id.res ((cu to-hex so) res.res)]
    ::
        %decode-psbt
      :-  id.res
      %.  res.res
      %-  ou
      :~  :-  'tx'
          =-  (un (ot -))
          =,  json-parser
          :~  ['txid' (cu to-hex so)]
              ['hash' (cu to-hex so)]
              ['size' ni]
              ['vsize' ni]
              ['weight' ni]
              ['version' ni]
              ['locktime' ni]
              ['vin' vin]
              ['vout' vout]
          ==
        ::
          ['unknown' (un (om so))]
        ::
          :-  'inputs'
          =-  (un (ar (ou -)))
          =,  json-parser
          :~  ['non_witness_utxo' utxo]
              ['witness_utxo' utxo]
            ::
              :-  'partial_signatures'
              =-  (uf ~ (mu -))
              (op hex (cu to-hex so))
            ::
              ['sighash' (uf ~ (mu (cu sig-hash so)))]
              ['redeem_script' script]
              ['witness_script' script]
            ::
              :-  'bip32_derivs'
              =-  (uf ~ (mu -))
              =-  (op hex (ot -))
              :~  ['master_fingerprint' so]
                  ['path' so]
              ==
            ::
              :-  'final_scriptsig'
              =-  (uf ~ (mu (ot -)))
              :~  ['asm' so]
                  ['hex' (cu to-hex so)]
              ==
            ::
              :-  'final_scriptwitness'
              =-  (uf ~ (mu -))
              (ar (cu to-hex so))
            ::
              ['unknown' (uf ~ (mu (om so)))]
          ==
        ::
          :-  'outputs'
          =-  (un (ar (ou -)))
          =,  json-parser
          :~  ['redeem_script' script]
              ['witness_script' script]
            ::
              :-  'bip32_derivs'
              =-  (uf ~ (mu -))
              =-  (op hex (ot -))
              :~  ['master_fingerprint' so]
                  ['path' so]
              ==
            ::
              ['unknown' (uf ~ (mu (om so)))]
          ==
        ::
          ['fee' (uf ~ (mu no))]
      ==
    ::
        %decode-raw-transaction
      :-  id.res
      %.  res.res
      %-  ot
      =,  json-parser
      :~  ['txid' (cu to-hex so)]
          ['hash' (cu to-hex so)]
          ['size' ni]
          ['vsize' ni]
          ['weight' ni]
          ['version' ni]
          ['locktime' ni]
          ['vin' vin]
          ['vout' vout]
      ==
    ::
        %decode-script
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['asm' (un so)]
          ['hex' (uf ~ (mu (cu to-hex so)))]
          ['type' (un so)]
          ['reqSigs' (uf ~ (mu ni))]
        ::
          :-  'addresses'
          =-  (uf ~ (mu (ar -)))
          (cu addr-type-validator so)
        ::
          :-  'p2sh'
          (uf ~ (mu (cu @uc so)))
        ::
          :-  'segwit'
          =-  (uf ~ (mu (ou -)))
          :~  ['asm' (un so)]
              ['hex' (uf ~ (mu (cu to-hex so)))]
              ['type' (un so)]
              ['reqSigs' (uf ~ (mu ni))]
            ::
              :-  'addresses'
              =-  (un (ar -))
              (cu |=(a=@t bech32+a) so)
            ::
              :-  'p2sh-segwit'
              =-  (uf ~ (mu -))
              (cu @uc (su fim:ag))
      ==  ==
    ::
        %finalize-psbt
      :-  id.res
      %.  res.res
      %-  ou
      :~  :-  'psbt'
          =-  (uf ~ (mu (cu - so)))
          |=(a=@t ?>(?=(^ (de:base64 a)) a))
        ::
          ['hex' (uf ~ (mu (cu to-hex so)))]
          ['complete' (un bo)]
      ==
    ::
        %fund-raw-transaction
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['hex' (cu to-hex so)]
          ['fee' no]
        ::
          :-  'changepos'
          =-  (cu - no)
          |=  a=@t
          ^-  ?(@ud %'-1')
          ?:  =(a '-1')
            %'-1'
          (rash a dem)
      ==
    ::
        %get-raw-transaction
      :-  id.res
      %.  res.res
      ?:  =(%s -.res.res)
        (cu to-hex so)
      ?.  =(%o -.res.res)
        !!
      raw-transaction:json-parser
    ::
        %join-psbts
      :-  id.res
      %.  res.res
      =-  (cu - so)
      |=(a=@t ?>(?=(^ (de:base64 a)) a))
    ::
        %send-raw-transaction
      [id.res ((cu to-hex so) res.res)]
    ::
        %sign-raw-transaction-with-key
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['hex' (un (cu to-hex so))]
          ['complete' (un bo)]
        ::
          :-  'errors'
          =-  (uf ~ (mu -))
          =-  (ar (ot -))
          :~  ['txid' (cu to-hex so)]
              ['vout' ni]
              ['scriptSig' (cu to-hex so)]
              ['sequence' ni]
              ['error' so]
      ==  ==
    ::
        %test-mempool-accept
      :-  id.res
      %.  res.res
      =-  (ar (ou -))
      :~  ['txid' (un (cu to-hex so))]
          ['allowed' (un bo)]
          ['reject-reason' (uf ~ (mu so))]
      ==
    ::
        %utxo-update-psbt
      :-  id.res
      %.  res.res
      =-  (cu - so)
      |=(a=@t ?>(?=(^ (de:base64 a)) a))
    ::  Util
    ::
        %create-multi-sig
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['address' (cu addr-type-validator so)]
          ['redeemScript' so]
      ==
    ::
        %derive-addresses
      :-  id.res
      %.  res.res
      (ar (cu addr-type-validator so))
    ::
        %estimate-smart-fee
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['feerate' (uf ~ (mu no))]
          ['errors' (uf ~ (mu (ar so)))]
          ['blocks' (un ni)]
      ==
    ::
        %get-descriptor-info
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['descriptor' so]
          ['checksum' so]
          ['isrange' bo]
          ['issolvable' bo]
          ['hasprivatekeys' bo]
      ==
    ::
        %sign-message-with-privkey
      [id.res (so res.res)]
    ::
        %validate-address
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['isvalid' (un bo)]
          ['address' (uf ~ (mu (cu addr-type-validator so)))]
          ['scriptPubKey' (uf ~ (mu (cu to-hex so)))]
          ['isscript' (uf ~ (mu bo))]
          ['iswitness' (uf ~ (mu bo))]
          ['witness_version' (uf ~ (mu so))]
          ['witness_program' (uf ~ (mu (cu to-hex so)))]
      ==
    ::
        %verify-message
      [id.res (bo res.res)]
    ::  Wallet
    ::
        %abandon-transaction
      [id.res ~]
    ::
        %abort-rescan
      [id.res ~]
    ::
        %add-multisig-address
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['address' (cu addr-type-validator so)]
        ::
          ['redeemScript' so]
      ==
    ::
        %backup-wallet
      [id.res ~]
    ::
        %bump-fee
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['txid' (cu to-hex so)]
          ['origfee' no]
          ['fee' no]
          ['errors' (ar so)]
      ==
    ::
        %create-wallet
      :-  id.res
      %.  res.res
      (ot ~[name+so warning+so])
    ::
        %dump-privkey
      [id.res (so res.res)]
    ::
        %dump-wallet
      :-  id.res
      %.  res.res
      (ot [filename+so]~)
    ::
        %encrypt-wallet
      [id.res ~]
    ::
        %get-addresses-by-label
      :-  id.res
      ::  Transforms the keys encoded as tapes to ?(@uc [%bech32 @t])
      ::
      =-  %+  turn   ~(tap by -)
      |*  [k=tape v=*]
      :_  v
      ^-  ?(@uc [%bech32 @t])
      (addr-type-validator (crip k))
      %.  res.res
      =-  (op (star aln) -)
      (ot ['purpose' (cu purpose so)]~)
    ::
        %get-address-info
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['address' (un (cu addr-type-validator so))]
          ['scriptPubKey' (un (cu to-hex so))]
          ['ismine' (un bo)]
          ['iswatchonly' (un bo)]
          ['solvable' (un bo)]
          ['desc' (uf ~ (mu so))]
          ['isscript' (un bo)]
          ['ischange' (un bo)]
          ['iswitness' (un bo)]
          ['witness_version' (uf ~ (mu no))]
          ['witness_program' (uf ~ (mu (cu to-hex so)))]
          ['script' (uf ~ (mu so))]
          ['hex' (uf ~ (mu (cu to-hex so)))]
        ::
          :-  'pubkeys'
          =-  (uf ~ (mu -))
          (ar (cu to-hex so))
        ::
          ['sigsrequired' (uf ~ (mu ni))]
          ['pubkey' (uf ~ (mu (cu to-hex so)))]
        ::
          :-  'embedded'
          =-  (uf ~ (mu -))
          %-  ou
          :~  ['scriptPubKey' (un (cu to-hex so))]
              ['solvable' (uf ~ (mu bo))]
              ['desc' (uf ~ (mu so))]
              ['isscript' (un bo)]
              ['ischange' (uf ~ (mu bo))]
              ['iswitness' (un bo)]
              ['witness_version' (uf ~ (mu no))]
              ['witness_program' (uf ~ (mu (cu to-hex so)))]
              ['script' (uf ~ (mu (cu to-hex so)))]
              ['hex' (uf ~ (mu (cu to-hex so)))]
            ::
              :-  'pubkeys'
              =-  (uf ~ (mu -))
              (ar (cu to-hex so))
            ::
              ['sigsrequired' (uf ~ (mu ni))]
              ['pubkey' (uf ~ (mu (cu to-hex so)))]
              ['iscompressed' (uf ~ (mu bo))]
              ['label' (uf ~ (mu so))]
              ['hdmasterfingerprint' (uf ~ (mu (cu to-hex so)))]
            ::
              :-  'labels'
              =-  (uf ~ (mu -))
              =-  (ar (ot -))
              :~  ['name' so]
                  ['purpose' (cu purpose so)]
          ==  ==
        ::
          ['iscompressed' (uf ~ (mu bo))]
          ['label' (uf ~ (mu so))]
          ['timestamp' (uf ~ (mu ni))]
          ['hdkeypath' (uf ~ (mu so))]
          ['hdseedid' (uf ~ (mu (cu to-hex so)))]
          ['hdmasterfingerprint' (uf ~ (mu (cu to-hex so)))]
        ::
          :-  'labels'
          =-  (un (ar (ot -)))
          :~  ['name' so]
              ['purpose' (cu purpose so)]
      ==  ==
    ::
        %get-balance
      [id.res (no res.res)]
    ::
        %get-balances
      :-  id.res
      %.  res.res
      %-  ou
      :~  :-  'mine'
          =;  mine
            (un (ou mine))
          :~  ['trusted' (uf ~ (mu no))]
              ['untrusted_pending' (un no)]
              ['immature' (un no)]
              ['used' (uf ~ (mu no))]
          ==
        ::
          :-  'watchonly'
          =;  watchonly
            (uf ~ (mu (ot watchonly)))
          :~  ['trusted' no]
              ['untrusted_pending' no]
              ['immature' no]
      ==  ==
    ::
        %get-new-address
      :-  id.res
      ^-  ?(@uc [%bech32 @t])
      %.  res.res
      (cu addr-type-validator so)
    ::
        %get-raw-change-address
      :-  id.res
      %.  res.res
      (cu addr-type-validator so)
    ::
        %get-received-by-address
      [id.res (no res.res)]
    ::
        %get-received-by-label
      [id.res (no res.res)]
    ::
        %get-transaction
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['amount' (un no)]
          ['fee' (uf ~ (mu no))]
          ['confirmations' (un ni)]
          ['blockhash' (uf ~ (mu (cu to-hex so)))]
          ['blockindex' (uf ~ (mu ni))]
          ['blocktime' (uf ~ (mu ni))]
          ['txid' (un (cu to-hex so))]
          ['time' (un ni)]
          ['timereceived' (un ni)]
          ['bip125-replaceable' (un (cu bip125-replaceable so))]
        ::
          :-  'details'
          =-  (un (ar (ou -)))
          :~  ['address' (uf ~ (mu (cu addr-type-validator so)))]
              ['category' (un (cu category so))]
              ['amount' (un no)]
              ['label' (uf ~ (mu so))]
              ['vout' (un ni)]
              ['fee' (uf ~ (mu no))]
              ['abandoned' (uf ~ (mu bo))]
          ==
        ::
          ['hex' (un (cu to-hex so))]
          ['decoded' (uf ~ (mu raw-transaction:json-parser))]
      ==
    ::
        %get-unconfirmed-balance
      [id.res (no res.res)]
    ::
        %get-wallet-info
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['walletname' (un so)]
          ['walletversion' (un ni)]
          ['balance' (un no)]
          ['unconfirmed_balance' (un no)]
          ['immature_balance' (un no)]
          ['txcount' (un ni)]
          ['keypoololdest' (un ni)]
          ['keypoolsize' (un ni)]
          ['keypool_size_hd_internal' (uf ~ (mu ni))]
          ['unlocked_until' (uf ~ (mu ni))]
          ['paytxfee' (un no)]
          ['hdseedid' (uf ~ (mu (cu to-hex so)))]
          ['private_keys_enabled' (un bo)]
          ['avoid_reuse' (un bo)]
          :-  'scanning'
          %-  un
          |=  =json
          %.  json
          ?:  =(%b -.json)
            bo
          ?.  =(%o -.json)
            !!
          (ot ~[['duration' no] ['progress' no]])
      ==
    ::
        %list-wallets
      [id.res ((ar so) res.res)]
    ::
        %import-address
      [id.res ~]
    ::
        %import-multi
      :-  id.res
      %.  res.res
      =-  (ar (ou -))
      :~  ['success' (un bo)]
          ['warnings' (uf ~ (mu (ar so)))]
        ::
          :-  'errors'
          =-  (uf ~ (mu -))
          (ot ~[['error' so] ['message' so]])
      ==
    ::
        %import-privkey
      [id.res ~]
    ::
        %import-pruned-funds
      [id.res ~]
    ::
        %import-pubkey
      [id.res ~]
    ::
        %import-wallet
      [id.res ~]
    ::
        %key-pool-refill
      [id.res ~]
    ::
        %list-address-groupings
      :-  id.res
      %.  res.res
      =-  (ar (ar -))
      =-  (cu groups (ar -))
      |=  =json
      %.  json
      ?:  =(%s -.json)
        so
      ?.  =(%n -.json)
        !!
      no
    ::
        %list-labels
      [id.res ((ar so) res.res)]
    ::
        %list-lock-unspent
      :-  id.res
      %.  res.res
      %-  ar
      (ot ~[['txid' (cu to-hex so)] ['vout' ni]])
    ::
        %list-received-by-address
      :-  id.res
      %.  res.res
      =-  (ar (ou -))
      :~  :-  'involvesWatchonly'
          =-  (uf ~ (cu - (mu bo)))
          |=  b=(unit ?)
          ?~  b  ~
          ?>(=(u.b &) (some %&))
        ::
          ['address' (un (cu addr-type-validator so))]
          ['amount' (un no)]
          ['confirmations' (un ni)]
          ['label' (un so)]
          ['txids' (un (ar (cu to-hex so)))]
      ==
    ::
        %list-received-by-label
      :-  id.res
      %.  res.res
      =-  (ar (ou -))
      :~  :-  'involvesWatchonly'
          =-  (uf ~ (cu - (mu bo)))
          |=  b=(unit ?)
          ?~  b  ~
          ?>(=(u.b &) (some %&))
        ::
          ['amount' (un no)]
          ['confirmations' (un ni)]
          ['label' (un so)]
      ==
    ::
        %lists-in-ceblock
      :-  id.res
      %.  res.res
      %-  ou
      =,  json-parser
      :~  ['transactions' (un tx-in-block)]
          ['removed' (uf ~ (mu tx-in-block))]
          ['lastblock' (un (cu to-hex so))]
      ==
    ::
        %list-transactions
      [id.res (tx-in-block:json-parser res.res)]
    ::
        %list-unspent
      :-  id.res
      %.  res.res
      =-  (ar (ou -))
      :~  ['txid' (un (cu to-hex so))]
          ['vout' (un ni)]
          ['address' (un (cu addr-type-validator so))]
          ['label' (un so)]
          ['scriptPubKey' (un (cu to-hex so))]
          ['amount' (un no)]
          ['confirmations' (un ni)]
          ['redeemScript' (un (cu to-hex so))]
          ['witnessScript' (uf ~ (mu (cu to-hex so)))]
          ['spendable' (un bo)]
          ['solvable' (un bo)]
          ['reused' (uf ~ (mu bo))]
          ['desc' (uf ~ (mu so))]
          ['safe' (un bo)]
      ==
    ::
        %list-wallet-dir
      :-  id.res
      %.  res.res
      =-  (ot ['wallets' -]~)
      (ar (ot [name+so]~))
    ::
        %list-wallets
      [id.res ((ar so) res.res)]
    ::
        %load-wallet
      :-  id.res
      %.  res.res
      (ot ~[name+so warning+so])
    ::
        %lock-unspent
      [id.res (bo res.res)]
    ::
        %remove-pruned-funds
      [id.res ~]
    ::
        %rescan-blockchain
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['start_height' ni]
          ['stop_height' ni]
      ==
    ::
        %send-many
      :-  id.res
      %.  res.res
      (cu to-hex so)
    ::
        %send-to-address
      :-  id.res
      %.  res.res
      (cu to-hex so)
    ::
        %set-hd-seed
      [id.res ~]
    ::
        %set-label
      [id.res ~]
    ::
        %set-tx-fee
      [id.res (bo res.res)]
    ::
        %set-wallet-flag
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['flag_name' so]
          ['flag_state' bo]
          ['warnings' so]
      ==
    ::
        %sign-message
      [id.res (so res.res)]
    ::
        %sign-raw-transaction-with-wallet
      :-  id.res
      %.  res.res
      %-  ou
      :~  ['hex' (un (cu to-hex so))]
          ['complete' (un bo)]
        ::
          :-  'errors'
          =-  (uf ~ (mu (ar (ot -))))
          :~  ['txid' (cu to-hex so)]
              ['vout' ni]
              ['scriptSig' (cu to-hex so)]
              ['sequence' ni]
              ['error' so]
      ==  ==
    ::
        %unload-wallet
      [id.res ~]
    ::
        %wallet-create-fundedpsbt
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['psbt' so]
          ['fee' no]
        ::
          =-  ['changepos' (cu - no)]
          |=  a=@t
          ^-  ?(@ud %'-1')
          ?:  =(a '-1')
            %'-1'
          (rash a dem)
      ==
    ::
        %wallet-lock
      [id.res ~]
    ::
        %wallet-passphrase
      [id.res ~]
    ::
        %wallet-passphrase-change
      [id.res ~]
    ::
        %wallet-process-psbt
      :-  id.res
      %.  res.res
      %-  ot
      :~  ['psbt' so]
          ['complete' bo]
      ==
    ::  ZMQ
    ::
        %get-zmq-notifications
      :-  id.res
      %.  res.res
      =-  (ar (ot -))
      :~  ['type' so]
          ['address' (cu addr-type-validator so)]
          ['hwm' ni]
      ==
    ==
  --
--
