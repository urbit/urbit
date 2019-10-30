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
                ['req-sigs' (uf ~ (mu ni))]
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
        %-  ot
        :~  ['size' ni]
            ['fee' no]
            ['modifiedfee' no]
            ['time' ni]
            ['height' ni]
            ['descendantcount' ni]
            ['descendantsize' ni]
            ['descendantfees' no]
            ['ancestorcount' ni]
            ['ancestorsize' ni]
            ['ancestorfees' no]
            ['wtxid' (cu to-hex so)]
          ::
            :-  'fees'
            %-  ot
            :~  ['base' no]
                ['modified' no]
                ['ancestor' no]
                ['descendant' no]
            ==
          ::
            ['depends' (ar (cu to-hex so))]
            ['spentby' (ar (cu to-hex so))]
            ['bip125-replaceable' bo]
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
        :~  ['txid' s+(hex-to-cord txid.t)]
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
    ?+    -.req  ~|([%unsupported-request -.req] !!)
    ::  Wallet
    ::
        %abandon-transaction
      ~[s+(hex-to-cord txid.req)]
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
      :~  s+(hex-to-cord txid.req)
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
              ['replaceable' (ferm replaceable.opts %b)]
              ['estimate_mode' (ferm mode.opts %s)]
      ==  ==
    ::
        %create-wallet
      :~  s+name.req
          (ferm disable-private-keys.req %b)
          (ferm blank.req %b)
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
      ==
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
      ~[s+(hex-to-cord txid.req) (ferm include-watch-only.req %b)]
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
          s+(hex-to-cord tx-out-proof.req)
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
          s+(hex-to-cord u.blockhash.req)
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
          ~[['txid' s+(hex-to-cord t)] ['vout' (numb v)]]
      ==
    ::
        %remove-pruned-funds
      ~[s+(hex-to-cord txid.req)]
    ::
        %rescan-blockchain
      :~  (feud start-height.req)
        ::
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
          :~  ['txid' s+(hex-to-cord txid.a)]
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
