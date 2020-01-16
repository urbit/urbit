::   This is a placeholder test template for the RPC calls.
::   TODO: expand unit tests and add missing cases
::
/-  *btc-node-hook
/+  *test, lib=btc-node-json
::
=/  en-addr=@uc  0c1GdK9UzpHBzqzX2A9JFP3Di4weBwqgmoQA
=/  addr=@t      (base58-to-cord:btc-rpc:lib en-addr)
=/  en-txid=@ux  0x1234.1234
=/  txid=@t      (hex-to-cord:btc-rpc:lib en-txid)
::
=,  format
::
|%
::  Others
::
++  test-get-block-count  ^-  tang
  =/  op  %get-block-count
  =/  action=request:btc-rpc  [op ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-blockchain-info  ^-  tang
  =/  op  %get-blockchain-info
  =/  action=request:btc-rpc  [op ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::  Wallet
::
++  test-abandon-transaction  ^-  tang
  =/  op  %abandon-transaction
  =/  action=request:btc-rpc  [op en-txid]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+txid]~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-abort-rescan  ^-  tang
  =/  op  %abort-rescan
  =/  action=request:btc-rpc  [op ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-add-multisig-address  ^-  tang
  =/  op  %add-multisig-address
  =/  action=request:btc-rpc
    [op 2 [en-addr]~ ~ %bech32]
  =/  exp
    :^  op   (method:btc-rpc:lib op)   %list
    ~[n+~.2 a+[s+addr]~ ~ s+'bech32']
  %+  expect-eq
    !>  exp
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-backup-wallet  ^-  tang
  =/  op  %backup-wallet
  =/  action=request:btc-rpc  [op 'XXXX']
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+'XXXX']~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-bump-fee  ^-  tang
  =/  op  %bump-fee
  ;:  weld
    =/  action=request:btc-rpc  [op en-txid ~]
    %+  expect-eq
      !>  [op (method:btc-rpc:lib op) %list ~[[%s '12341234'] [%o ~]]]
      !>  (request-to-rpc:btc-rpc:lib action)
  ::
    =/  action=request:btc-rpc
      [op en-txid `[`23 `'23' `& `%'ECONOMICAL']]
    =/  exp
      :^  op   (method:btc-rpc:lib op)  %list
          :~  s+txid
              ^-  json
              =-  (pairs:enjs -)
              :~  ['confTarget' n+'23']
                  ['totalFee' n+~.23]
                  ['replaceable' b+&]
                  ['estimate_mode' s+'ECONOMICAL']
          ==  ==
    %+  expect-eq
      !>  exp
      !>  (request-to-rpc:btc-rpc:lib action)
  ==
::
++  test-create-wallet  ^-  tang
  =/  op  %create-wallet
  =/  action=request:btc-rpc
    [op name='test-create-wallet' disable-private-keys=`| blank=`|]
    %+  expect-eq
      !>  :^  op   (method:btc-rpc:lib op)  %list
          ~[s+'test-create-wallet' b+| b+|]
      !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-dump-privkey  ^-  tang
  =/  op  %dump-privkey
  =/  action=request:btc-rpc  [op en-addr]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+addr]~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-dump-wallet  ^-  tang
  =/  op  %dump-wallet
  =/  action=request:btc-rpc  [op filename=*@t]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+'']~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-encrypt-wallet  ^-  tang
  =/  op  %encrypt-wallet
  =/  action=request:btc-rpc  [op passphrase=*@t]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+'']~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-addresses-by-label  ^-  tang
  =/  op  %get-addresses-by-label
  =/  action=request:btc-rpc  [op label=*@t]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+'']~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-address-info  ^-  tang
  =/  op  %get-address-info
  =/  action=request:btc-rpc  [op en-addr]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+addr]~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-balance  ^-  tang
  =/  op  %get-balance
  =/  action=request:btc-rpc  [op ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-new-address  ^-  tang
  =/  op  %get-new-address
  =/  action=request:btc-rpc
    [op ~ ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-raw-change-address  ^-  tang
  =/  op  %get-raw-change-address
  =/  action=request:btc-rpc
    [op address-type=*(unit address-type)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-received-by-address  ^-  tang
  =/  op  %get-received-by-address
  =/  action=request:btc-rpc
    [op en-addr minconf=*@ud]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[s+addr n+(scot %ud *@ud)]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-received-by-label  ^-  tang
  =/  op  %get-received-by-label
  =/  action=request:btc-rpc  [op *@t ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%s p=''] ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-transaction  ^-  tang
  =/  op  %get-transaction
  =/  action  [op en-txid *(unit ?)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%s p='12341234'] ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-unconfirmed-balance  ^-  tang
  =/  op  %get-unconfirmed-balance
  =/  action=request:btc-rpc  [op ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-get-wallet-info  ^-  tang
  =/  op  %get-wallet-info
  =/  action=request:btc-rpc  [op wallet='']
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-import-address  ^-  tang
  =/  op  %import-address
  =/  action=request:btc-rpc
    [op [%addr en-addr] label=*(unit @t) rescan=*(unit ?) p2sh=*(unit ?)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%s addr] ~ ~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-import-multi  ^-  tang
  =/  op  %import-multi
  =/  action=request:btc-rpc  [op requests=~ options=~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-import-privkey  ^-  tang
  =/  op  %import-privkey
  =/  action=request:btc-rpc
    [op privkey=*@t label=*(unit @t) rescan=*(unit ?)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%s p=''] ~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-import-pruned-funds  ^-  tang
  =/  op  %import-pruned-funds
  =/  action=request:btc-rpc
    [op raw-transaction=*@ux tx-out-proof=*@ux]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[s+'' s+'']]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-import-pubkey  ^-  tang
  =/  op  %import-pubkey
  =/  action=request:btc-rpc
    [op pubkey=0x1234.1234 label=*(unit @t) rescan=*(unit ?)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%s p='12341234'] ~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-import-wallet  ^-  tang
  =/  op  %import-wallet
  =/  action=request:btc-rpc  [op filename=*@t]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+'']~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-key-pool-refill  ^-  tang
  =/  op  %key-pool-refill
  =/  action=request:btc-rpc  [op new-size=*(unit @ud)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-list-address-groupings  ^-  tang
  =/  op  %list-address-groupings
  =/  action=request:btc-rpc  [op ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-list-labels  ^-  tang
  =/  op  %list-labels
  =/  action=request:btc-rpc  [op purpose=*(unit purpose)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-list-lock-unspent  ^-  tang
  =/  op  %list-lock-unspent
  =/  action=request:btc-rpc  [op ~]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-list-received-by-address  ^-  tang
  =/  op  %list-received-by-address
  =/  action=request:btc-rpc

    :-  op
    %-  some  :*
        minconf=*(unit @ud)
        include-empty=*(unit ?)
        include-watch-only=*(unit ?)
        address-filter=~
    ==
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~ ~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-list-received-by-label  ^-  tang
  =/  op  %list-received-by-label
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib [op ~])
::
++  test-lists-in-ceblock  ^-  tang
  =/  op  %lists-in-ceblock
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib [op ~])
::
++  test-list-transactions  ^-  tang
  =/  op  %list-transactions
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib [op ~])
::
++  test-list-unspent  ^-  tang
  =/  query-options  %-  some
  :*  minimum-amount=*(unit @ud)
      maximum-amount=*(unit @ud)
      maximum-count=*(unit @ud)
      minimum-sum-amount=*(unit @ud)
  ==
  =/  op  %list-unspent
  =/  action=request:btc-rpc
    :-  op
    %-  some  :*
        minconf=*(unit @ud)
        maxconf=*(unit @ud)
        addresses=~
        include-unsafe=*(unit ?)
        query-options=query-options
    ==
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~ ~ ~ ~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-list-wallet-dir  ^-  tang
    =/  op  %list-wallet-dir
    =/  action=request:btc-rpc  [op ~]
    %+  expect-eq
      !>  [op (method:btc-rpc:lib op) %list ~]
      !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-list-wallets     ^-  tang
    =/  op  %list-wallets
    =/  action=request:btc-rpc  [op ~]
    %+  expect-eq
      !>  [op (method:btc-rpc:lib op) %list ~]
      !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-load-wallet      ^-  tang
  =/  op  %load-wallet
  =/  action=request:btc-rpc  [op filename=*@t]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+'']~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-lock-unspent  ^-  tang
  =/  op  %lock-unspent
  =/  action=request:btc-rpc
    [op unlock=*? transactions=*(unit (list [txid=@ux vout=@ud]))]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%b p=%.y] ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-remove-pruned-funds  ^-  tang
  =/  op  %remove-pruned-funds
  =/  action=request:btc-rpc  [op txid=en-txid]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list [s+txid]~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-rescan-blockchain  ^-  tang
  =/  op  %rescan-blockchain
  =/  action=request:btc-rpc
    [op start-height=*(unit @ud) stop-height=*(unit @ud)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-send-many  ^-  tang
  =/  op  %send-many
  =/  action=request:btc-rpc
    :*  op
        dummy=%''
        amounts=~
        minconf=~
        comment=~
        subtract-fee-from=~
        send-many=~
        conf-target=~
        estimate-mode=~
    ==
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%s p=''] ~ ~ ~ ~ ~ ~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-send-to-address  ^-  tang
  =/  op  %send-to-address
  =/  action=request:btc-rpc
    :*  op
        en-addr
        amount='23.23'
        comment=~
        comment-to=~
        subtract-fee-from-amount=~
        replaceable=~
        conf-target=~
        estimate-mode=~
    ==
  %+  expect-eq
    !>  :*  op   (method:btc-rpc:lib op)   %list
            ~[[%s addr] [%n ~.23.23] ~ ~ ~ ~ ~ ~]
        ==
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-set-hd-seed   ^-  tang
    =/  op  %set-hd-seed
    =/  action=request:btc-rpc  [op ~]
    %+  expect-eq
      !>  [op (method:btc-rpc:lib op) %list ~]
      !>  (request-to-rpc:btc-rpc:lib action)
  ::
++  test-set-label     ^-  tang
  =/  op  %set-label
  =/  action=request:btc-rpc  [op en-addr label=*@t]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[s+addr s+'']]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-set-tx-fee    ^-  tang
  =/  op  %set-tx-fee
  =/  action=request:btc-rpc  [op amount=*@t]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%n p=~.]]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-sign-message  ^-  tang
  =/  op  %sign-message
  =/  action=request:btc-rpc  [op en-addr message=*@t]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[s+addr s+'']]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-sign-raw-transaction-with-wallet  ^-  tang
  =/  op  %sign-raw-transaction-with-wallet
  =/  action=request:btc-rpc
    :*  op
        en-txid
        ~
        `%'ALL'
    ==
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[s+txid ~ s+'ALL']]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-unload-wallet  ^-  tang
  =/  op  %unload-wallet
  =/  action=request:btc-rpc
    [op wallet-name=*(unit @t)]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-wallet-create-fundedpsbt  ^-  tang
  =/  op  %wallet-create-fundedpsbt
  =/  action=request:btc-rpc
    :*  op
        [en-txid 2 2]~
        :-  [%data *@ux]
        [en-addr '23.23']~
        `23
        %-  some
          :*  change-address=`en-addr
              change-position=`23
              change-type=`%bech32
              include-watching=`&
              lock-unspents=`&
              fee-rate=`'23'
              subtract-fee-from-outputs=`[23]~
              replaceable=`&
              conf-target=`23
              `%'UNSET'
          ==
        `&
    ==
  =/  exp=(list json)
    :~  =-  a+[(pairs:enjs -)]~
        ~[['txid' s+txid] ['vout' n+~.2] ['sequence' n+~.2]]
        :-  %a
        :~  (pairs:enjs ['data' s+'']~)
            :-  %o
            %-  molt  ^-  (list [@t json])
            [addr n+~.23.23]~
        ==
        n+~.23
        =-  (pairs:enjs -)
        :~  ['feeRate' [%n ~.23]]
            ['subtractFeeFromOutputs' [%a ~[[%n ~.23]]]]
            ['conf_target' [%n ~.23]]
            ['replaceable' [%b %.y]]
            ['lockUnspents' [%b %.y]]
            ['change_type' [%s 'bech32']]
            ['changeAddress' [%s '1GdK9UzpHBzqzX2A9JFP3Di4weBwqgmoQA']]
            ['estimate_mode' [%s 'UNSET']]
            ['includeWatching' [%b %.y]]
            ['changePosition' [%n ~.23]]
        ==
        b+&
    ==
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list exp]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-wallet-lock  ^-  tang
  =/  op  %wallet-lock
  =/  action=request:btc-rpc  [op ~]
   %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-wallet-passphrase  ^-  tang
  =/  op  %wallet-passphrase
  =/  action=request:btc-rpc
    [op passphrase=*@t timeout=*@ud]
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[s+'' n+(scot %ud *@ud)]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-wallet-passphrase-change  ^-  tang
  =/  op  %wallet-passphrase-change
  =/  action=request:btc-rpc
    :+  op
    old-passphrase=*(unit @t)  new-passphrase=*(unit @t)
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[~ ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::
++  test-wallet-process-psbt  ^-  tang
  =/  op  %wallet-process-psbt
  =/  action=request:btc-rpc
    :*  op
        psbt=''
        sign=&
        sig-hash-type=%'ALL'
        bip32derivs=~
    ==
  %+  expect-eq
    !>  [op (method:btc-rpc:lib op) %list ~[[%s ''] [%b %.y] [%s 'ALL'] ~]]
    !>  (request-to-rpc:btc-rpc:lib action)
::  ZMQ
::
++  test-get-zmq-notifications  ^-  tang
    =/  op  %get-zmq-notifications
    =/  action=request:btc-rpc  [op ~]
    %+  expect-eq
      !>  [op (method:btc-rpc:lib op) %list ~]
      !>  (request-to-rpc:btc-rpc:lib action)
::
--
