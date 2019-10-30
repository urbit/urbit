/-  *btc-node-hook
/+  *test, lib=btc-node-json
::
=/  addr     '1GdK9UzpHBzqzX2A9JFP3Di4weBwqgmoQA'
=/  en-addr=@uc  (to-base58:btc-rpc:lib addr)
::
=,  format
::
|%
::  Wallet
::
++  test-abandon-transaction  ^-  tang
  =/  op  %abandon-transaction
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-abort-rescan  ^-  tang
  =/  op  %abort-rescan
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-add-multisig-address  ^-  tang
  =/  op  %add-multisig-address
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    %-  pairs:enjs
      :~  ['address' s+addr]
          ['redeemScript' s+'23']
      ==
  =/  exp=response:btc-rpc  [op en-addr '23']
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
++  test-backup-wallet  ^-  tang
  =/  op  %backup-wallet
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-bump-fee  ^-  tang
  =/  op  %bump-fee
  =/  expected-res=response:btc-rpc
    [op txid=0x23 orig-fee='23' fee='23' errors=['23']~]
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    %-  pairs:enjs
    ^-  (list (pair @t json))
    :~  ['txid' s+'23']
        ['origfee' n+~.23]
        ['fee' n+~.23]
        ['errors' a+[s+'23']~]
    ==
  %+  expect-eq
    !>  expected-res
    !>  (parse-response:btc-rpc:lib result)
::
++  test-create-wallet  ^-  tang
  =/  op  %create-wallet
  =/  expected-res=response:btc-rpc
    [op name='23' warning='']
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    :-  %o  %-  ~(gas by *(map @t json))
    ^-  (list (pair @t json))
    :~  ['name' s+'23']
        ['warning' s+'']
    ==
  %+  expect-eq
    !>  expected-res
    !>  (parse-response:btc-rpc:lib result)
::
++  test-dump-privkey  ^-  tang
  =/  op  %dump-privkey
  =/  result=response:rpc:jstd  [%result op s+'23']
  %+  expect-eq
    !>  [op '23']
    !>  (parse-response:btc-rpc:lib result)
::
++  test-dump-wallet  ^-  tang
  =/  op  %dump-wallet
  =/  expected-res=response:btc-rpc
    [op filename='23']
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    :-  %o  %-  ~(gas by *(map @t json))
    ^-  (list (pair @t json))
    ['filename' s+'23']~
  %+  expect-eq
    !>  expected-res
    !>  (parse-response:btc-rpc:lib result)
::
++  test-encrypt-wallet  ^-  tang
  =/  op  %encrypt-wallet
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-addresses-by-label  ^-  tang
  =/  op  %get-addresses-by-label
  =/  result=response:rpc:jstd
    :+  %result  op
    %-  pairs:enjs
      :~  :-  addr
          %-  pairs:enjs
            ['purpose' s+'send']~
      ==
  =/  exp=response:btc-rpc  [op [en-addr %send]~]
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-address-info  ^-  tang
  =/  op  %get-address-info
  =/  label-result
    =-  ['labels' [%a -]]
    :~  %-  pairs:enjs
        :~  ['name' s+'label1']
            ['purpose' s+'send']
    ==  ==
  =/  result=response:rpc:jstd
    :+  %result  op
    %-  pairs:enjs
      :~  ['address' s+addr]
          ['scriptPubKey' s+'23']
          ['ismine' b+&]
          ['iswatchonly' b+&]
          ['solvable' b+&]
          ['desc' s+'23']
          ['isscript' b+&]
          ['ischange' b+&]
          ['iswitness' b+&]
          ['witness_version' s+'23']
          ['witness_program' s+'23']
          ['script' s+'23']
          ['hex' s+'23']
          ['pubkeys' a+[s+'23']~]
          ['sigsrequired' n+~.2]
          ['pubkey' s+'23']
          :-  'embedded'
          %-  pairs:enjs
            :~  ['scriptPubKey' s+'23']
                ['solvable' b+&]
                ['desc' s+'23']
                ['isscript' b+&]
                ['ischange' b+&]
                ['iswitness' b+&]
                ['witness_version' s+'23']
                ['witness_program' s+'23']
                ['script' s+'23']
                ['hex' s+'23']
                ['pubkeys' a+[s+'23']~]
                ['sigsrequired' n+~.2]
                ['pubkey' s+'23']
                ['iscompressed' b+&]
                ['label' s+'23']
                ['hdmasterfingerprint' s+'23']
                label-result
            ==
          ['iscompressed' b+&]
          ['label' s+'23']
          ['timestamp' s+'23']
          ['hdkeypath' s+'23']
          ['hdseedid' s+'23']
          ['hdmasterfingerprint' s+'23']
          label-result
      ==
  =/  embedded
    :*  0x23  &  (some '23')  &  &  &
        (some '23')  (some 0x23)  (some 0x23)  (some 0x23)
        (some [0x23]~)  (some 2)  (some 0x23)  (some &)
        (some '23')  (some 0x23)  ['label1' %send]~
    ==
  =/  exp=response:btc-rpc
    :*  op  en-addr  0x23  &  &  &  (some '23')  &  &  &
        (some '23')  (some 0x23)  (some '23')  (some 0x23)
        (some [0x23]~)  (some 2)  (some 0x23)  (some embedded)
        (some &)  (some '23')  (some '23')  (some '23')
        (some 0x23)  (some 0x23)
        ['label1' %send]~
    ==
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-balance  ^-  tang
  =/  op  %get-balance
  =/  result=response:rpc:jstd  [%result op n+~.23]
  %+  expect-eq
    !>  [op '23']
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-new-address  ^-  tang
  =/  op  %get-new-address
  =/  result=response:rpc:jstd  [%result op s+addr]
  %+  expect-eq
    !>  [op en-addr]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-raw-change-address  ^-  tang
  =/  op  %get-raw-change-address
  =/  result=response:rpc:jstd  [%result op s+addr]
  %+  expect-eq
    !>  [op en-addr]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-received-by-address  ^-  tang
  =/  op  %get-received-by-address
  =/  result=response:rpc:jstd  [%result op s+'23']
  %+  expect-eq
    !>  [op '23']
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-received-by-label  ^-  tang
  =/  op  %get-received-by-label
  =/  result=response:rpc:jstd  [%result op s+'23']
  %+  expect-eq
    !>  [op '23']
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-transaction  ^-  tang
  =/  op  %get-transaction
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    %-  pairs:enjs
    :~  ['amount' n+'23']
        ['fee' n+'23']
        ['confirmations' n+~.23]
        ['blockhash' s+'23']
        ['blockindex' n+~.23]
        ['blocktime' n+~.23]
        ['txid' s+'23']
        ['time' n+~.23]
        ['timereceived' n+~.23]
        ['bip125-replaceable' s+'yes']
        :-  'details'  ^-  json
        :-  %a  ^-  (list json)
        :~  %-  pairs:enjs
            :~  ['address' s+addr]
                ['category' s+'send']
                ['amount' n+'23.23']
                ['label' s+'23']
                ['vout' n+~.23]
                ['fee' n+'23']
                ['abandoned' b+|]
        ==  ==
        ['hex' s+'23']
    ==
  =/  exp=response:btc-rpc
    :*  op  '23'  '23'  23  0x23  23
        23  0x23  23  23  %yes
        [en-addr %send '23.23' '23' 23 '23' %.n]~
        0x23
    ==
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-unconfirmed-balance  ^-  tang
  =/  op  %get-unconfirmed-balance
  =/  result=response:rpc:jstd  [%result op s+'99']
  %+  expect-eq
    !>  [op '99']
    !>  (parse-response:btc-rpc:lib result)
::
++  test-get-wallet-info  ^-  tang
  =/  op  %get-wallet-info
  =/  result=response:rpc:jstd
    :+  %result  op
    %-  pairs:enjs
      :~  ['walletname' s+'23']
          ['walletversion' n+~.23]
          ['balance' n+'23.23']
          ['unconfirmed_balance' n+'23']
          ['immature_balance' n+'23']
          ['txcount' n+~.23]
          ['keypoololdest' n+~.23]
          ['keypoolsize' n+~.23]
          ['keypool_size_hd_internal' n+~.23]
          ['unlocked_until' n+~.23]
          ['paytxfee' n+'23.23']
          ['hdseedid' s+'23']
          ['private_keys_enabled' b+&]
      ==
  =/  exp=response:btc-rpc
    :*  op  '23'  23  '23.23'  '23'  '23'  23  23
       23  (some 23)  (some 23)  '23.23'  (some 0x23)  &
    ==
  %+  expect-eq
      !>  exp
      !>  (parse-response:btc-rpc:lib result)
::
++  test-import-address  ^-  tang
  =/  op  %import-address
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-import-multi  ^-  tang
  =/  op  %import-multi
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    :-  %a  ^-  (list json)
    :~  %-  pairs:enjs
        :~  ['success' b+|]
            ['warnings' a+[s+'warning1']~]
            :-  'errors'
            %-  pairs:enjs
            :~  ['error' s+'1']
                ['message' s+'sms']
            ==
    ==  ==
  =/  exp=response:btc-rpc
    :-  op
    [| (some ['warning1']~) (some ['1' 'sms'])]~
  %+  expect-eq
      !>  exp
      !>  (parse-response:btc-rpc:lib result)
::
++  test-import-privkey  ^-  tang
  =/  op  %import-privkey
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-import-pruned-funds  ^-  tang
  =/  op  %import-pruned-funds
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-import-pubkey  ^-  tang
  =/  op  %import-pubkey
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-import-wallet  ^-  tang
  =/  op  %import-wallet
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-key-pool-refill  ^-  tang
  =/  op  %key-pool-refill
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-list-address-groupings  ^-  tang
  =/  op  %list-address-groupings
  =/  result=response:rpc:jstd
    :+  %result  op
    :-  %a
    [%a ~[s+addr s+'amount' s+'label']]~
  =/  exp=response:btc-rpc
    :-  op
    :~  :~  [en-addr 'amount' (some 'label')]
    ==  ==
  %+  expect-eq
      !>  exp
      !>  (parse-response:btc-rpc:lib result)
::
++  test-list-labels  ^-  tang
  =/  op  %list-labels
  =/  result=response:rpc:jstd
    :+  %result   op
    :-  %a  ^-  (list json)
    ~[[s+'l1'] [s+'l2']]
  =/  exp=response:btc-rpc  [op ~['l1' 'l2']]
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
++  test-list-lock-unspent  ^-  tang
  =/  op  %list-lock-unspent
  =/  result=response:rpc:jstd
    :+  %result   op
    :-  %a  ^-  (list json)
    :~  %-  pairs:enjs
        ~[['txid' s+'23'] ['vout' n+~.23]]
    ==
  %+  expect-eq
    !>  [op [0x23 23]~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-list-received-by-address  ^-  tang
  =/  op  %list-received-by-address
  =/  result=response:rpc:jstd
    :+  %result  op
    :-  %a  ^-  (list json)
    :~  %-  pairs:enjs
        :~  ['involvesWatchonly' b+&]
            ['address' s+addr]
            ['amount' n+'23']
            ['confirmations' n+~.23]
            ['label' s+'23']
            ['txids' a+[s+'23']~]
    ==  ==
  =/  exp=response:btc-rpc
    [op [(some %&) en-addr '23' 23 '23' [0x23]~]~]
  %+  expect-eq
      !>  exp
      !>  (parse-response:btc-rpc:lib result)
::
++  test-list-received-by-label  ^-  tang
  =/  op  %list-received-by-label
  =/  result=response:rpc:jstd
    :+  %result  op
    :-  %a  ^-  (list json)
    :~  %-  pairs:enjs
        :~  ['involvesWatchonly' b+&]
            ['amount' n+'23.23']
            ['confirmations' n+~.23]
            ['label' s+'23']
    ==  ==
  =/  exp=response:btc-rpc
    [op [(some %&) '23.23' 23 '23']~]
  %+  expect-eq
      !>  exp
      !>  (parse-response:btc-rpc:lib result)
::
++  test-lists-in-ceblock  ^-  tang
  =/  op  %lists-in-ceblock
  =/  tx-response
    :~  ['address' s+addr]
        ['category' s+'send']
        ['amount' n+'23.23']
        ['label' s+'23']
        ['vout' n+~.23]
        ['fee' n+'23.23']
        ['confirmations' n+~.23]
        ['blockhash' s+'23']
        ['blockindex' n+~.23]
        ['blocktime' n+~.23]
        ['txid' s+'23']
        ['time' n+~.23]
        ['timereceived' n+~.23]
        ['bip125-replaceable' s+'yes']
        ['abandoned' b+&]
        ['comment' s+'23']
        ['label' s+'23']
        ['to' s+'23']
    ==
  =/  result=response:rpc:jstd
    :+  %result  op
    %-  pairs:enjs
      :~  =-  ['transactions' -]
          ^-  json
          :-  %a  ^-  (list json)
          :~  %-  pairs:enjs  ^-  (list (pair @t json))
              tx-response
          ==
          =-  ['removed' -]
          [%a [(pairs:enjs tx-response)]~]
          ['lastblock' s+'23']
      ==
  =/  exp-tx-response
    :*  address=en-addr
        category=%send
        amount='23.23'
        label='23'
        vout=23
        fee='23.23'
        confirmations=23
        blockhash=0x23
        blockindex=23
        blocktime=23
        txid=0x23
        time=23
        time-received=23
        bip125-replaceable=%yes
        abandoned=&
        comment='23'
        label='23'
        to='23'
    ==
  =/  exp=response:btc-rpc
    :*  op
        [exp-tx-response]~
        (some [exp-tx-response]~)
       0x23
    ==
  %+  expect-eq
      !>  exp
      !>  (parse-response:btc-rpc:lib result)
::
++  test-list-transactions  ^-  tang
  =/  op  %list-transactions
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    :-  %a  ^-  (list json)
    :~  %-  pairs:enjs
        :~  ['address' s+addr]
            ['category' s+'send']
            ['amount' n+'23']
            ['label' s+'23']
            ['vout' n+~.23]
            ['fee' n+'23']
            ['confirmations' n+~.23]
            ['trusted' b+|]
            ['blockhash' s+'23']
            ['blockindex' n+~.23]
            ['blocktime' n+~.23]
            ['txid' s+'23']
            ['time' n+~.23]
            ['timereceived' n+~.23]
            ['comment' s+'23']
            ['bip125-replaceable' s+'yes']
            ['abandoned' b+|]
    ==  ==
  =/  exp=response:btc-rpc
    :-  op
    :_  ~
    :*  en-addr  %send  '23'  '23'  23  '23'  23  %.n
        0x23  23  23  0x23  23  23  '23'  %yes  %.n
    ==
  %+  expect-eq
      !>  exp
      !>  (parse-response:btc-rpc:lib result)
::
++  test-list-unspent  ^-  tang
  =/  op  %list-unspent
  =/  result=response:rpc:jstd
    :+  %result   op
    :-  %a  ^-  (list json)
    :~  %-  pairs:enjs
        :~  ['txid' s+'23']
            ['vout' n+~.23]
            ['address' s+addr]
            ['label' s+'23']
            ['scriptPubKey' s+'23']
            ['amount' n+'23']
            ['confirmations' n+~.23]
            ['redeemScript' s+'23']
            ['witnessScript' s+'23']
            ['spendable' b+|]
            ['solvable' b+|]
            ['desc' s+'23']
            ['safe' b+|]
    ==  ==
  =/  exp=response:btc-rpc
    :-  op
    [0x23 23 en-addr '23' 0x23 '23' 23 0x23 0x23 %.n %.n (some '23') %.n]~
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
++  test-list-wallet-dir  ^-  tang
    =/  op  %list-wallet-dir
    =/  expected-res=response:btc-rpc
      [op [name='test-wallet']~]
    =/  result=response:rpc:jstd
      :+  %result  op
      ^-  json
      :-  %o  %-  ~(gas by *(map @t json))
      ^-  (list (pair @t json))
      :~  :-  'wallets'
          :-  %a  ^-  (list json)
          :~  %-  pairs:enjs
              ['name' s+'test-wallet']~
      ==  ==
    %+  expect-eq
      !>  expected-res
      !>  (parse-response:btc-rpc:lib result)
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
  =/  expected-res=response:btc-rpc
    [op name='23' warning='']
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    :-  %o  %-  ~(gas by *(map @t json))
    ^-  (list (pair @t json))
    :~  ['name' s+'23']
        ['warning' s+'']
    ==
  %+  expect-eq
    !>  expected-res
    !>  (parse-response:btc-rpc:lib result)
::
++  test-lock-unspent  ^-  tang
  =/  op  %lock-unspent
  =/  expected-res=response:btc-rpc  [op &]
  =/  result=response:rpc:jstd  [%result op b+&]
  %+  expect-eq
    !>  expected-res
    !>  (parse-response:btc-rpc:lib result)
::
++  test-remove-pruned-funds  ^-  tang
  =/  op  %remove-pruned-funds
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-rescan-blockchain  ^-  tang
  =/  op  %rescan-blockchain
  =/  expected-res=response:btc-rpc
    [op start-height=20 stop-height=30]
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    :-  %o  %-  ~(gas by *(map @t json))
    ^-  (list (pair @t json))
    :~  ['start_height' n+~.20]
        ['stop_height' n+~.30]
    ==
  %+  expect-eq
    !>  expected-res
    !>  (parse-response:btc-rpc:lib result)
::
++  test-send-many  ^-  tang
  =/  op  %send-many
  =/  result=response:rpc:jstd  [%result op s+'99']
  %+  expect-eq
    !>  [op 0x99]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-send-to-address  ^-  tang
  =/  op  %send-to-address
  =/  result=response:rpc:jstd  [%result op s+'99']
  %+  expect-eq
    !>  [op 0x99]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-set-hd-seed  ^-  tang
  =/  op  %set-hd-seed
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-set-label  ^-  tang
  =/  op  %set-label
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-set-tx-fee    ^-  tang
  =/  op  %set-tx-fee
  =/  result=response:rpc:jstd  [%result op b+&]
  %+  expect-eq
    !>  [op &]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-sign-message  ^-  tang
  =/  op  %sign-message
  =/  result=response:rpc:jstd  [%result op s+'99']
  %+  expect-eq
    !>  [op '99']
    !>  (parse-response:btc-rpc:lib result)
::
++  test-sign-raw-transaction-with-wallet  ^-  tang
  =/  op  %sign-raw-transaction-with-wallet
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    %-  pairs:enjs
    :~  ['hex' s+'34']
        ['complete' b+|]
        :-  'errors'  ^-  json
        :-  %a  ^-  (list json)
        :~  %-  pairs:enjs
            :~  ['txid' s+'34']
                ['vout' n+~.34]
                ['scriptSig' s+'34']
                ['sequence' n+~.34]
                ['error' s+'34']
    ==  ==  ==
  =/  exp=response:btc-rpc
    [op 0x34 | [0x34 34 0x34 34 '34']~]
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
++  test-unload-wallet  ^-  tang
  =/  op  %unload-wallet
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-wallet-create-fundedpsbt  ^-  tang
  =/  op  %wallet-create-fundedpsbt
  =/  result=response:rpc:jstd
    :+  %result  op
    %-  pairs:enjs
    :~  ['psbt' s+'99']
        ['fee' n+'20.1']
        ['changepos' n+~.-1]
    ==
  =/  exp=response:btc-rpc  [op '99' '20.1' %'-1']
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
++  test-wallet-lock  ^-  tang
  =/  op  %wallet-lock
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-wallet-passphrase  ^-  tang
  =/  op  %wallet-passphrase
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-wallet-passphrase-change  ^-  tang
  =/  op  %wallet-passphrase-change
  =/  result=response:rpc:jstd  [%result op ~]
  %+  expect-eq
    !>  [op ~]
    !>  (parse-response:btc-rpc:lib result)
::
++  test-wallet-process-psbt  ^-  tang
  =/  op  %wallet-process-psbt
  =/  expected-res=response:btc-rpc  [op psbt='99' complete=&]
  =/  result=response:rpc:jstd
    :+  %result  op
    ^-  json
    :-  %o  %-  ~(gas by *(map @t json))
    ^-  (list (pair @t json))
    :~  ['psbt' s+'99']
        ['complete' b+&]
    ==
  %+  expect-eq
    !>  expected-res
    !>  (parse-response:btc-rpc:lib result)
::  ZMQ
::
++  test-get-zmq-notifications  ^-  tang
  =/  op  %get-zmq-notifications
  =/  result=response:rpc:jstd
    :+  %result  op
    :-  %a  ^-  (list json)
    :~  %-  pairs:enjs
        :~  ['type' s+'23']
            ['address' s+addr]
            ['hwm' n+~.23]
    ==  ==
  =/  exp=response:btc-rpc  [op ['23' en-addr 23]~]
  %+  expect-eq
    !>  exp
    !>  (parse-response:btc-rpc:lib result)
::
--
