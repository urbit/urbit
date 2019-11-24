=>  ::  Helper types
    ::
    |%
    ::  +address: base58check encoded public key (20 bytes)
    ::
    ::  FIXME: %bech32 is not parsed by fim:ag
    ::
    +$  address    ?(@uc [%bech32 @t])
    ++  blockhash  @ux
    ::  $estimate-mode
    ::
    ::    Used in:
    ::      - %fund-raw-transaction
    ::      - %estimate-smart-fee
    ::      - %bump-fee
    ::      - %wallet-create-fundedpsbt
    ::      - %send-to-address
    ::      - %send-many
    ::
    +$  estimate-mode  ?(%'UNSET' %'ECONOMICAL' %'CONSERVATIVE')
    ::  $category:
    ::
    ::    Used in:
    ::      - $tx-in-block
    ::      - %get-transaction
    ::      - %list-transactions
    ::
    +$  category  ?(%send %receive %generate %immature %orphan)
    ::  $chain-status:
    ::
    ::    Used in:
    ::      - %get-chain-tips
    ::
    +$  chain-status
      $?  %invalid
          %headers-only
          %valid-headers
          %valid-fork
          %active
      ==
    ::  $soft-fork-status:
    ::
    ::    Used in:
    ::      - %get-blockchain-info/$bip9-softforks
    ::
    +$  soft-fork-status  ?(%defined %started %'locked_in' %active %failed)
    ::  $purpose:
    ::
    ::    Used in:
    ::      - %get-addresses-by-label
    ::      - %get-address-info
    ::
    +$  purpose  ?(%send %receive)
    ::  $address-type:
    ::
    ::    Used in:
    ::      - %add-multisig-address
    ::
    +$  address-type  ?(%legacy %p2sh-segwit %bech32)
    ::  $bip125-replaceable:
    ::
    ::    Used in:
    ::      - %get-transaction
    ::      - %list-transactions
    ::      - $tx-in-block
    ::
    +$  bip125-replaceable  ?(%yes %no %unknown)
    ::  $network-name:
    ::
    ::    Used in:
    ::      - %get-blockchain-info
    ::
    +$  network-name  ?(%main %test %regtest)
    ::  $sig-hash:
    ::
    ::    Used in:
    ::      - %wallet-process-psbt
    ::      - %sign-raw-transaction-with-wallet
    ::      - %sign-raw-transaction-with-key
    ::      - %decode-psbt
    ::
    +$  sig-hash
      $?  %'NONE'
          %'SINGLE'
          %'ALL|ANYONECANPAY'
          %'NONE|ANYONECANPAY'
          %'SINGLE|ANYONECANPAY'
          %'ALL'
      ==
    ::  $script:
    ::
    ::    Used in:
    ::      - %decode-psbt: $redeem-script/$witness-script
    ::
    +$  script  [asm=@t hex=@ux type=@t]
    ::  $script-pubkey:
    ::
    ::    Used in:
    ::      - $utxo
    ::
    +$  script-pubkey
      $:  asm=@t
          hex=@ux
          type=@t
          =address
      ==
    ::  $range:
    ::
    ::    Used in:
    ::      - %derive-addresses
    ::      - $scan-object
    ::
    +$  range  ?(@ud [@ud @ud])
    ::  $vin:
    ::
    ::    Used in:
    ::      - $serialized-tx
    ::      - $raw-transaction-rpc-out
    ::
    +$  vin
      $:  txid=(unit @ux)
          vout=(unit @ud)
          script-sig=(unit [asm=@t hex=@ux])
          tx-in-witness=(unit (list @ux))
          sequence=@ud
      ==
    ::  $vout:
    ::
    ::    Used in:
    ::      - $serialized-tx
    ::      - $raw-transaction-rpc-out
    ::
    +$  vout
      $:  value=@t
          n=@ud
        ::
          $=  script-pubkey
          $:  asm=@t
              hex=@ux
              req-sigs=(unit @ud)
              type=@t
              addresses=(unit (list address))
      ==  ==
    ::  $input:
    ::
    ::    Used in:
    ::      - %wallet-create-fundedpsbt
    ::      - $partially-signed-transaction
    ::
    +$  input
      $:  txid=@ux
          vout=@ud
          sequence=@ud
      ==
    ::  $output:
    ::
    ::    Used in:
    ::      - %wallet-create-fundedpsbt
    ::      - $partially-signed-transaction
    ::
    +$  output
      $:  data=[%data @ux]
          addresses=(list [=address amount=@t])
      ==
    ::  $scan-object:
    ::
    ::    Used in:
    ::      - %scan-tx-outset
    ::
    +$  scan-object
      $?  descriptor=@t
        ::
          $=  object
          $:  desc=@t
              range=(unit range)
      ==  ==
    ::  $utxo:
    ::
    ::    Used in:
    ::      - %decode-psbt
    ::
    +$  utxo
      (unit [amount=@t =script-pubkey])
    ::  $segwit-script:
    ::
    ::    Used in:
    ::      - %decode-script
    ::
    +$  segwit-script
      $:  asm=@t
          hex=(unit @ux)
          type=@t
          req-sigs=(unit @ud)
          addresses=(list [%bech32 @t])
          p2sh-segwit=(unit @uc)
      ==
    ::  $partially-signed-transaction:
    ::
    ::    Used in:
    ::      - %create-psbt
    ::      - %create-raw-transaction
    ::
    +$  partially-signed-transaction
      $:  inputs=(list input)
          outputs=output
          locktime=(unit @ud)
          replaceable=(unit ?)
      ==
    ::  $import-request:
    ::
    ::    Used in:
    ::      - %import-multi
    ::
    +$  import-request
      $:  desc=(unit @t)
        ::
          $=  script-pubkey
          $%  [%script s=@t]
              [%address a=address]
          ==
        ::
          timestamp=?(@da %now)
          redeem-script=(unit @t)
          witness-script=(unit @t)
          pubkeys=(unit (list @t))
          keys=(unit (list @t))
          range=(unit ?(@ud [@ud @ud]))
          internal=(unit ?)
          watchonly=(unit ?)
          label=(unit @t)
          keypool=(unit ?)
      ==
    ::  $serialized-tx:
    ::
    ::    Used in:
    ::      - %decode-psbt
    ::      - %decode-raw-transaction
    ::
    +$  serialized-tx
      $:  txid=@ux
          hash=@ux
          size=@ud
          vsize=@ud
          weight=@ud
          version=@ud
          locktime=@ud
          vin=(list vin)
          vout=(list vout)
      ==
    ::  $prev-tx:
    ::
    ::    Used in:
    ::      - %sign-raw-transaction-with-wallet
    ::      - %sign-raw-transaction-with-key
    ::
    +$  prev-tx
      $:  txid=@ux
          vout=@ud
          script-pubkey=@ux
          redeem-script=(unit @ux)
          witness-script=(unit @ux)
          amount=@t
      ==
    ::  $raw-transaction-rpc-out:
    ::
    ::    Used in:
    ::      - %get-block
    ::      - %get-raw-transaction
    ::
    +$  raw-transaction-rpc-out
      $:  in-active-chain=(unit ?)
          hex=@ux
          txid=@ux
          hash=@ux
          size=@ud
          vsize=@ud
          weight=@ud
          version=@t
          locktime=@ud
          vin=(list vin)
          vout=(list vout)
          blockhash=(unit @ux)
          confirmations=(unit @ud)
          blocktime=(unit @ud)
          time=(unit @ud)
      ==
    ::  $tx-in-block:
    ::
    ::    Used in:
    ::      - %lists-in-ceblock
    ::      - %list-transactions
    ::
    +$  tx-in-block
      $:  address=(unit address)
          =category
          amount=@t
          label=(unit @t)
          vout=@ud
          fee=(unit @t)
          confirmations=@ud
          blockhash=(unit @ux)
          blockindex=(unit @ud)
          blocktime=(unit @ud)
          txid=@ux
          time=@ud
          time-received=(unit @ud)
          wallet-conflicts=(unit (list @ux))
          =bip125-replaceable
          abandoned=(unit ?)
          comment=(unit @t)
          to=(unit @t)
      ==
    ::  $errors:
    ::
    ::    Used in:
    ::      - %sign-raw-transaction-with-key
    ::      - %sign-raw-transaction-with-wallet
    ::
    +$  errors
      %-  list
      $:  txid=@
          vout=@
          script-sig=@
          sequence=@
          error=@t
      ==
    ::  $mem-pool:
    ::
    ::    Used in:
    ::      - $mem-pool-response
    ::      - %get-mempool-entry
    ::
    +$  mem-pool
      $:  size=@ud
          fee=@t
          modified-fee=@t
          time=@ud
          height=@ud
          descendant-count=@ud
          descendant-size=@ud
          descendant-fees=@t
          ancestor-count=@ud
          ancestor-size=@ud
          ancestor-fees=@t
          w-txid=@ux
        ::
          $=  fees
          $:  base=@t
              modified=@t
              ancestor=@t
              descendant=@t
          ==
        ::
          depends=(list @ux)
          spent-by=(list @ux)
          bip125-replaceable=?
      ==
    ::  $mem-pool-response:
    ::
    ::    Used in:
    ::      - %get-raw-mempool
    ::      - %get-mempool-entry
    ::      - %get-mempool-ancestors
    ::      - %get-mempool-descendants
    ::      - %get-raw-mempool
    ::
    +$  mem-pool-response
      %-  list
      $@  ::  (for verbose = false)
          ::
          @ux
        ::
          ::  (for verbose = true)
          ::
          [id=@ux =mem-pool]
      ::
    --
|%
::
+$  btc-node-hook-action    request:btc-rpc
+$  btc-node-hook-response  response:btc-rpc
+$  btc-node-hook-command   command:btc-rpc
::
++  btc-rpc
  |%
  +$  request
    $%
    ::  Control
    ::
        [%help command=(unit @t)]
    ::  Generating
    ::
        [%generate blocks=@ud max-tries=(unit @ud)]
    ::  Raw Transactions
    ::
        ::  %analyze-psbt: Analyzes and provides information about
        ::  the current status of a PSBT and its inputs
        ::
        [%analyze-psbt psbt=@t]
        ::  %combine-psbt: Combine multiple partially signed Bitcoin
        ::  transactions into one transaction.
        ::
        [%combine-psbt txs=(list @t)]
        ::  %combine-raw-transaction: Combine multiple partially signed t
        ::  ransactions into one transaction.
        ::
        [%combine-raw-transaction txs=(list @ux)]
        ::  %convert-to-psbt: Converts a network serialized transaction to a
        ::  PSBT.
        ::
        $:  %convert-to-psbt
            hex-string=@ux
            permit-sig-data=(unit ?)
            is-witness=(unit ?)
        ==
        ::  %create-psbt: Creates a transaction in the Partially Signed
        ::  Transaction format.
        ::
        [%create-psbt partially-signed-transaction]
        ::  %create-raw-transaction: Create a transaction spending the given
        ::  inputs and creating new outputs.
        ::
        [%create-raw-transaction partially-signed-transaction]
        ::  %decode-psbt: Return a JSON object representing the serialized,
        ::  base64-encoded partially signed Bitcoin transaction.
        ::
        [%decode-psbt psbt=@t]
        ::  %decode-raw-transaction: Return a JSON object representing the
        ::  serialized, hex-encoded transaction.
        ::
        [%decode-raw-transaction hex-string=@ux is-witness=?]
        ::  %decode-script: Decode a hex-encoded script.
        ::
        [%decode-script hex-string=@ux]
        ::  %finalize-psbt: Finalize the inputs of a PSBT.
        ::
        [%finalize-psbt psbt=@t extract=(unit ?)]
        ::  %fund-raw-transaction: Add inputs to a transaction until it has
        ::  enough in value to meet its out value.
        ::
        $:  %fund-raw-transaction
            hex-string=@ux
          ::
            $=  options
            %-  unit
            $:  change-address=(unit address)
                change-position=(unit @ud)
                change-type=(unit address-type)
                include-watching=(unit ?)
                lock-unspents=(unit ?)
                fee-rate=(unit @t)
                subtract-fee-from-outputs=(unit (list @ud))
                replaceable=(unit ?)
                conf-target=(unit @ud)
                mode=(unit estimate-mode)
            ==
          ::
            is-witness=?
        ==
        ::  %get-raw-transaction: Return the raw transaction data.
        ::
        [%get-raw-transaction txid=@ux verbose=(unit ?) blockhash=(unit @ux)]
        ::  %join-psbts: Joins multiple distinct PSBTs with different inputs
        ::  and outputs into one PSBT with inputs and outputs from all of
        ::  the PSBTs
        ::
        [%join-psbts txs=(list @t)]
        ::  %send-raw-transaction: Submits raw transaction
        ::  (serialized, hex-encoded) to local node and network.
        ::
        [%send-raw-transaction hex-string=@ux allow-high-fees=?]
        ::  %sign-raw-transaction-with-key: Sign inputs for raw transaction
        ::  (serialized, hex-encoded).
        ::
        $:  %sign-raw-transaction-with-key
            hex-string=@ux
            priv-keys=(list @t)
            prev-txs=(unit (list prev-tx))
            sig-hash-type=(unit sig-hash)
        ==
        ::  %test-mempool-accept: Returns result of mempool acceptance tests
        ::  indicating if raw transaction (serialized, hex-encoded) would be
        ::  accepted by mempool.
        ::
        [%test-mempool-accept raw-txs=(list @ux) allow-high-fees=?]
        ::  %utxo-update-psbt: Updates a PSBT with witness UTXOs retrieved from
        ::  the UTXO set or the mempool.
        ::
        [%utxo-update-psbt psbt=@t]
    ::  Util
    ::
        ::  %create-multi-sig: Creates a multi-signature address with n
        ::  signature of m keys required. It returns a json object with the
        ::  address and redeemScript.
        ::
        $:  %create-multi-sig
            n-required=@ud
            keys=(list @ux)
            address-type=(unit address-type)
        ==
        ::  %derive-addresses: Derives one or more addresses corresponding to
        ::  an output descriptor.
        ::
        [%derive-addresses descriptor=@t range=(unit range)]
        ::  %estimate-smart-fee: Estimates the approximate fee per kilobyte
        ::  needed for a transaction to begin confirmation within conf_target
        ::  blocks if possible and return the number of blocks for which the
        ::  estimate is valid.
        ::
        [%estimate-smart-fee conf-target=@ud mode=(unit estimate-mode)]
        ::  %get-descriptor-info: Analyses a descriptor.
        ::
        [%get-descriptor-info descriptor=@t]
        ::  %sign-message-with-privkey: Sign a message with the private key of
        ::  an address
        ::
        [%sign-message-with-privkey privkey=@t message=@t]
        ::  %validate-address: Return information about the given
        ::  bitcoin address.
        ::
        [%validate-address =address]
        ::  %verify-message: Verify a signed message
        ::
        [%verify-message =address signature=@t message=@t]
    ::  Wallet
    ::
        ::  %abandon-transaction: Mark in-wallet transaction as abandoned.
        ::
        [%abandon-transaction txid=@ux]
        ::  %abort-rescan: Stops current wallet rescan triggered by an
        ::  RPC call, e.g. by an importprivkey call.
        ::
        [%abort-rescan ~]
        ::  %add-multisig-address: Add a nrequired-to-sign multisignature
        ::  address to the wallet.
        ::
        $:  %add-multisig-address
            n-required=@ud
            keys=(list address)
            label=(unit @t)
            =address-type
        ==
        ::  %backupwallet: Safely copies current wallet file to destination.
        ::
        [%backup-wallet destination=@t]
        ::  %bump-fee: Bumps the fee of an opt-in-RBF transaction T, replacing
        ::  it with a new transaction B.
        ::
        $:  %bump-fee
            txid=@ux
          ::
            $=  options
            %-  unit
            $:  conf-target=(unit @ud)
                total-fee=(unit @t)
                replaceable=(unit ?)
                mode=(unit estimate-mode)
        ==  ==
        ::  %create-wallet: Creates and loads a new wallet.
        ::
        ::    - %name: The name for the new wallet.
        ::    - %disable-private-keys: Disable the possibility of private keys
        ::      (only watchonlys are possible in this mode).
        ::    - %blank: Create a blank wallet.
        ::      A blank wallet has no keys or HD seed.
        ::      One can be set using sethdseed.
        ::
        [%create-wallet name=@t disable-private-keys=(unit ?) blank=(unit ?)]
        ::  %dump-privkey: Reveals the private key corresponding to 'address'.
        ::
        [%dump-privkey =address]
        ::  %dump-wallet: Dumps all wallet keys in a human-readable format to
        ::  a server-side file.
        ::
        [%dump-wallet filename=@t]
        ::  %encrypt-wallet: Encrypts the wallet with 'passphrase'.
        ::
        [%encrypt-wallet passphrase=@t]
        ::  %get-addresses-by-label: Returns the list of addresses assigned the
        ::  specified label.
        ::
        [%get-addresses-by-label label=@t]
        ::  %get-address-info: Return information about the given bitcoin
        ::  address.
        ::
        [%get-address-info =address]
        ::  %get-balance: Returns the total available balance.
        ::
        $:  %get-balance
            %-  unit
            $:  dummy=(unit %'*')
                minconf=(unit @ud)
                include-watch-only=(unit ?)
        ==  ==
        ::  %get-new-address: Returns a new Bitcoin address for receiving
        ::  payments.
        ::
        [%get-new-address label=(unit @t) address-type=(unit address-type)]
        ::  %get-raw-change-address: Returns a new Bitcoin address,
        ::  for receiving change.
        ::
        [%get-raw-change-address address-type=(unit address-type)]
        ::  %get-received-by-address:  Returns the total amount received by the
        ::  given address in transactions with at least minconf confirmations.
        ::
        [%get-received-by-address =address minconf=@ud]
        ::  %get-received-by-label:  Returns the total amount received by
        ::  addresses with <label> in transactions with at least [minconf]
        ::  confirmations.
        ::
        [%get-received-by-label label=@t minconf=(unit @ud)]
        ::  %get-transaction:  Get detailed information about in-wallet
        ::  transaction <txid>
        ::
        [%get-transaction txid=@ux include-watch-only=(unit ?)]
        ::  %get-unconfirmed-balance:  Returns the server's total unconfirmed
        ::  balance
        ::
        [%get-unconfirmed-balance ~]
        ::  %get-wallet-info:  Returns an object containing various wallet
        ::  state info.
        ::
        [%get-wallet-info wallet=@t]
        ::  %import-address: Adds an address or script (in hex) that can be
        ::  watched as if it were in your wallet but cannot be used to spend.
        ::
        $:  %import-address
            $=  address
            $%  [%addr @uc]
                [%bech32 @t]
                [%script @ux]
            ==
          ::
            label=(unit @t)
            rescan=(unit ?)
            p2sh=(unit ?)
        ==
        ::  %import-multi: Import addresses/scripts (with private or public
        ::  keys, redeem script (P2SH)), optionally rescanning the blockchain
        ::  from the earliest creation time of the imported scripts.
        ::
        $:  %import-multi
            requests=(list import-request)
            options=(unit rescan=?)
        ==
        ::  %import-privkey:  Adds a private key (as returned by dumpprivkey)
        ::  to your wallet.
        ::
        [%import-privkey privkey=@t label=(unit @t) rescan=(unit ?)]
        ::  %import-pruned-funds:  Imports funds without rescan.
        ::
        [%import-pruned-funds raw-transaction=@ux tx-out-proof=@ux]
        ::  %import-pubkey:  Adds a public key (in hex) that can be watched as
        ::  if it were in your wallet but cannot be used to spend.
        ::
        [%import-pubkey pubkey=@ux label=(unit @t) rescan=(unit ?)]
        ::  %import-wallet:  Imports keys from a wallet dump file
        ::
        [%import-wallet filename=@t]
        ::  %key-pool-refill:  Fills the keypool.
        ::
        [%key-pool-refill new-size=(unit @ud)]
        ::  %list-address-groupings:  Lists groups of addresses which have had
        ::  their common ownership (made public by common use as inputs or as
        ::  the resulting change in past transactions)
        ::
        [%list-address-groupings ~]
        ::  %list-labels:  Returns the list of all labels, or labels that are
        ::  assigned to addresses with a specific purpose.
        ::
        [%list-labels purpose=(unit purpose)]
        ::  %list-lock-unspent:  Returns list of temporarily unspendable
        ::  outputs.
        ::
        [%list-lock-unspent ~]
        ::  %list-received-by-address: List balances by receiving address.
        ::
        $:  %list-received-by-address
            %-  unit
            $:  minconf=(unit @ud)
                include-empty=(unit ?)
                include-watch-only=(unit ?)
                address-filter=(unit =address)
        ==  ==
        ::  %list-received-by-label: List received transactions by label.
        ::
        $:  %list-received-by-label
            %-  unit
            $:  minconf=(unit @ud)
                include-empty=(unit ?)
                include-watch-only=(unit ?)
        ==  ==
        ::  %lists-in-ceblock: Get all transactions in blocks since block
        ::  [blockhash], or all transactions if omitted.
        ::
        $:  %lists-in-ceblock
            %-  unit
            $:  blockhash=(unit blockhash)
                target-confirmations=(unit @ud)
                include-watch-only=(unit ?)
                include-removed=(unit ?)
        ==  ==
        ::  %list-transactions: If a label name is provided, this will return
        ::  only incoming transactions paying to addresses with the specified
        ::  label.
        ::
        $:  %list-transactions
            %-  unit
            $:  label=(unit @t)
                count=(unit @ud)
                skip=(unit @ud)
                include-watch-only=(unit ?)
        ==  ==
        ::  %list-unspent: Returns array of unspent transaction outputs
        ::  (with between minconf and maxconf (inclusive) confirmations.
        ::
        $:  %list-unspent
            %-  unit
            $:  minconf=(unit @ud)
                maxconf=(unit @ud)
                addresses=(unit (list address))
                include-unsafe=(unit ?)
              ::
                $=  query-options
                %-  unit
                $:  minimum-amount=(unit @ud)
                    maximum-amount=(unit @ud)
                    maximum-count=(unit @ud)
                    minimum-sum-amount=(unit @ud)
        ==  ==  ==
        ::  %list-wallet-dir  Returns a list of wallets in the wallet directory.
        ::
        [%list-wallet-dir ~]
        ::  %list-wallets  Returns a list of currently loaded wallets.
        ::  (For full information on the wallet, use "getwalletinfo"
        ::
        [%list-wallets ~]
        ::  %load-wallet  Loads a wallet from a wallet file or directory.
        ::
        [%load-wallet filename=@t]
        ::  %lock-unspent: Updates list of temporarily unspendable outputs.
        ::
        $:  %lock-unspent
            unlock=?
            transactions=(unit (list [txid=@ux vout=@ud]))
        ==
        ::  %remove-pruned-funds  Deletes the specified transaction from the
        ::  wallet.
        ::
        [%remove-pruned-funds txid=@ux]
        ::  %rescan-blockchain  Rescan the local blockchain for wallet related
        ::  transactions.
        ::
        [%rescan-blockchain start-height=(unit @ud) stop-height=(unit @ud)]
        ::  %send-many: Send multiple times.
        ::  Amounts are double-precision floating point numbers.
        ::
        $:  %send-many
            dummy=%$
            amounts=(list [=address amount=@t])
            minconf=(unit @ud)
            comment=(unit @t)
            subtract-fee-from=(unit (list address))
            replaceable=(unit ?)
            conf-target=(unit @ud)
            mode=(unit estimate-mode)
        ==
        ::  %send-to-address: Send an amount to a given address.
        ::
        $:  %send-to-address
            =address
            amount=@t
            comment=(unit @t)
            comment-to=(unit @t)
            subtract-fee-from-amount=(unit ?)
            replaceable=(unit ?)
            conf-target=(unit @ud)
            mode=(unit estimate-mode)
        ==
        ::  %set-hd-seed: Set or generate a new HD wallet seed.
        ::  Non-HD wallets will not be upgraded to being a HD wallet.
        ::
        [%set-hd-seed ~]
        ::  %set-label:   Sets the label associated with the given address.
        ::
        [%set-label =address label=@t]
        ::  %set-tx-fee: Set the transaction fee per kB for this wallet.
        ::
        [%set-tx-fee amount=@t]
        ::  %sign-message:   Sign a message with the private key of an address
        ::
        [%sign-message =address message=@t]
        ::  %sign-raw-transaction-with-wallet: Sign inputs for raw transaction
        ::  (serialized, hex-encoded).
        ::
        $:  %sign-raw-transaction-with-wallet
            hex-string=@ux
            prev-txs=(unit (list prev-tx))
            sig-hash-type=(unit sig-hash)
        ==
        ::  %unload-wallet:   Unloads the wallet referenced by the request
        ::  endpoint otherwise unloads the wallet specified in the argument.
        ::
        [%unload-wallet wallet-name=(unit @t)]
        ::  %wallet-create-fundedpsbt: Creates and funds a transaction in the
        ::  Partially Signed Transaction format.
        ::  Inputs will be added if supplied inputs are not enough
        ::
        $:  %wallet-create-fundedpsbt
            inputs=(list input)
            outputs=output
            locktime=(unit @ud)
          ::
            $=  options
            %-  unit
            $:  change-address=(unit address)
                change-position=(unit @ud)
                change-type=(unit address-type)
                include-watching=(unit ?)
                lock-unspents=(unit ?)
                fee-rate=(unit @t)
                subtract-fee-from-outputs=(unit (list @ud))
                replaceable=(unit ?)
                conf-target=(unit @ud)
                mode=(unit estimate-mode)
            ==
          ::
            bip32-derivs=(unit ?)
        ==
        ::  %wallet-lock:   Removes the wallet encryption key from memory,
        ::  locking the wallet.
        ::
        [%wallet-lock ~]
        ::  %wallet-passphrase:   Stores the wallet decryption key in memory
        ::  for 'timeout' seconds.
        ::
        [%wallet-passphrase passphrase=@t timeout=@ud]
        ::  %wallet-passphrase-changehange: s the wallet passphrase from
        ::  'oldpassphrase' to 'newpassphrase'.
        ::
        $:  %wallet-passphrase-change
            old-passphrase=(unit @t)
            new-passphrase=(unit @t)
        ==
        ::  %wallet-process-psbt: Update a PSBT with input information from
        ::  our wallet and then sign inputs
        ::
        $:  %wallet-process-psbt
            psbt=@t
            sign=?
            =sig-hash
            bip32-derivs=(unit ?)
        ==
    ::  ZMQ management
    ::
        ::  %get-zmq-notifications Returns information about the active
        ::  ZeroMQ notifications.
        ::
        [%get-zmq-notifications ~]
    ==
  ::
  +$  response
    $%
    ::  Control
        [%help ~]
    ::  Generating
    ::
        [%generate blocks=(list blockhash)]
    ::  Raw Transactions
    ::
        $:  %analyze-psbt
            $=  inputs
            %-  list
            $:  has-utxo=?
                is-final=?
              ::
                $=  missing
                %-  unit
                $:  pubkeys=(unit (list @ux))
                    signatures=(unit (list @ux))
                    redeem-script=(unit @ux)
                    witness-script=(unit @ux)
                ==
              ::
                next=(unit @t)
            ==
          ::
            estimated-vsize=(unit @t)
            estimated-feerate=(unit @t)
            fee=(unit @t)
            next=@t
        ==
      ::
        [%combine-psbt psbt=@t]
        [%combine-raw-transaction hex=@ux]
        [%convert-to-psbt psbt=@t]
        [%create-psbt psbt=@t]
        [%create-raw-transaction transaction=@ux]
      ::
        $:  %decode-psbt
            tx=serialized-tx
          ::
            unknown=(map @t @t)
          ::
            $=  inputs
            %-  list
            $:  non-witness-utxo=utxo
                witness-utxo=utxo
                partial-signatures=(unit (map pubkey=@ux signature=@ux))
                sig-hash-type=(unit sig-hash)
                redeem-script=(unit script)
                witness-script=(unit script)
              ::
                $=  bip32-derivs
                %-  unit
                %+  map
                pubkey=@ux
                ::
                $:  master-fingerprint=@t
                    path=@t
                ==
              ::
                final-script-sig=(unit [asm=@t hex=@ux])
                final-script-witness=(unit (list @ux))
                unknown=(unit (map @t @t))
            ==
          ::
            $=  outputs
            %-  list
            $:  redeem-script=(unit script)
                witness-script=(unit script)
              ::
                $=  bip32-derivs
                %-  unit
                %+  map
                pubkey=@ux
                ::
                $:  master-fingerprint=@t
                    path=@t
                ==
              ::
                unknown=(unit (map @t @t))
            ==
          ::
            fee=(unit @t)
        ==
      ::
        [%decode-raw-transaction =serialized-tx]
      ::
        $:  %decode-script
            asm=@t
            hex=(unit @ux)
            type=@t
            req-sigs=(unit @ud)
            addresses=(unit (list address))
            p2sh=(unit @uc)
            segwit=(unit segwit-script)
        ==
      ::
        [%finalize-psbt psbt=@t hex=(unit @ux) complete=?]
        [%fund-raw-transaction hex=@ux fee=@t change-pos=?(@ud %'-1')]
        [%get-raw-transaction data=?(@ux raw-transaction-rpc-out)]
        [%join-psbts psbt=@t]
        [%send-raw-transaction hex=@ux]
      ::
        $:  %sign-raw-transaction-with-key
            hex=@ux
            complete=?
            errors=(unit errors)
        ==
      ::
        $:  %test-mempool-accept
            %-  list
            $:  txid=@ux
                allowed=?
                reject-reason=(unit @t)
        ==  ==
      ::
        [%utxo-update-psbt psbt=@t]
    ::  Util
    ::
        [%create-multi-sig =address redeem-script=@t]
        [%derive-addresses (list address)]
      ::
        $:  %estimate-smart-fee
            fee-rate=(unit @t)
            errors=(unit (list @t))
            blocks=@ud
        ==
      ::
        $:  %get-descriptor-info
            descriptor=@t
            is-range=?
            is-solvable=?
            has-private-keys=?
        ==
      ::
        [%sign-message-with-privkey signature=@t]
      ::
        $:  %validate-address
            is-valid=?
            =address
            script-pubkey=@ux
            is-script=?
            is-witness=?
            witness-version=(unit @t)
            witness-program=(unit @ux)
        ==
      ::
        [%verify-message ?]
    ::  Wallet
    ::
        [%abandon-transaction ~]
        [%abort-rescan ~]
      ::
        $:  %add-multisig-address
            =address
            redeem-script=@t
        ==
      ::
        [%backup-wallet ~]
        [%bump-fee txid=@ux orig-fee=@t fee=@t errors=(list @t)]
        [%create-wallet name=@t warning=@t]
        [%dump-privkey key=@t]
        [%dump-wallet filename=@t]
        [%encrypt-wallet ~]
      ::
        $:  %get-addresses-by-label
            addresses=(list [address =purpose])
        ==
      ::
        $:  %get-address-info
            =address
            script-pubkey=@ux
            is-mine=?
            is-watchonly=?
            solvable=?
            desc=(unit @t)
            is-script=?
            is-change=?
            is-witness=?
            witness-version=(unit @t)
            witness-program=(unit @ux)
            script=(unit @t)
            hex=(unit @ux)
            pubkeys=(unit (list @ux))
            sigs-required=(unit @ud)
            pubkey=(unit @ux)
          ::
            $=  embedded
            %-  unit
            $:  script-pubkey=@ux
                solvable=(unit ?)
                desc=(unit @t)
                is-script=?
                is-change=(unit ?)
                is-witness=?
                witness-version=(unit @t)
                witness-program=(unit @ux)
                script=(unit @ux)
                hex=(unit @ux)
                pubkeys=(unit (list @ux))
                sigs-required=(unit @ud)
                pubkey=(unit @ux)
                is-compressed=(unit ?)
                label=(unit @t)
                hd-master-finger-print=(unit @ux)
                labels=(unit (list [name=@t =purpose]))
            ==
          ::
            is-compressed=(unit ?)
            label=(unit @t)
            timestamp=(unit @ud)
            hd-key-path=(unit @t)
            hd-seed-id=(unit @ux)
            hd-master-finger-print=(unit @ux)
            labels=(list [name=@t =purpose])
        ==
      ::
        [%get-balance amount=@t]
        [%get-new-address =address]
        [%get-raw-change-address =address]
        [%get-received-by-address amount=@t]
        [%get-received-by-label amount=@t]
      ::
        $:  %get-transaction
            amount=@t
            fee=(unit @t)
            confirmations=@ud
            blockhash=(unit @ux)
            blockindex=(unit @ud)
            blocktime=(unit @ud)
            txid=@ux
            time=@ud
            time-received=@ud
            =bip125-replaceable
          ::
            $=  details
            %-  list
            $:  address=(unit address)
                =category
                amount=@t
                label=(unit @t)
                vout=@ud
                fee=(unit @t)
                abandoned=(unit ?)
            ==
          ::
            hex=@ux
        ==
      ::
        [%get-unconfirmed-balance amount=@t]
      ::
        $:  %get-wallet-info
            wallet-name=@t
            wallet-version=@ud
            balance=@t
            unconfirmed-balance=@t
            immature-balance=@t
            tx-count=@ud
            key-pool-oldest=@ud
            key-pool-size=@ud
            key-pool-size-hd-internal=(unit @ud)
            unlocked-until=(unit @ud)
            pay-tx-fee=@t
            hd-seed-id=(unit @ux)
            private-keys-enabled=?
        ==
      ::
        [%import-address ~]
      ::
        $:  %import-multi
            %-  list
            $:  success=?
                warnings=(unit (list @t))
                errors=(unit [code=@t message=@t])
        ==  ==
      ::
        [%import-privkey ~]
        [%import-pruned-funds ~]
        [%import-pubkey ~]
        [%import-wallet ~]
        [%key-pool-refill ~]
      ::
        $:  %list-address-groupings
            $=  groups
            %-  list
            %-  list
            %-  list
            $:  address
                amount=@t
                label=(unit @t)
        ==  ==
      ::
        [%list-labels labels=(list @t)]
        [%list-lock-unspent outputs=(list [txid=@ux vout=@ud])]
      ::
        $:  %list-received-by-address
            %-  list
            $:  involves-watch-only=(unit %&)
                =address
                amount=@t
                confirmations=@ud
                label=@t
                txids=(list @ux)
        ==  ==
      ::
        $:  %list-received-by-label
            %-  list
            $:  involves-watch-only=(unit %&)
                amount=@t
                confirmations=@ud
                label=@t
        ==  ==
      ::
        $:  %lists-in-ceblock
            transactions=(list tx-in-block)
            removed=(unit (list tx-in-block))
            last-block=@ux
        ==
      ::
        [%list-transactions transactions=(list tx-in-block)]
      ::
        $:  %list-unspent
        $=  txs
            %-  list
            $:  txid=@ux
                vout=@ud
                =address
                label=@t
                script-pubkey=@ux
                amount=@t
                confirmations=@ud
                redeem-script=@ux
                witness-script=(unit @ux)
                spendable=?
                solvable=?
                desc=(unit @t)
                safe=?
        ==  ==
      ::
        [%list-wallet-dir wallets=(list @t)]
        [%list-wallets wallets=(list @t)]
        [%load-wallet name=@t warning=@t]
        [%lock-unspent ?]
        [%remove-pruned-funds ~]
        [%rescan-blockchain start-height=@ud stop-height=@ud]
        [%send-many txid=@ux]
        [%send-to-address txid=@ux]
        [%set-hd-seed ~]
        [%set-label ~]
        [%set-tx-fee ?]
        [%sign-message signature=@t]
      ::
        $:  %sign-raw-transaction-with-wallet
            hex=@ux
            complete=?
            errors=(unit errors)
        ==
      ::
        [%unload-wallet ~]
        [%wallet-create-fundedpsbt psbt=@t fee=@t changepos=?(@ud %'-1')]
        [%wallet-lock ~]
        [%wallet-passphrase ~]
        [%wallet-passphrase-change ~]
        [%wallet-process-psbt psbt=@t complete=?]
      ::
    ::  ZMQ
    ::
        $:  %get-zmq-notifications
            (list [type=@t =address hwm=@ud])
    ==  ==
  ::
  +$  command
    $%  ::  Loads RPC node URL+credentials
        ::
        [%credentials url=@t heads=(list [@t @t])]
        ::  TODO: Sync data/wallets...
        ::
        [%sync ~]
    ==
  --
--
