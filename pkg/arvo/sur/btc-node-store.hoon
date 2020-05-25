=>  ::  Helper types
    ::
    |%
    ++  blockhash  @ux
    ::
    +$  purpose  ?(%send %receive)
    +$  address
      $:  address=?(@uc [%bech32 @t])
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
          is-compressed=(unit ?)
          label=(unit @t)
          timestamp=(unit @t)
          hd-key-path=(unit @t)
          hd-seed-id=(unit @ux)
          hd-master-finger-print=(unit @ux)
          labels=(list [name=@t =purpose])
      ==
    ::
    +$  wallet-attr
      $:  wallet-version=@ud
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
          avoid-reuse=?
          scanning=?(? [duration=@t progress=@t])
      ==
    ::
    +$  wallet
      [name=@t attrs=(unit wallet-attr)]
    ::
    +$  wallets    (map @t wallet)
    ::
    +$  addresses  (list address)
    --
|%
::
+$  btc-node-store-action   action:btc-rpc
+$  btc-node-store-update   update:btc-rpc
+$  btc-node-store-command  command:btc-rpc
::
++  btc-rpc
  |%
  ::  %action: the result of an RPC action from the full node
  ::
  +$  action
    $%  :: [%abandon-transaction]
        :: [%abort-rescan]
        [%add-multisig-address ~]
        :: [%backup-wallet ~]
        :: [%bump-fee]
        ::   Adds a new wallet to the list
        ::
        [%add-wallet name=@t warning=(unit @t)]
        :: [%dump-privkey ~]
        :: [%dump-wallet ~]
        :: [%encrypt-wallet ~]
        :: [%get-addresses-by-label]
        :: [%get-address-info]
        :: [%get-balance]
        :: [%get-new-address]
        :: [%get-raw-change-address]
        :: [%get-received-by-address]
        :: [%get-received-by-label]
        :: [%get-transaction]
        :: [%get-unconfirmed-balance]
        :: [%get-wallet-info ~] -> This is replaced by %update
        [%update-wallet name=@t attrs=wallet-attr]
        [%import-address ~]
        [%import-multi ~]
        [%import-privkey ~]
        [%import-pruned-funds ~]
        [%import-pubkey ~]
        [%import-wallet ~]
        [%key-pool-refill ~]
        :: [%list-address-groupings]
        :: [%list-labels]
        :: [%list-lock-unspent]
        :: [%list-received-by-address]
        :: [%list-received-by-label]
        :: [%lists-in-ceblock]
        :: [%list-transactions]
        :: [%list-unspent]
        :: [%list-wallet-dir ~]
        [%list-wallets ~]
        [%load-wallet name=@t]
        :: [%lock-unspent]
        :: [%remove-pruned-funds ~]
        :: [%rescan-blockchain ~]
        :: [%send-many ~]
        :: [%send-to-address ~]
        :: [%set-hd-seed ~]
        :: [%set-label ~]
        :: [%set-tx-fee ~]
        :: [%sign-message ~]
        :: [%sign-raw-transaction-with-wallet ~]
        :: [%unload-wallet ~]
        :: [%wallet-create-fundedpsbt ~]
        :: [%wallet-lock ~]
        :: [%wallet-passphrase ~]
        :: [%wallet-passphrase-change ~]
        :: [%wallet-process-psbt ~]
        :: [%get-zmq-notifications]
    ==
  ::
  ::  %update: modifies data on the %store app
  ::
  +$  update
    $%  ::  Wallet name has changed
        ::
        [%wallet-name name=@t]
        ::  Updates wallet attributes
        ::  FIXME: all attrs might need to be units...
        ::
        [%wallet-attrs name=@t attr=wallet-attr]
    ==
  ::
  ::  %command: instruction to perform over the stored data
  ::
  +$  command
    $%  ::  Updated the default wallet stored in btc-node-store
        ::
        [%switch-wallet @t]
      ::
        ::  Loads an external wallet
        ::
        [%load-wallet @t]
      ::
        ::  TODO: do the actual syncing
        ::
        [%sync ~]
    ==
  --
--
