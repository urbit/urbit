|%
++  address  @  ::TODO  did an aura exist for this? we need a to-string arm
++  blockhash  @ux
::
+$  btc-node-hook-action  request:btc-rpc
::
++  btc-rpc
  |%
  +$  request
    $%  ::  node management
        ::
        [%generate blocks=@ud max-tries=(unit @ud)]
        ::  chain state
        ::
        [%get-block-count ~]
        ::  wallet management
        ::
        [%list-wallets ~]
        [%create-wallet name=@t disable-private-keys=_|]
    ==
  ::
  +$  response
    $%  [%generate blocks=(list blockhash)]
        [%get-block-count count=@ud]
        [%list-wallets wallets=(list @t)]
        [%create-wallet name=@t warning=@t]
    ==
  --
--
