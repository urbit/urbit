=<  tx
|%
+$  pubkey  @ux 
+$  multisig
  [members=(set pubkey) threshold=@ud]
+$  owner  ?(pubkey multisig)
+$  id  @ud
+$  account-id  @ux
+$  hash  @ux
+$  nonce  @ud
+$  amount  @ud
+$  supply  @ud
+$  zigs  amount
::
+$  asset
  $%  [%nft minter=account-id =id uri=@t =hash can-xfer=?]
      [%tok minter=account-id =amount]
  ==
+$  minting-asset
  $%  [%nft uri=@t =hash can-xfer=?]
      [%tok =amount]
  ==
+$  account
  $%
    $:  %blank-account
        ~
    ==
    $:  %minter-account
        =owner
        ::  minter accounts can't themselves
        ::  transact -- no nonce needed?
        ::  =nonce
        whitelist=(set account-id)
        max=supply
        total=supply
    ==
    $:  %asset-account
        =owner
        =nonce
        assets=(map account-id asset)
    ==
  ==
+$  state  [=hash accts=(map account-id account)]
::  TODO: patricia merkle tree data structure

::  transactions
::
+$  sig-type  ?(%schnorr %ecdsa)
+$  signature
  [r=@ux s=@ux =sig-type]
+$  pubkey-sender
  $:  =account-id 
      =nonce
      feerate=zigs
      =pubkey 
      sig=signature
  ==
+$  multisig-sender
  $:  =account-id 
      =nonce
      feerate=zigs
      signers=(set [pubkey signature])
  ==
+$  sender  ?(pubkey-sender multisig-sender)
::
++  tx
  $%  
    $:  %send
        from=sender
        to=?(account-id pubkey)
        ::  making assets a map to enforce uniqueness of assets
        assets=(map ?(account-id hash) asset)
    ==
    $:  %mint
        from=sender
        minter=account-id
        to=(list [?(account-id pubkey) minting-asset])
    ==
    $:  %lone-mint
        from=sender
        to=(list [?(account-id pubkey) minting-asset])
    ==
    ::
    $:  %create-minter
        from=sender
        max=supply
        =owner
        whitelist=(set account-id)
    ==
    $:  %update-minter
        from=sender
        minter=account-id
        =owner
        whitelist=(set account-id)
    ==
    ::
    [%create-multisig from=sender owner=multisig]
    [%update-multisig from=multisig-sender owner=multisig]
    ::
    [%coinbase from=sender fees=zigs]
  ==
--
