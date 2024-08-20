::  claz:  command & call structures
::
::TODO  contract structures might go into stdlib
::
=,  ethereum-types
::
|%
++  command
  $%  [%generate =path =network as=address =batch]
  ==
::
++  network
  $?  %mainnet
      %goerli
      %fakenet
      [%other id=@]
  ==
::
++  batch
  $~  [%deed '{}']
  $%  ::  %single: execute a single ecliptic function call
      ::
      [%single =call]
      ::  %deed: deed ships based on json, assumes spawnable
      ::
      [%deed deeds-json=cord]
      ::  %invites: sendPoint for every ship in ship,ticket,owner file
      ::
      ::    to generate such a file, try |claz-invites ~star 1 10 %/out/txt
      ::
      [%invites as-who=ship file=path]
      ::  %lock-prep: prepare for lockup by transfering ships to the ceremony address
      ::
      [%lock-prep what=(list ship)]
      ::  %lock: put ships into lockup for the target address
      ::
      [%lock how=?(%spawn %transfer) what=(list ship) to=address =lockup]
      ::  %more: multiple batches sequentially
      ::
      [%more batches=(list batch)]
  ==
::
++  lockup
  $%  [%linear windup-years=@ud unlock-years=@ud]
      [%conditional [b1=@ud b2=@ud b3=@ud] unlock-years-per-batch=@ud]
  ==
::
++  rights
  $:  own=address
      manage=(unit address)
      voting=(unit address)
      transfer=(unit address)
      spawn=(unit address)
      net=(unit [crypt=@ux auth=@ux])
  ==
::
++  call
  $%  [%create-galaxy gal=ship to=address]
      [%spawn who=ship to=address]
      [%configure-keys who=ship crypt=@ auth=@]
      [%set-management-proxy who=ship proxy=address]
      [%set-voting-proxy who=ship proxy=address]
      [%set-spawn-proxy who=ship proxy=address]
      [%transfer-ship who=ship to=address]
      [%set-transfer-proxy who=ship proxy=address]
      [%adopt who=ship]
      [%start-document-poll gal=ship hash=@]
      [%cast-document-vote gal=ship hash=@ vote=?]
      [%start-upgrade-poll gal=ship =address]
      [%cast-upgrade-vote gal=ship =address vote=?]
    ::
      [%send-point as=ship point=ship to=address]
    ::
      [%approve-batch-transfer to=address]
      [%transfer-batch from=address]
      [%withdraw to=address]
  ==
::
++  prep-result
  $%  [%nonce nonce=@ud]
  ==
--
