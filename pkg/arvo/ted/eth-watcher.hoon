::  eth-watcher: ethereum event log collector
::
/-  spider, *eth-watcher
/+  strandio, ethio
=,  ethereum-types
=,  able:jael
::
::  Main loop: get updates since last checked
::
|=  args=vase
|^
=+  !<(pup=watchpup args)
=/  m  (strand:strandio ,vase)
^-  form:m
;<  =latest=block                 bind:m  (get-latest-block:ethio url.pup)
;<  pup=watchpup   bind:m         (zoom pup number.id.latest-block)
=|  vows=disavows
?.  eager.pup
  (pure:m !>([vows pup]))
|-  ^-  form:m
=*  loop  $
?:  (gth number.pup number.id.latest-block)
  (pure:m !>([vows pup]))
;<  =block  bind:m                (get-block-by-number:ethio url.pup number.pup)
;<  [=new=disavows pup=watchpup]  bind:m  (take-block pup block)
%=  loop
  pup   pup
  vows  (weld vows new-disavows)
==
::
::  Process a block, detecting and handling reorgs
::
++  take-block
  |=  [pup=watchpup =block]
  =/  m  (strand:strandio ,[disavows watchpup])
  ^-  form:m
  ::  if this next block isn't direct descendant of our logs, reorg happened
  ?:  &(?=(^ blocks.pup) !=(parent-hash.block hash.id.i.blocks.pup))
    (rewind pup block)
  ;<  =new=loglist  bind:m  ::  oldest first
    (get-logs-by-hash:ethio url.pup hash.id.block contracts.pup topics.pup)
  %-  pure:m
  :-  ~
  %_  pup
    number        +(number.id.block)
    pending-logs  (~(put by pending-logs.pup) number.id.block new-loglist)
    blocks        [block blocks.pup]
  ==
::
::  Reorg detected, so rewind until we're back in sync
::
++  rewind
  ::  block: wants to be head of blocks.pup, but might not match
  |=  [pup=watchpup =block]
  =/  m  (strand:strandio ,[disavows watchpup])
  =*  blocks  blocks.pup
  =|  vows=disavows
  |-  ^-  form:m
  =*  loop  $
  ::  if we have no further history to rewind, we're done
  ?~  blocks
    (pure:m (flop vows) pup(blocks [block blocks]))
  ::  if target block is directly after "latest", we're done
  ?:  =(parent-hash.block hash.id.i.blocks)
    (pure:m (flop vows) pup(blocks [block blocks]))
  ::  next-block: the new target block
  ;<  =next=^block  bind:m
    (get-block-by-number:ethio url.pup number.id.i.blocks)
  =.  pending-logs.pup  (~(del by pending-logs.pup) number.id.i.blocks)
  =.  vows  [id.block vows]
  loop(block next-block, blocks t.blocks)
::
::  Zoom forward to near a given block number.
::
::    Zooming doesn't go forward one block at a time.  As a
::    consequence, it cannot detect and handle reorgs.  Only use it
::    at a safe distance -- 100 blocks ago is probably sufficient.
::
++  zoom
  |=  [pup=watchpup =latest=number:block]
  =/  m  (strand:strandio ,watchpup)
  ^-  form:m
  =/  zoom-margin=number:block  30
  =/  zoom-step=number:block  100.000
  ?:  (lth latest-number (add number.pup zoom-margin))
    (pure:m pup)
  =/  up-to-number=number:block  (sub latest-number zoom-margin)
  |-
  =*  loop  $
  ?:  (gth number.pup up-to-number)
    (pure:m pup(blocks ~))
  =/  to-number=number:block
    (min up-to-number (add number.pup zoom-step))
  ;<  =loglist  bind:m  ::  oldest first
    %:  get-logs-by-range:ethio
      url.pup
      contracts.pup
      topics.pup
      number.pup
      to-number
    ==
  =?  pending-logs.pup  ?=(^ loglist)
    (~(put by pending-logs.pup) to-number loglist)
  loop(number.pup +(to-number))
--
