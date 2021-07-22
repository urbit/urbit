::  dice: helper functions for L2 Rollers
::
/-  *dice
/+  naive, *naive-transactions
::
|%
++  apply-effects
  |=  [=effects:naive nas=^state:naive own=owners chain-t=@]
  ^+  [nas=nas own=own]
  %+  roll  effects
  |=  [=diff:naive nas=_nas own=_own]
  ^+  [nas own]
  ?.  ?=([%tx *] diff)  [nas own]
  =+  cache-nas=nas
  =*  raw-tx  raw-tx.diff
  =/  chain-t=@t  (ud-to-ascii:naive chain-t)
  ?.  (verify-sig-and-nonce:naive verifier chain-t nas raw-tx)
    [nas own]
  =^  *  points.nas
    (increment-nonce:naive nas from.tx.raw-tx)
  ?~  nex=(receive-tx:naive nas tx.raw-tx)
    [cache-nas own]
  =*  up-nas  +.u.nex
  =*  diffs   -.u.nex
  :-  up-nas
  (update-ownership diffs cache-nas up-nas own)
::
++  update-ownership
  |=  $:  =effects:naive
          cache-nas=^state:naive
          up-nas=^state:naive
          =owners
      ==
  ^+  owners
  %+  roll  effects
  |=  [=diff:naive owners=_owners]
  =,  orm:naive
  ?.  ?=([%point *] diff)  owners
  =/  old=(unit point:naive)
    (get points.cache-nas ship.diff)
  =/  new=point:naive
    (need (get points.up-nas ship.diff))
  =*  event  +>.diff
  =;  [to=@ux from=@ux]
    =?  owners  !=(from 0x0)
      (~(del ju owners) from ship.diff)
    ?:  =(to 0x0)  owners
    (~(put ju owners) to ship.diff)
  ?+    -.event  [0x0 0x0]
      %owner
    [+.event ?~(old 0x0 address.owner.own.u.old)]
  ::
      %spawn-proxy
    [+.event ?~(old 0x0 address.spawn-proxy.own.u.old)]
  ::
      %management-proxy
    [+.event ?~(old 0x0 address.management-proxy.own.u.old)]
  ::
      %voting-proxy
    [+.event ?~(old 0x0 address.voting-proxy.own.u.old)]
  ::
      %transfer-proxy
    [+.event ?~(old 0x0 address.transfer-proxy.own.u.old)]
  ==
--
