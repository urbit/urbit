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
  =<  [nas own]
  (apply-raw-tx | raw-tx.diff nas own chain-t)
::
++  apply-raw-tx
  |=  [force=? =raw-tx:naive nas=^state:naive own=owners chain-t=@]
  ^-  [? nas=_nas ups=(list update) own=_own]
  =+  cache-nas=nas
  =/  chain-t=@t  (ud-to-ascii:naive chain-t)
  ?.  (verify-sig-and-nonce:naive verifier chain-t nas raw-tx)
    ~&  [%verify-sig-and-nonce %failed tx.raw-tx]
    [force nas ~ own]
  =^  *  points.nas
    (increment-nonce:naive nas from.tx.raw-tx)
  ?~  nex=(receive-tx:naive nas tx.raw-tx)
    ~&  [%receive-tx %failed]
    [force ?:(force nas cache-nas) ~ own]
  =*  new-nas   +.u.nex
  =*  effects   -.u.nex
  :+  &
    new-nas
  (update-ownership effects cache-nas new-nas own)
::
++  update-ownership
  |=  $:  =effects:naive
          cache-nas=^state:naive
          nas=^state:naive
          =owners
      ==
  ^-  (quip update _owners)
  %+  roll  effects
  |=  [=diff:naive ups=(list update) owners=_owners]
  =,  orm:naive
  ?.  ?=([%point *] diff)  [ups owners]
  =*  ship  ship.diff
  =/  old=(unit point:naive)
    (get points.cache-nas ship)
  =/  new=point:naive
    (need (get points.nas ship))
  =*  event  +>.diff
  =;  [to=(unit @ux) from=(unit @ux)]
    =?  owners  ?=(^ from)
      (~(del ju owners) u.from ship)
    ?~  to  [ups owners]
    :-  (snoc ups [%point u.to ship new])
    (~(put ju owners) u.to ship)
  ?+    -.event  [~ ~]
      %owner
    [`+.event ?~(old ~ `address.owner.own.u.old)]
  ::
      %spawn-proxy
    [`+.event ?~(old ~ `address.spawn-proxy.own.u.old)]
  ::
      %management-proxy
    [`+.event ?~(old ~ `address.management-proxy.own.u.old)]
  ::
      %voting-proxy
    [`+.event ?~(old ~ `address.voting-proxy.own.u.old)]
  ::
      %transfer-proxy
    [`+.event ?~(old ~ `address.transfer-proxy.own.u.old)]
  ==
::
++  get-owner
  |=  [=point:naive =proxy:naive]
  ^-  [nonce=@ _point]
  =*  own  own.point
  ?-    proxy
      %own
    :-  nonce.owner.own
    point(nonce.owner.own +(nonce.owner.own))
  ::
      %spawn
    :-  nonce.spawn-proxy.own
    point(nonce.spawn-proxy.own +(nonce.spawn-proxy.own))
  ::
      %manage
    :-  nonce.management-proxy.own
    point(nonce.management-proxy.own +(nonce.management-proxy.own))
  ::
      %vote
    :-  nonce.voting-proxy.own
    point(nonce.voting-proxy.own +(nonce.voting-proxy.own))
  ::
      %transfer
    :-  nonce.transfer-proxy.own
    point(nonce.transfer-proxy.own +(nonce.transfer-proxy.own))
  ==
::
--
