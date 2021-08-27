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
  ^-  (quip update own=_owners)
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
  =;  [to=(unit owner) from=(unit owner)]
    =?  owners  &(?=(^ from) !=(address.u.from 0x0))
      (~(del ju owners) u.from ship)
    ?:  ?|  =(~ to)
            &(?=(^ to) =(address.u.to 0x0))
        ==
      [ups owners]
    ?~  to  [ups owners]
    :_  (~(put ju owners) u.to ship)
    (snoc ups [%point ship new u.to from])
  ?+    -.event  [~ ~]
      %owner
    :-  `[%own +.event]
    ?~  old  ~
    `[%own address.owner.own.u.old]
  ::
      %management-proxy
    :-  `[%manage +.event]
    ?~  old  ~
    `[%manage address.management-proxy.own.u.old]
  ::
      %spawn-proxy
    :-  `[%spawn +.event]
    ?~  old  ~
    `[%spawn address.spawn-proxy.own.u.old]
  ::
      %voting-proxy
    :-  `[%vote +.event]
    ?~  old  ~
    `[%vote address.voting-proxy.own.u.old]
  ::
      %transfer-proxy
    :-  `[%transfer +.event]
    ?~  old  ~
    `[%transfer address.transfer-proxy.own.u.old]
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
