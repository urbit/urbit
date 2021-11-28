::  dice: helper functions for L2 Rollers
::
/-  *dice
/+  naive, *naive-transactions
::
|%
::  orp: ordered points in naive state by parent ship
::
++  orp  ((on ship point:naive) por:naive)
::  ors: ordered sending map by (increasing) L1 nonce
::
++  ors  ((on l1-tx-pointer send-tx) nonce-order)
::  orh: ordered tx history by (decreasing) timestamp
::
++  orh  ((on time roll-tx) gth)
::
++  nonce-order
  |=  [a=[* =nonce:naive] b=[* =nonce:naive]]
  (lte nonce.a nonce.b)
::
++  tx-effects
  |=  [chain-t=@ =effects:naive nas=^state:naive =indices]
  ^+  indices
  =<  indices
  %+  roll  effects
  |=  [=diff:naive nas=_nas indices=_indices]
  ?.  ?=([%tx *] diff)  [nas indices]
  =<  [nas indices]
  (apply-raw-tx | chain-t raw-tx.diff nas indices)
::
++  apply-raw-tx
  |=  [force=? chain-t=@ =raw-tx:naive nas=^state:naive =indices]
  ^-  [? (list update) nas=_nas indices=_indices]
  =+  cache=nas
  =/  chain-t=@t  (ud-to-ascii:naive chain-t)
  ?.  (verify-sig-and-nonce:naive verifier chain-t nas raw-tx)
    ~&  >>>  [%verify-sig-and-nonce %failed tx.raw-tx]
    [force ~ nas indices]
  =^  effects-1  points.nas
    (increment-nonce:naive nas from.tx.raw-tx)
  ?~  nex=(receive-tx:naive nas tx.raw-tx)
    ~&  >>>  [%receive-tx %failed]
    [force ~ ?:(force nas cache) indices]
  =*  new-nas   +.u.nex
  =/  effects   (welp effects-1 -.u.nex)
  =^  updates   indices
    (point-effects effects points.cache points.new-nas [own spo]:indices)
  [& updates new-nas indices]
::
++  point-effects
  |=  [=effects:naive cache=points:naive =points:naive =indices]
  ^-  [(list update) indices=_indices]
  =^  updates=(list update)  indices
    %+  roll  effects
    |=  [=diff:naive updates=(list update) indices=_indices]
    ?.  |(?=([%point *] diff) ?=([%nonce *] diff))
      [updates indices]
    ?:  ?=([%nonce *] diff)
      :_  indices
      (welp (nonce-updates diff cache points) updates)
    ?>  ?=([%point *] diff)
    =*  ship      ship.diff
    =*  sponsors  spo.indices
    =*  owners    own.indices
    =/  old=(unit point:naive)  (get:orp cache ship)
    =/  new=point:naive         (need (get:orp points ship))
    =^  update-1  sponsors
      (sponsorship diff ship new old sponsors)
    =^  update-2  owners
      (ownership diff ship new old owners)
    =/  update-3=_updates
      (point-data-updates diff ship new old)
    :_  indices
    :(welp update-3 update-2 update-1 updates)
  [(flop updates) indices]
::
++  sponsorship
  |=  [=diff:naive =ship new=point:naive old=(unit point:naive) =sponsors]
  ::^+  sponsors
  ^-  (quip update _sponsors)
  ?.  ?=([%point *] diff)  `sponsors
  =*  event  +>.diff
  ?+    -.event  `sponsors
      %owner
    ?^  old
      ::  ownership change
      ::
      `sponsors
    ::  owner event with ?=(~ old) is a spawn
    ::
    =*  parent  who.sponsor.net.new
    ?:  =(parent ship)  `sponsors
    ::  updates for proxy %own are taken care of
    ::  in +sponsorship  to avoid duplicates
    ::
    :-  ~
    %+  ~(put by sponsors)  parent
    ?~  sponsor=(~(get by sponsors) parent)
      :_  *(set @p)
      (~(put in *(set @p)) ship)
    :_  requests.u.sponsor
    (~(put in residents.u.sponsor) ship)
  ::
      %escape
    ?~  old     `sponsors
    =*  from    who.sponsor.net.u.old
    =*  to      to.event
    =*  escape  escape.net.u.old
    ?~  to
      :: cancel/reject escape
      ::
      ?~  escape  `sponsors
      ?~  receiver=(~(get by sponsors) u.escape)
        `sponsors
      ::
      :-  (proxy-updates diff ship new old)
      ::
      %+  ~(put by sponsors)  u.escape
      :-  residents.u.receiver
      (~(del in requests.u.receiver) ship)
    ::  normal escape
    ::
    :-  (proxy-updates diff ship new old)
    ::
    %+  ~(put by sponsors)  u.to
    ?~  receiver=(~(get by sponsors) u.to)
      :-  *(set @p)
      (~(put in *(set @p)) ship)
    :-  residents.u.receiver
    (~(put in requests.u.receiver) ship)
  ::
      %sponsor
    ?~  old   `sponsors
    =*  from  who.sponsor.net.u.old
    =*  to    sponsor.event
    =/  previous  (~(get by sponsors) from)
    ?~  to
      :: detach
      ::
      ?~  previous  `sponsors
      ::
      :-  (proxy-updates diff ship new old)
      ::
      %+  ~(put by sponsors)  from
      :_  requests.u.previous
      (~(del in residents.u.previous) ship)
    ::  accepted
    ::
    =/  receiver  (~(get by sponsors) u.to)
    =?  sponsors  ?=(^ receiver)
      %+  ~(put by sponsors)  u.to
      :-  (~(put in residents.u.receiver) ship)
      (~(del in requests.u.receiver) ship)
    =?  sponsors  ?=(^ previous)
      %+  ~(put by sponsors)  from
      :_  requests.u.previous
      (~(del in residents.u.previous) ship)
    :_  sponsors
    (proxy-updates diff ship new old)
  ==
::
++  ownership
  |=  [=diff:naive =ship new=point:naive old=(unit point:naive) =owners]
  ^-  (quip update _owners)
  ?.  ?=([%point *] diff)  `owners
  =*  event  +>.diff
  =;  [to=(unit owner) from=(unit owner)]
    =?  owners  &(?=(^ from) !=(address.u.from 0x0))
      (~(del ju owners) u.from ship)
    ?:  ?|  =(~ to)
            &(?=(^ to) =(address.u.to 0x0))
        ==
      [~ owners]
    ?~  to  [~ owners]
    :_  (~(put ju owners) u.to ship)
    [%point diff ship new old u.to from]~
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
++  proxy-updates
  |=  [=diff:naive =ship =point:naive old=(unit point:naive)]
  ^-  (list update)
  =/  proxies=(list proxy:naive)
    ~[%own %spawn %manage %vote %transfer]
  ?~  old  ~
  %-  flop
  %+  roll  proxies
  |=  [=proxy:naive updates=(list update)]
  ?~  owner=(get-owner u.old proxy)  updates
  :_  updates
  [%point diff ship point old u.owner ~]
::
++  nonce-updates
  |=  [=diff:naive cache=points:naive =points:naive]
  ^-  (list update)
  ?.  ?=([%nonce *] diff)  ~
  =/  old=(unit point:naive)  (get:orp cache ship.diff)
  =/  new=point:naive         (need (get:orp points ship.diff))
  (point-data-updates diff ship.diff new old)
::
++  point-data-updates
  |=  [=diff:naive =ship =point:naive old=(unit point:naive)]
  ^-  (list update)
  ?.  ?=([%point *] diff)  ~
  =*  event  +>.diff
  ?+    -.event  ~
    %rift      (proxy-updates +<)
    %keys      (proxy-updates +<)
    %dominion  (proxy-updates +<)
  ==
::
++  get-owner
  |=  [=point:naive =proxy:naive]
  ^-  (unit owner)
  =,  own.point
  ?-    proxy
      %own
    ?:(=(address.owner 0x0) ~ `own+address.owner)
  ::
      %spawn
    ?:(=(address.spawn-proxy 0x0) ~ `spawn+address.spawn-proxy)
  ::
      %manage
    ?:(=(address.management-proxy 0x0) ~ `manage+address.management-proxy)
  ::
      %vote
    ?:(=(address.voting-proxy 0x0) ~ `vote+address.voting-proxy)
  ::
      %transfer
    ?:(=(address.transfer-proxy 0x0) ~ `transfer+address.transfer-proxy)
  ==
::
++  get-nonce
  |=  [=point:naive =proxy:naive]
  ^-  nonce:naive
  =,  own.point
  ?-  proxy
    %own       nonce.owner
    %spawn     nonce.spawn-proxy
    %manage    nonce.management-proxy
    %vote      nonce.voting-proxy
    %transfer  nonce.transfer-proxy
  ==
::
++  controlled-ships
  |=  [=address:ethereum =owners]
  ^-  (list [=proxy:naive =ship])
  =/  proxies=(list proxy:naive)
    ~[%own %spawn %manage %vote %transfer]
  %+  roll  proxies
  |=  [=proxy:naive ships=(list [=proxy:naive ship])]
  %+  welp
    %+  turn
      ~(tap in (~(get ju owners) [proxy address]))
    (lead proxy)
  ships
::  +update-history: updates status for all given list of transaction
::
++  update-history
  |=  [=history txs=(list pend-tx) =status]
  ^-  (quip update _history)
  =^  updates=(list update)  history
    %+  roll  txs
    |=  [=pend-tx ups=(list update) history=_history]
    =,  pend-tx
    =/  =roll-tx
      =,  pend-tx
      :*  ship.from.tx.raw-tx
          status
          (hash-raw-tx raw-tx)
          (l2-tx +<.tx.raw-tx)
      ==
    =/  txs=(tree hist-tx)
      ?~  txs=(~(get by history) address)  ~
      u.txs
    =?  txs  ?=(^ txs)  +:(del:orh txs time)
    :-  [tx+[pend-tx status] ups]
    %+  ~(put by history)  address
    (put:orh txs [time roll-tx])
  [(flop updates) history]
--
