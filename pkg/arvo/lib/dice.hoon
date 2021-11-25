::  dice: helper functions for L2 Rollers
::
/-  *dice
/+  naive, *naive-transactions
::
|%
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
  =^  *  points.nas
    (increment-nonce:naive nas from.tx.raw-tx)
  ?~  nex=(receive-tx:naive nas tx.raw-tx)
    ~&  >>>  [%receive-tx %failed]
    [force ~ ?:(force nas cache) indices]
  =*  new-nas   +.u.nex
  =*  effects   -.u.nex
  =^  updates   indices
    (point-effects effects cache new-nas [own spo]:indices)
  [& updates new-nas indices]
::
++  point-effects
  |=  [=effects:naive cache=^state:naive nas=^state:naive =indices]
  ^-  [(list update) indices=_indices]
  =;  [updates=(list update) indices=_indices]
    [(flop updates) indices]
  %+  roll  effects
  |=  [=diff:naive updates=(list update) indices=_indices]
  =,  orm:naive
  ?.  ?=([%point *] diff)  [updates indices]
  =*  ship      ship.diff
  =*  sponsors  spo.indices
  =*  owners    own.indices
  =/  old=(unit point:naive)
    (get points.cache ship)
  =/  new=point:naive
    (need (get points.nas ship))
  =*  event  +>.diff
  |^
  =.  sponsors  sponsorship
  =^  update    owners  ownership
  [(welp update updates) indices]
  ::
  ++  sponsorship
    ^+  sponsors
    ?+    -.event  sponsors
        %owner
      ?^  old
        ::  ownership change
        ::
        sponsors
      ::  owner event with ?=(~ old) is a spawn
      ::
      =*  parent  who.sponsor.net.new
      ?:  =(parent ship)  sponsors
      %+  ~(put by sponsors)  parent
      ?~  sponsor=(~(get by sponsors) parent)
        :_  *(set @p)
        (~(put in *(set @p)) ship)
      :_  requests.u.sponsor
      (~(put in residents.u.sponsor) ship)
    ::
        %escape
      ?~  old     sponsors
      =*  from    who.sponsor.net.u.old
      =*  to      to.event
      =*  escape  escape.net.u.old
      ?~  to
        :: cancel/reject escape
        ::
        ?~  escape  sponsors
        ?~  receiver=(~(get by sponsors) u.escape)
          sponsors
        %+  ~(put by sponsors)  u.escape
        :-  residents.u.receiver
        (~(del in requests.u.receiver) ship)
      ::  normal escape
      ::
      %+  ~(put by sponsors)  u.to
      ?~  receiver=(~(get by sponsors) u.to)
        :-  *(set @p)
        (~(put in *(set @p)) ship)
      :-  residents.u.receiver
      (~(put in requests.u.receiver) ship)
    ::
        %sponsor
      ?~  old   sponsors
      =*  from  who.sponsor.net.u.old
      =*  to    sponsor.event
      =/  previous  (~(get by sponsors) from)
      ?~  to
        :: detach
        ::
        ?~  previous  sponsors
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
      sponsors
    ==
  ::
  ++  ownership
    ^-  (quip update _owners)
    =;  [to=(unit owner) from=(unit owner)]
      =?  owners  &(?=(^ from) !=(address.u.from 0x0))
        (~(del ju owners) u.from ship)
      ?:  ?|  =(~ to)
              &(?=(^ to) =(address.u.to 0x0))
          ==
        [~ owners]
      ?~  to  [~ owners]
      :-  [%point ship new u.to from]~
      (~(put ju owners) u.to ship)
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
  --
::
++  get-owner
  |=  [=point:naive =proxy:naive]
  ^-  [=nonce:naive _point]
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
