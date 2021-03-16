::  L1 contract changes:
::  - Enforce that once spawn proxy is set to deposit address, it can't
::    switched back
::  - Enforce that once spawn proxy is set to deposit address, you can't
::    spawn children
::  - Possibly the same for approveForAll
::  - Enforce that only ownership key can set spawn proxy to rollup
::  - Disallow depositing galaxy to L2
::
::  TODO: maybe split out a generic "modify state" core, and have both
::  L1 and L2 transactions reduce to that?  Would need to see what that
::  looks like for L2 transactions, since they need to assert various
::  things, but maybe those reduce to just asserting the signer is
::  correct?  Also need to handle depositing and managing l1 vs l2 to
::  some extent.
::
::  TODO: can an L1 star adopt an L2 planet?  It's not obvious how --
::  maybe they need to adopt as an L2 transaction?  That sounds right I
::  think.
::
::  TODO: make kid and net in point not a unit
::
/+  ethereum, azimuth
::  Constants
::
|%
::  Transfers on L1 to this address count as depositing to L2
::
++  deposit-address  0x1234.5678.9012.3456.7890.1234.5678.9012.3456.7890
--
::  Types
=,  ethereum-types
|%
+$  state
  $:  =points
      =operators
      dns=(list @t)
  ==
+$  points       (map ship point-state)
+$  operators    (jug address address)
+$  point-state  [dominion=?(%l1 %l2 %spawn) point:azimuth-types]
+$  tx
  $%  [%transfer-point =ship =address reset=?]
      [%spawn =ship =address]
      [%set-operator owner=address operator=address add=?]
      [%configure-keys =ship encrypt=@ auth=@ crypto-version=@ breach=?]
      [%escape =ship parent=ship]
      [%cancel-escape =ship parent=ship]
      [%adopt =ship]
      [%reject =ship]
      [%detach =ship]
      [%set-management-proxy =ship =address]
      [%set-spawn-proxy =ship =address]
      [%set-voting-proxy =ship =address]
      [%set-transfer-proxy =ship =address]
      [%set-dns-domains primary=@t secondary=@t tertiary=@t]
  ==
+$  input
  [%bat batch=@]
  [%log =event-log:rpc:ethereum]
+$  verifier  $-([dat=@ v=@ r=@ s=@] pub=@)
--
::
=,  ethereum-types
|%
++  take
  |=  [=bite txdata=@]
  [(end bite txdata) (rsh bite txdata)]
::
++  parse-tx
  |=  txdata=@
  ^-  tx
  =^  op  txdata  (take [0 7] txdata)
  ?-    op
      %0
    :-  %transfer-point
    =^  reset=?   txdata  (take [0 1] txdata)
    =^  =ship     txdata  (take [3 4] txdata)
    =^  =address  txdata  (take [3 20] txdata)
    ?>  =(0 txdata)
    [ship address reset]
  ::
      %1
    :-  %spawn
    =^  pad=?     txdata  (take [0 1] txdata)
    =^  =ship     txdata  (take [3 4] txdata)
    =^  =address  txdata  (take [3 20] txdata)
    ?>  =(0 txdata)
    [ship address]
  ==
::
++  verify-tx
  |=  [txdata=@ =verifier]
  ^-  [address @]
  =^  v  txdata  (take 3 txdata)
  =^  r  txdata  (take [3 32] txdata)
  =^  s  txdata  (take [3 32] txdata)
  :_  txdata
  %-  address-from-pub:key:ethereum
  (verifier txdata v r s)
::
++  get-point-size
  |=  =ship
  ^-  @
  ?:  (lth ship 0x100)     0
  ?:  (lth ship 0X1.0000)  1
  2
::
++  hash-log-name
  |=  name=@t
  ^-  @ux
  (keccack-256:keccak:crypto (as-octs:mimes:html name))
::
++  get-point
  |=  [=state =ship]
  ^-  point-state
  =/  existing  (~(get by points.state) ship)
  ?^  existing
    u.existing
  :_  *point:azimuth-types
  ?+    (clan:title ship)  ~|(%strange-point !!)
      %czar  %l1
      ?(%king %duke)
    =/  existing-parent  $(ship (^sein:title ship))
    ?-  dominion.existing-parent
      %l1     %l1
      %l2     %l2
      %spawn  %l2
    ==
  ==
--
|%
:: Received log from L1 transaction
::
++  process-log
  |=  [log=event-log:rpc:ethereum =state]
  ^-  [(list [ship udiff:point:jael]) ^state]
  =*  log-name  i.topics.log
  ?:  =(log-name ^~((hash-log-name 'ChangedDns(string,string,string)')))
    ?>  ?=(~ t.topics.log)
    =/  words  (rip 8 data.log)
    ?>  ?=([a=@ @ b=@ @ c=@ @ @ @ @ ~] words)
    =*  one  &1.words
    =*  two  &3.words
    =*  tri  &5.words
    :-  ~
    state(dns (turn ~[one two tri] (cury swp 3)))
  ::
  ?:  =(log-name ^~((hash-log-name 'ApprovalForAll(address,address,bool)')))
    ?>  ?=([@ @ ~] t.topics.log)
    =*  ship      i.t.topics.log
    =*  operator  i.t.t.topics.log
    =/  approved  !data.log
    =/  point  (get-point state ship)
    ?>  ?=(%l1 -.point)
    =-  `state(operators -)
    ?:  approved
      (~(put ju operators.state) ship operator)
    (~(del ju operators.state) ship operator)
  ::
  ::  The rest of the logs modify a particular ship, specified in the
  ::  second topic.  We fetch it, and insert the modification back into
  ::  our state.
  ::
  ?>  ?=([@ *] t.topics.log)
  =*  ship  i.t.topics.log
  =/  point  (get-point state ship)
  =-  [effects state(points.state (~(put by points.state) ship new-point))]
  ^-  [effects=(list [ship udiff:point:jael]) new-point=point-state]
  ::
  ?:  =(log-name ^~((hash-log-name 'ChangedSpawnProxy(uint32,address)')))
    ?>  ?=(%l1 -.point)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    :-  ~
    ?.  =(deposit-address to)
      point(spawn-proxy.kid to)
    point(dominion %spawn)
  ::
  ::  The rest can be done by any ship on L1, even if their spawn proxy
  ::  is set to L2
  ::
  ?<  ?=(%l2 -.point)
  ::
  ?:  =(log-name ^~((hash-log-name 'BrokeContinuity(uint32,uint32)')))
    ?>  ?=(~ t.t.topics.log)
    =*  rift  data.log
    :-  [ship %rift rift]~
    point(continuity-number.net.point rift)
  ::
  =/  changed-keys-hash
    ^~((hash-log-name 'ChangedKeys(uint32,bytes32,bytes32,uint32,uint32)'))
  ?:  =(log-name changed-keys-hash)
    ?>  ?=(~ t.t.topics.log)
    =/  words  (rip 8 data.log)
    ?>  ?=([@ @ @ @ ~] words)
    =*  encryption      i.words
    =*  authentication  i.t.words
    =*  crypto-suite    i.t.t.words  ::  TODO: store in state, or add to pass
    =*  life            i.t.t.t.words
    =/  =pass  (pass-from-eth 32^encryption 32^authentication crypto-suite)
    :-  [ship %keys life crypto-suite pass]~
    point(life.net.point life, pass.net.point pass)
  ::
  ?:  =(log-name ^~((hash-log-name 'EscapeAccepted(uint32,uint32)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent  i.t.t.topics.log
    :-  [ship %spon `parent]~
    point(escape.net.point ~, sponsor.net.point [%& parent])
  ::
  ?:  =(log-name ^~((hash-log-name 'LostSponsor(uint32,uint32)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent  i.t.t.topics.log
    :-  [ship %spon ~]~
    point(has.sponsor.net.point %|)
  ::
  ::  The rest do not produce effects
  ::
  :-  ~
  ::
  ?:  =(log-name ^~((hash-log-name 'EscapeRequested(uint32,uint32)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent  i.t.t.topics.log
    point(escape.net.point `parent)
  ::
  ?:  =(log-name ^~((hash-log-name 'EscapeCanceled(uint32,uint32)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent  i.t.t.topics.log
    point(escape.net.point ~)
  ::
  ?:  =(log-name ^~((hash-log-name 'OwnerChanged(uint32,address)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    ::
    ?:  =(deposit-address to)
      point(dominion.point %l2)
    point(owner.own.point to)
  ::
  ?:  =(log-name ^~((hash-log-name 'ChangedTransferProxy(uint32,address)')))  !!
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    point(transfer-proxy.own to)
  ::
  ?:  =(log-name ^~((hash-log-name 'ChangedManagementProxy(uint32,address)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    point(management-proxy.own to)
  ::
  ?:  =(log-name ^~((hash-log-name 'ChangedVotingProxy(uint32,address)')))    !!
    ?>  ?=([@ ~] t.topics.log)
    =*  to  i.t.t.topics.log
    point(voting-proxy.own to)
  ::
  ~|  [%unknown-log log]
  !!  ::  TODO: handle operators
--
::
::  State transition function
::
::  TODO: wrap in mule to no-op instead of crash?  perhaps that's better
::  as part of the spec?  it's not a clear part of the nock spec, though
::
=,  ethereum-types
|=  [=input =state =verifier]
^-  [(list [ship udiff:point:jael]) ^state]
?:  ?=(%log -.input)
  :: Received log from L1 transaction
  ::
  (process-log event-log.input state)
::  Received batch
::
=/  txs=(list @)  (split-batch batch.input)
=^  signer=address  txdata  (verify-tx txdata verifier)
|^
=/  =tx  (parse-tx txdata)
?-  -.tx
  %transfer-point  (process-transfer-point +.tx)
  %spawn           (process-spawn +.tx)
==
::
++  process-transfer-point
  |=  tx=[=ship to=address reset=?]
  ::  Assert ship is on L2
  ::
  =/  point  (get-point state ship.tx)
  ?>  ?=(%l2 -.point)
  ::  Assert signer is owner or transfer prxoy
  ::
  ?>  |(=(owner.own.point signer) =(transfer-proxy.own.point signer))
  ::  Execute transfer
  ::
  =:  owner.own.point           to
      transfer-proxy.own.point  *address
    ==
  ::  Execute reset if requested
  ::
  ::  Save point now, but we might overwrite later
  =.  points.state  (~(put by points) ship.tx point)
  ?.  reset
    [~ state]
  ::
  =?  net.point  &(?=(^ net.point) (gth life.u.net.point 0))
    `[+(life) 0 +(continuity-number) sponsor escape]:u.net.point
  =.  own.point  [owner.own.point *address *address *address]
  =.  spawn-proxy.kid.point  *address
  =.  points.state  (~(put by points.state) ship.tx point)
  [~ state]
::
++  process-spawn
  |=  tx=[=ship to=address]
  =/  sen=^ship  (^sein:title ship.tx)
  ::  Assert parent is on L2
  ::
  =/  parent  (get-point state sen)
  ?>  ?=(?(%l2 %spawn) -.parent)
  ::  Assert signer is owner or spawn proxy
  ::
  ?>  ?|  =(owner.own.parent signer)
          =(spawn-proxy.kid.parent signer)
      ==
  ::  Assert child not already spawned
  ::
  ::  TODO: verify this means the ship exists on neither L1 nor L2
  ::
  ?<  (~(has by points.state) ship.tx)
  ::  Assert one-level-down
  ::
  ?>  =(+((get-point-size sen)) (get-point-size ship.tx))
  ::  TODO check spawnlimit
  ::
  =.  points.state
    %^  ~(put by points.state)  ship.tx  %l2
    ?:  =(to.tx signer)
      ::  If spawning to self, just do it
      ::
      %*  .  *point:azimuth-types
        owner.own  to.tx
      ==
    ::  Else spawn to parent and set transfer proxy
    ::
    %*  .  *point:azimuth-types
      owner.own           owner.own.parent
      transfer-proxy.own  to.tx
    ==
  [~ points]
--
