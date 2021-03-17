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
::  think.  Can an L2 star adopt an L1 planet?  I guess, but L1 wouldn't
::  know about it.  Should L1 check whether the escape target is on L2
::  for some reason?
::
::  TODO: make kid and net in point not a unit
::
::  TODO: consider the implications of having two operator lists, on L1
::  and L2.  Are they the same list, or different?
::
::  TODO: should we emit all the events azimuth.sol does?  might be
::  convenient for tracking edges?
::
::  TODO: decide about adopt/reject/detach.  right now i add specifying
::  the parent because it feels safer and cleaner, at the cost of four
::  bytes.  without specifying the parent, are there race conditions
::  involving multiple possible sponsors owned/managed by the same
::  address?
::
::  TODO: add nonces to txs.  how to keep track of them?  need replay
::  protection.  maybe we can add nonces to ships instead of addresses?
::  Then what about operators?
::
::  TODO: if sig fails to verify, skip instead of crashing
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
+$  effects      (list [ship udiff:point:jael])
+$  tx
  $%  [%transfer-point =ship =address reset=?]
      [%spawn =ship =address]
      [%set-dns-domains primary=@t secondary=@t tertiary=@t]
      [%set-operator operator=address add=?]
      [%configure-keys =ship encrypt=@ auth=@ crypto-suite=@ breach=?]
      [%escape =ship parent=ship]
      [%cancel-escape =ship parent=ship]
      [%adopt =ship parent=ship]
      [%reject =ship parent=ship]
      [%detach =ship parent=ship]
      [%set-management-proxy =ship =address]
      [%set-spawn-proxy =ship =address]
      [%set-voting-proxy =ship =address]
      [%set-transfer-proxy =ship =address]
  ==
::
+$  input
  [%bat batch=@]
  [%log =event-log:rpc:ethereum]
+$  verifier  $-([dat=@ v=@ r=@ s=@] pub=@)
--
::
=,  ethereum-types
|%
++  parse-batch
  |=  batch=@
  ^-  (list [address tx])
  ?~  batch
    ~
  =^  [signer=address =tx]  batch  (parse-tx batch)
  [[signer tx] $]
::
++  parse-tx
  |=  [batch=@ =verifier]
  ^-  [[address tx] rest=@]
  =/  batch  [0 batch]
  |^
  =^  sig  batch  (take 3 65)
  =/  signed-batch  batch
  =-  =/  signer=address
        (verify-tx sig (end [0 len] signed-batch) verifier)
      [[signer tx] rest]
  ^-  [=tx [len=@ rest=@]]
  =^  op   batch  (take 0 7)
  ?-    op
      %0
    :-  %transfer-point
    =^  reset=?         batch  (take 0)
    =^  =ship           batch  (take 3 4)
    =^  =address        batch  (take 3 20)
    [[ship address reset] batch]
  ::
      %1  [%spawn take-ship-address]
      %2
    :-  %configure-keys
    =^  breach=?        batch  (take 0)
    =^  =ship           batch  (take 3 4)
    =^  encrypt=@       batch  (take 3 32)
    =^  auth=@          batch  (take 3 32)
    =^  crypto-suite=@  batch  (take 3 4)
    [[ship encrypt auth crypto-suite breach] batch]
  ::
      %3   [%escape take-escape]
      %4   [%cancel-escape take-escape]
      %5   [%adopt take-escape]
      %6   [%reject take-escape]
      %7   [%detach take-escape]
      %8   [%set-management-proxy take-ship-address]
      %9  [%set-spawn-proxy take-ship-address]
      %10  [%set-voting-proxy take-ship-address]
      %11  [%set-transfer-proxy take-ship-address]
      %12
    :-  %set-dns-domains
    =^  pad=?           batch  (take 0)
    =^  primary=@t      batch  take-string
    =^  secondary=@t    batch  take-string
    =^  tertiary=@t     batch  take-string
    [[primary secondary tertiary] batch]
  ::
      %13
    :-  %set-operator
    =^  add=?           batch  (take 0)
    =^  =address        batch  (take 3 20)
    [[address add] batch]
  ==
  ::
  ::  Take a bite
  ::
  ++  take
    |=  =bite
    ^-  [@ _batch]
    :-  (end bite batch)
    :-  ?@  bite  (bex bite)
        (mul step.bite (bex bloq.bite))
    (rsh bite batch)
  ::  Dumb encoding of strings up to 255 bytes
  ::
  ++  take-string
    ^-  [string=@t _batch]
    =^  len=@      batch  (take 3)
    (take [3 len] batch)
  ::
  ::  Encode ship and address
  ::
  ++  take-ship-address
    ^-  [[ship address] _batch]
    =^  pad=?     batch  (take 0)
    =^  =ship     batch  (take 3 4)
    =^  =address  batch  (take 3 20)
    [ship address]
  ::  Encode escape-related txs
  ::
  ++  take-escape
    ^-  [[ship ship] _batch]
    =^  pad=?        batch  (take 0)
    =^  child=ship   batch  (take 3 4)
    =^  parent=ship  batch  (take 3 4)
    [child parent]
  ::
  ++  verify-tx
    |=  [sig=@ txdata=@ =verifier]
    ^-  address
    |^
    =^  v  sig  (take 3)
    =^  r  sig  (take 3 32)
    =^  s  sig  (take 3 32)
    %-  address-from-pub:key:ethereum
    (verifier txdata v r s)
    ::
    ++  take
      |=  =bite
      [(end bite sig) (rsh bite sig)]
    --
  --
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
:: Receive log from L1 transaction
::
++  receive-log
  |=  [=state log=event-log:rpc:ethereum]
  ^-  [(list [ship udiff:point:jael]) ^state]
  =*  log-name  i.topics.log
  ?:  =(log-name ^~((hash-log-name 'ChangedDns(string,string,string)')))
    ?>  ?=(~ t.topics.log)
    =/  words  (rip 8 data.log)
    ?>  ?=([a=@ @ b=@ @ c=@ @ @ @ @ ~] words)
    =*  one  &1.words
    =*  two  &3.words
    =*  tri  &5.words
    `state(dns (turn ~[one two tri] (cury swp 3)))
  ::
  ?:  =(log-name ^~((hash-log-name 'ApprovalForAll(address,address,bool)')))
    ?>  ?=([@ @ ~] t.topics.log)
    =*  owner     i.t.topics.log
    =*  operator  i.t.t.topics.log
    =/  approved  !data.log
    =-  `state(operators -)
    ?:  approved
      (~(put ju operators.state) owner operator)
    (~(del ju operators.state) owner operator)
  ::
  ::  The rest of the logs modify a particular ship, specified in the
  ::  second topic.  We fetch it, and insert the modification back into
  ::  our state.
  ::
  ?>  ?=([@ *] t.topics.log)
  =*  ship  i.t.topics.log
  =/  point  (get-point state ship)
  =-  [effects state(points.state (~(put by points.state) ship new-point))]
  ^-  [=effects new-point=point-state]
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
  !!
::
::  Receive batch of L2 transactions
::
++  receive-batch
  |=  [=state batch=@]
  =/  txs=(list [signer=address =tx])  (parse-batch batch.input)
  |-  ^-  [effects ^state]
  ?~  txs
    [~ state]
  =^  effects-1  state  (receive-tx state signer.i.txs tx.i.txs)
  =^  effects-2  state  $(txs t.txs)
  [(welp effects-1 effects-2) state]
::
::  Receive an individual L2 transaction
::
++  receive-tx
  |=  [=state =signer =tx]
  ^-  [effects ^state]
  |^
  ?-  +<.tx
    %set-dns-domains        (process-set-dns-domains +.tx)
    %set-operator           (process-set-operator +.tx)
    %spawn                  (process-spawn +.tx)
    %transfer-point         (w-point process-transfer-point +.tx)
    %configure-keys         (w-point-fx process-configure-keys +.tx)
    %escape                 (w-point process-escape +.tx)
    %cancel-escape          (w-point process-cancel-escape +.tx)
    %adopt                  (w-point-fx process-adopt +.tx)
    %reject                 (w-point process-reject +.tx)
    %detach                 (w-point-fx process-detach +.tx)
    %set-management-proxy   (w-point process-set-management-proxy +.tx)
    %set-spawn-proxy        (w-point process-set-spawn-proxy +.tx)
    %set-voting-proxy       (w-point process-set-voting-proxy +.tx)
    %set-transfer-proxy     (w-point process-set-transfer-proxy +.tx)
  ==
  ::
  ++  w-point-fx
    |*  [fun=$-([ship point-state *] [effects point-state]) =ship rest=*]
    ^-  [effects ^state]
    =/  point  (get-point state ship)
    ?>  ?=(%l2 -.point)
    =/  [=effects new-point=point-state]  (fun ship point rest)
    [effects state(points.state (~(put by points.state) ship new-point))]
  ::
  ++  w-point
    |*  [=ship fun=$-([ship point-state *] point-state) =ship rest=*]
    ^-  [effects ^state]
    =/  point  (get-point state ship)
    ?>  ?=(%l2 -.point)
    =/  new-point=point-state  (fun ship point rest)
    `state(points.state (~(put by points.state) ship new-point))
  ::
  ++  process-set-dns-domains
    |=  [primary=@t secondary=@t tertiary=@t]
    ?>  =(signer 0x0)  ::  TODO: who?
    `state(dns ~[primary secondary tertiary])
  ::
  ++  process-set-operator
    |=  [operator=address add=?]
    =-  `state(operators -)
    ?<  =(0x0 operator)
    ?:  add
      (~(put ju operators.state) signer operator)
    (~(del ju operators.state) signer operator)
  ::
  ++  process-transfer-point
    |=  [=ship point=point-state to=address reset=?]
    ::  Assert ship is on L2
    ::
    ::  Assert signer is owner or transfer prxoy
    ::
    ?>  ?|  =(owner.own.point signer)
            =(transfer-proxy.own.point signer)
        ==
    ::  Execute transfer
    ::
    =:  owner.own.point           to
        transfer-proxy.own.point  *address
      ==
    ::  Execute reset if requested
    ::
    ?.  reset
      point
    ::
    =?  net.point  &(?=(^ net.point) (gth life.u.net.point 0))
      `[+(life) 0 +(continuity-number) sponsor escape]:u.net.point
    =.  own.point  [owner.own.point *address *address *address]
    =.  spawn-proxy.kid.point  *address
    point
  ::
  ++  process-spawn
    |=  [=ship to=address]
    =/  parent=^ship  (^sein:title ship)
    ::  Assert parent is on L2
    ::
    =/  parent-point  (get-point state parent)
    ?>  ?=(?(%l2 %spawn) -.parent-point)
    ::  Assert signer is owner or spawn proxy
    ::
    ?>  ?|  =(owner.own.parent-point signer)
            =(spawn-proxy.kid.parent-point signer)
        ==
    ::  Assert child not already spawned
    ::
    ::  TODO: verify this means the ship exists on neither L1 nor L2
    ::
    ?<  (~(has by points.state) ship)
    ::  Assert one-level-down
    ::
    ?>  =(+((get-point-size parent)) (get-point-size ship))
    ::  TODO check spawnlimit
    ::
    =.  points.state
      %^  ~(put by points.state)  ship  %l2
      ?:  =(to signer)
        ::  If spawning to self, just do it
        ::
        %*  .  *point:azimuth-types
          owner.own  to
        ==
      ::  Else spawn to parent and set transfer proxy
      ::
      %*  .  *point:azimuth-types
        owner.own           owner.own.parent-point
        transfer-proxy.own  to
      ==
    [~ points]
  ::
  ++  process-configure-keys
    |=  [=ship point=point-state encrypt=@ auth=@ crypto-suite=@ breach=?]
    ::
    ?>  ?|  =(owner.own.point signer)
            =(management-proxy.own.point signer)
        ==
    ::
    =?  continuity-number.net.point  breach  +(continuity-number.net.point)
    =/  rift-effects  ?:(breach [ship %rift continuity-number.net.point]~ ~)
    ::
    =/  =pass  (pass-from-eth 32^encrypt 32^auth crypto-suite)
    =?  net.point  !=(pass.net.point pass)  ::  TODO: check crypto-suite
      net.point(life +(life.net.point), pass pass)
    =/  keys-effects
      ?:  =(pass.net.point pass)
        ~
      [ship %keys life.net.point crypto-suite pass]~
    ::
    [(welp rift-effects keys-effects) point]
  ::
  ++  process-escape
    |=  [=ship point=point-state parent=ship]
    ?>  ?|  =(owner.own.point signer)
            =(management-proxy.own.point signer)
        ==
    ::
    ::  TODO: don't allow "peer escape"?
    ?>  =(+((get-point-size parent)) (get-point-size ship))
    ::
    point(escape.net.point `parent)  ::  TODO: omitting a lot of source material?
  ::
  ++  process-cancel-escape
    |=  [=ship point=point-state parent=ship]
    ?>  ?|  =(owner.own.point signer)
            =(management-proxy.own.point signer)
        ==
    ::
    point(escape.net.point ~)
  ::
  ++  process-adopt
    |=  [=ship point=point-state parent=ship]
    =/  parent-point  (get-point state parent) :: TODO: assert child/parent on L2?
    ::
    ?>  ?|  =(owner.own.parent-point signer)
            =(management-proxy.own.parent-point signer)
        ==
    ::
    ?>  =(escape.net.point `ship)
    :-  [ship %spon `parent]~
    point(escape.net.point ~, sponsor.net.point [%& parent])
  ::
  ++  process-reject
    |=  [=ship point=point-state parent=ship]
    =/  parent-point  (get-point state parent) :: TODO: assert child/parent on L2?
    ::
    ?>  ?|  =(owner.own.parent-point signer)
            =(management-proxy.own.parent-point signer)
        ==
    ::
    point(escape.net.point ~)
  ::
  ++  process-detach
    |=  [=ship point=point-state parent=ship]
    =/  parent-point  (get-point state parent) :: TODO: assert child/parent on L2?
    ::
    ?>  ?|  =(owner.own.parent-point signer)
            =(management-proxy.own.parent-point signer)
        ==
    ::
    :-  [ship %spon ~]~
    point(has.sponsor.net.point %|)
  ::
  ++  process-set-management-proxy
    |=  [=ship point=point-state =address]
    ?>  ?|  =(owner.own.point signer)
            =(management-proxy.own.parent-point signer)
        ==
    ::
    point(management-proxy.own.point address)
  ::
  ++  process-set-spawn-proxy
    |=  [=ship point=point-state =address]
    ?>  ?|  =(owner.own.point signer)
            =(spawn-proxy.own.parent-point signer)
        ==
    ::
    point(spawn-proxy.own.point address)
  ::
  ++  process-set-voting-proxy
    |=  [=ship point=point-state =address]
    ?>  ?|  =(owner.own.point signer)
            =(voting-proxy.own.parent-point signer)
        ==
    ::
    point(voting-proxy.own.point address)
  ::
  ++  process-set-transfer-proxy
    |=  [=ship point=point-state =address]
    ?>  ?|  =(owner.own.point signer)
            =(transfer-proxy.own.parent-point signer)
        ==
    ::
    point(transfer-proxy.own.point address)
  --
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
  (receive-log state event-log.input)
::  Received batch
::
(receive-batch batch.input state)
