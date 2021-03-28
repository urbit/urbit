::  L1 contract changes:
::  - Enforce that once spawn proxy is set to deposit address, it can't
::    switched back
::  - Enforce that once spawn proxy is set to deposit address, you can't
::    spawn children
::  - Possibly the same for approveForAll
::  - Enforce that only ownership key can set spawn proxy to rollup.
::    maybe not though
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
::  TODO: is it possible to spawn directly to the deposit address?  if
::  so, should we find its parent's owner to control it?
::
::  TODO: should we add any protection in the L1 contracts that you
::  don't deposit from a contract?
::
::  TODO: check operator anywhere you check for owner
::
::  TODO: batch tx type to reduce signatures
::
::  TODO: how to implement claims on L2?
::
::  TODO: on L1, when depositing, clear proxies (maybe require reset)
::
/+  ethereum
::  Constants
::
|%
::  Transfers on L1 to this address count as depositing to L2
::
++  deposit-address  0x1234.5678.9012.3456.7890.1234.5678.9012.3456.7890
--
::  Types
|%
::  ethereum address, 20 bytes.
::
+$  address  @ux
++  point
  $:  ::  domain
      ::
      dominion=?(%l1 %l2 %spawn)
    ::
      ::  ownership
      ::
      $=  own
      $:  owner=address
          management-proxy=address
          voting-proxy=address
          transfer-proxy=address
          spawn-proxy=address
      ==
    ::
      ::  networking
      ::
      $=  net
      $:  =life
          =pass
          rift=@ud
          sponsor=[has=? who=@p]
          escape=(unit @p)
      ==
  ==
::
::  TODO: add effects for all changes
::
++  diff
  $:  =ship
      $%  [%rift =rift]
          [%keys =life crypto-suite=@ud =pass]
          [%spon sponsor=(unit @p)]
  ==  ==
::
+$  state
  $:  =points
      =operators
      dns=(list @t)
  ==
+$  points       (map ship point)
+$  operators    (jug address address)
+$  effects      (list diff)
+$  tx
  $%  [%transfer-point =ship =address reset=?]
      [%spawn =ship =address]
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
  $%  [%bat batch=@]
      [%log =event-log:rpc:ethereum]
  ==
::  ECDSA verifier
::
+$  verifier  $-([dat=@ v=@ r=@ s=@] (unit address))
--
::
|%
++  parse-batch
  |=  [=verifier batch=@]
  ^-  (list [address tx])
  ?~  batch
    ~
  =^  parsed=(unit [signer=address =tx])  batch  (parse-tx batch verifier)
  ?~  parsed
    $
  [u.parsed $]
::
::  TODO: change batch to be a cursor to avoid allocating atoms
::
++  parse-tx
  |=  [batch=@ =verifier]
  ^-  [(unit [address tx]) rest=@]
  =/  batch  [len=0 rest=batch]
  |^
  =^  sig  batch  (take 3 65)
  =/  signed-batch  +.batch
  =-  =/  signer=(unit address)
        (verify-tx sig (end [0 len] signed-batch))
      ?~  signer
        [~ rest]
      [`[u.signer tx] rest]
  ^-  [=tx [len=@ rest=@]]
  =^  op   batch  (take 0 7)
  ?+    op  ~|([%strange-opcode op] !!)
      %0
    =^  reset=@         batch  (take 0)
    =^  =ship           batch  (take 3 4)
    =^  =address        batch  (take 3 20)
    [[%transfer-point ship address =(0 reset)] batch]
  ::
      %1   =^(res batch take-ship-address [[%spawn res] batch])
      %2
    =^  breach=@        batch  (take 0)
    =^  =ship           batch  (take 3 4)
    =^  encrypt=@       batch  (take 3 32)
    =^  auth=@          batch  (take 3 32)
    =^  crypto-suite=@  batch  (take 3 4)
    [[%configure-keys ship encrypt auth crypto-suite =(0 breach)] batch]
  ::
      %3   =^(res batch take-escape [[%escape res] batch])
      %4   =^(res batch take-escape [[%cancel-escape res] batch])
      %5   =^(res batch take-escape [[%adopt res] batch])
      %6   =^(res batch take-escape [[%reject res] batch])
      %7   =^(res batch take-escape [[%detach res] batch])
      %8   =^(res batch take-ship-address [[%set-management-proxy res] batch])
      %9   =^(res batch take-ship-address [[%set-spawn-proxy res] batch])
      %10  =^(res batch take-ship-address [[%set-voting-proxy res] batch])
      %11  =^(res batch take-ship-address [[%set-transfer-proxy res] batch])
  ==
  ::
  ::  Take a bite
  ::
  ++  take
    |=  =bite
    ^-  [@ _batch]
    :-  (end bite +.batch)
    :-  %+  add  -.batch
        ?@  bite  (bex bite)
        (mul step.bite (bex bloq.bite))
    (rsh bite +.batch)
  ::  Encode ship and address
  ::
  ++  take-ship-address
    ^-  [[ship address] _batch]
    =^  pad=@     batch  (take 0)
    =^  =ship     batch  (take 3 4)
    =^  =address  batch  (take 3 20)
    [[ship address] batch]
  ::  Encode escape-related txs
  ::
  ++  take-escape
    ^-  [[ship ship] _batch]
    =^  pad=@        batch  (take 0)
    =^  child=ship   batch  (take 3 4)
    =^  parent=ship  batch  (take 3 4)
    [[child parent] batch]
  ::  Verify signature and produce signer address
  ::
  ++  verify-tx
    |=  [sig=@ txdata=@]
    ^-  (unit address)
    |^
    =^  v  sig  (take 3)
    =^  r  sig  (take 3 32)
    =^  s  sig  (take 3 32)
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
  ?:  (lth ship 0x1.0000)  1
  2
::
++  hash-log-name
  |=  name=@t
  ^-  @ux
  (keccak-256:keccak:crypto (as-octs:mimes:html name))
::
++  pass-from-eth
  |=  [enc=octs aut=octs sut=@ud]
  ^-  pass
  %^  cat  3  'b'
  ?.  &(=(1 sut) =(p.enc 32) =(p.aut 32))
    (cat 8 0 0)  ::  TODO: fix
  (cat 8 q.aut q.enc)
::
++  get-point
  |=  [=state =ship]
  ^-  point
  =/  existing  (~(get by points.state) ship)
  ?^  existing
    u.existing
  %*    .  *point
      dominion
    ::  TODO: use get-point-size
    ::
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
  ==
--
|%
:: Receive log from L1 transaction
::
++  receive-log
  |=  [=state log=event-log:rpc:ethereum]
  ^-  [effects ^state]
  =*  log-name  i.topics.log
  ?:  =(log-name ^~((hash-log-name 'ChangedDns(string,string,string)')))
    ?>  ?=(~ t.topics.log)
    =/  words  (rip 8 data.log)
    ?>  ?=([c=@ @ b=@ @ a=@ @ @ @ @ ~] words)  ::  TODO: not always true
    =*  one  &5.words
    =*  two  &3.words
    =*  tri  &1.words
    `state(dns (turn ~[one two tri] (cury swp 3)))
  ::
  ?:  =(log-name ^~((hash-log-name 'ApprovalForAll(address,address,bool)')))
    ?>  ?=([@ @ ~] t.topics.log)
    =*  owner     i.t.topics.log
    =*  operator  i.t.t.topics.log
    =/  approved  !=(0 data.log)
    =-  `state(operators -)
    ?:  approved
      (~(put ju operators.state) owner operator)
    (~(del ju operators.state) owner operator)
  ::
  ::  The rest of the logs modify a particular ship, specified in the
  ::  second topic.  We fetch it, and insert the modification back into
  ::  our state.
  ::
  ::  TODO: cast in =* instead of after
  ::
  ?>  ?=([@ *] t.topics.log)
  =*  ship=@  i.t.topics.log
  =/  point  (get-point state ship)
  =-  [effects state(points (~(put by points.state) ship new-point))]
  ^-  [=effects new-point=^point]
  ::
  ?:  =(log-name ^~((hash-log-name 'ChangedSpawnProxy(uint32,address)')))
    ?>  ?=(%l1 -.point)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    ::  Depositing to L2 is represented by a spawn proxy change on L1,
    ::  but it doesn't change the actual spawn proxy.
    ::
    :-  ~
    ?:  =(deposit-address to)
      point(dominion %spawn)
    point(spawn-proxy.own to)
  ::
  ::  The rest can be done by any ship on L1, even if their spawn proxy
  ::  is set to L2
  ::
  ?<  ?=(%l2 -.point)
  ::
  ?:  =(log-name ^~((hash-log-name 'BrokeContinuity(uint32,uint32)')))
    ?>  ?=(~ t.t.topics.log)
    =*  rift=@  data.log
    :-  [ship %rift rift]~
    point(rift.net rift)
  ::
  =/  changed-keys-hash
    ^~((hash-log-name 'ChangedKeys(uint32,bytes32,bytes32,uint32,uint32)'))
  ?:  =(log-name changed-keys-hash)
    ?>  ?=(~ t.t.topics.log)
    =/  words  (rip 8 data.log)
    ?>  ?=([@ @ @ @ ~] words)  :: TODO: reverse order?
    =*  encryption=@      i.words
    =*  authentication=@  i.t.words
    =*  crypto-suite=@    i.t.t.words  ::  TODO: store in state, or add to pass
    =*  life=@            i.t.t.t.words
    =/  =pass  (pass-from-eth 32^encryption 32^authentication crypto-suite)
    :-  [ship %keys life crypto-suite pass]~
    point(life.net life, pass.net pass)
  ::
  ?:  =(log-name ^~((hash-log-name 'EscapeAccepted(uint32,uint32)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent=@  i.t.t.topics.log
    :-  [ship %spon `parent]~
    point(escape.net ~, sponsor.net [%& parent])
  ::
  ?:  =(log-name ^~((hash-log-name 'LostSponsor(uint32,uint32)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent  i.t.t.topics.log
    :-  [ship %spon ~]~
    point(has.sponsor.net %|)
  ::
  ::  The rest do not produce effects
  ::
  :-  ~
  ::
  ?:  =(log-name ^~((hash-log-name 'EscapeRequested(uint32,uint32)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent=@  i.t.t.topics.log
    point(escape.net `parent)
  ::
  ?:  =(log-name ^~((hash-log-name 'EscapeCanceled(uint32,uint32)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent  i.t.t.topics.log
    point(escape.net ~)
  ::
  ?:  =(log-name ^~((hash-log-name 'OwnerChanged(uint32,address)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    ::  Depositing to L2 is represented by an ownership change on L1,
    ::  but it doesn't change who actually owns the ship.
    ::
    ?:  =(deposit-address to)
      point(dominion %l2)
    point(owner.own to)
  ::
  ?:  =(log-name ^~((hash-log-name 'ChangedTransferProxy(uint32,address)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    point(transfer-proxy.own to)
  ::
  ?:  =(log-name ^~((hash-log-name 'ChangedManagementProxy(uint32,address)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    point(management-proxy.own to)
  ::
  ?:  =(log-name ^~((hash-log-name 'ChangedVotingProxy(uint32,address)')))
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    point(voting-proxy.own to)
  ::
  ~&  [%unknown-log log]
  point  ::  TODO: crash?
::
::  Receive batch of L2 transactions
::
++  receive-batch
  |=  [=verifier =state batch=@]
  =/  txs=(list [signer=address =tx])  (parse-batch verifier batch)
  |-  ^-  [effects ^state]
  ?~  txs
    [~ state]
  =^  effects-1  state  (receive-tx state i.txs)
  =^  effects-2  state  $(txs t.txs)
  [(welp effects-1 effects-2) state]
::
::  Receive an individual L2 transaction
::
++  receive-tx
  |=  [=state signer=address =tx]
  |^
  ^-  [effects ^state]
  ?-  -.tx
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
    |*  [fun=$-([ship point *] [effects point]) =ship rest=*]
    ^-  [effects ^state]
    =/  point  (get-point state ship)
    ?>  ?=(%l2 -.point)
    =/  [=effects new-point=^point]  (fun ship point rest)
    [effects state(points (~(put by points.state) ship new-point))]
  ::
  ++  w-point
    |*  [fun=$-([ship point *] point) =ship rest=*]
    ^-  [effects ^state]
    =/  point  (get-point state ship)
    ?>  ?=(%l2 -.point)
    =/  new-point  (fun ship point rest)
    `state(points (~(put by points.state) ship new-point))
  ::
  ++  process-transfer-point
    |=  [=ship =point to=address reset=?]
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
    =?  net.point  (gth life.net.point 0)
      [+(life) 0 +(rift) sponsor escape]:net.point
    =.  own.point  [owner.own.point *address *address *address *address]
    point
  ::
  ++  process-spawn
    |=  [=ship to=address]
    ^-  [effects ^state]
    =/  parent=^ship  (^sein:title ship)
    ::  Assert parent is on L2
    ::
    =/  parent-point  (get-point state parent)
    ?>  ?=(?(%l2 %spawn) -.parent-point)
    ::  Assert signer is owner or spawn proxy
    ::
    ?>  ?|  =(owner.own.parent-point signer)
            =(spawn-proxy.own.parent-point signer)
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
      %+  ~(put by points.state)  ship
      ?:  =(to signer)
        ::  If spawning to self, just do it
        ::
        %*  .  *point
          dominion   %l2
          owner.own  to
        ==
      ::  Else spawn to parent and set transfer proxy
      ::
      %*  .  *point
        dominion            %l2
        owner.own           owner.own.parent-point
        transfer-proxy.own  to
      ==
    `state
  ::
  ++  process-configure-keys
    |=  [=ship =point encrypt=@ auth=@ crypto-suite=@ breach=?]
    ::
    ?>  ?|  =(owner.own.point signer)
            =(management-proxy.own.point signer)
        ==
    ::
    =^  rift-effects  rift.net.point
      ?.  breach
        `rift.net.point
      [[ship %rift +(rift.net.point)]~ +(rift.net.point)]
    ::
    =/  =pass  (pass-from-eth 32^encrypt 32^auth crypto-suite)
    =?  net.point  !=(pass.net.point pass)  ::  TODO: check crypto-suite
      net.point(life +(life.net.point), pass pass)
    =/  keys-effects
      ?:  =(pass.net.point pass)  ::  TODO: check will always be true
        ~
      [ship %keys life.net.point crypto-suite pass]~
    ::
    [(welp rift-effects keys-effects) point]
  ::
  ++  process-escape
    |=  [=ship =point parent=ship]
    ?>  ?|  =(owner.own.point signer)
            =(management-proxy.own.point signer)
        ==
    ::  TODO: don't allow "peer escape"?
    ::
    ?>  =(+((get-point-size parent)) (get-point-size ship))
    ::
    point(escape.net `parent)  ::  TODO: omitting a lot of source material?
  ::
  ++  process-cancel-escape
    |=  [=ship =point parent=ship]
    ?>  ?|  =(owner.own.point signer)
            =(management-proxy.own.point signer)
        ==
    ::
    point(escape.net ~)
  ::
  ++  process-adopt
    |=  [=ship =point parent=ship]
    =/  parent-point  (get-point state parent) :: TODO: assert child/parent on L2?
    ::
    ?>  ?|  =(owner.own.parent-point signer)
            =(management-proxy.own.parent-point signer)
        ==
    ::
    ?>  =(escape.net.point `ship)
    :-  [ship %spon `parent]~
    point(escape.net ~, sponsor.net [%& parent])
  ::
  ++  process-reject
    |=  [=ship =point parent=ship]
    =/  parent-point  (get-point state parent) :: TODO: assert child/parent on L2?
    ::
    ?>  ?|  =(owner.own.parent-point signer)
            =(management-proxy.own.parent-point signer)
        ==
    ::
    point(escape.net ~)
  ::
  ++  process-detach
    |=  [=ship =point parent=ship]
    =/  parent-point  (get-point state parent) :: TODO: assert child/parent on L2?
    ::
    ?>  ?|  =(owner.own.parent-point signer)
            =(management-proxy.own.parent-point signer)
        ==
    ::
    :-  [ship %spon ~]~
    point(has.sponsor.net %|)
  ::
  ++  process-set-management-proxy
    |=  [=ship =point =address]
    ?>  ?|  =(owner.own.point signer)
            =(management-proxy.own.point signer)
        ==
    ::
    point(management-proxy.own address)
  ::
  ++  process-set-spawn-proxy
    |=  [=ship =point =address]
    ?>  ?|  =(owner.own.point signer)
            =(spawn-proxy.own.point signer)
        ==
    ::
    point(spawn-proxy.own address)
  ::
  ++  process-set-voting-proxy
    |=  [=ship =point =address]
    ?>  ?|  =(owner.own.point signer)
            =(voting-proxy.own.point signer)
        ==
    ::
    point(voting-proxy.own address)
  ::
  ++  process-set-transfer-proxy
    |=  [=ship =point =address]
    ?>  ?|  =(owner.own.point signer)
            =(transfer-proxy.own.point signer)
        ==
    ::
    point(transfer-proxy.own address)
  --
--
::
::  State transition function
::
::  TODO: wrap in mule to no-op instead of crash?  perhaps that's better
::  as part of the spec?  it's not a clear part of the nock spec, though
::
|=  [=verifier =state =input]
^-  [effects ^state]
?:  ?=(%log -.input)
  :: Received log from L1 transaction
  ::
  (receive-log state event-log.input)
::  Received L2 batch
::
(receive-batch verifier state batch.input)
