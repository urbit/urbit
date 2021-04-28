::  L1 contract changes:
::  - Enforce that once spawn proxy is set to deposit address, it can't
::    switched back
::  - Enforce that once spawn proxy is set to deposit address, you can't
::    spawn children
::  - Possibly the same for approveForAll
::  - Enforce that only ownership key can set spawn proxy to rollup.
::    maybe not though
::  - Disallow depositing galaxy to L2
::  - When depositing, clear proxies (maybe require reset)
::  - Maybe require that we're not depositing from a contract?
::
::  TODO: wraps and identities are verified at the beginning but
::  executed later.  that's no good, since the ownership could change
::  throughout the wrap or batch.
::
::  TODO: can an L1 star adopt an L2 planet?  It's not obvious how --
::  maybe they need to adopt as an L2 transaction?  That sounds right I
::  think.  Can an L2 star adopt an L1 planet?  I guess, but L1 wouldn't
::  know about it.  Should L1 check whether the escape target is on L2
::  for some reason?  IMO if either side is on L2, then both sides
::  should operate on L2
::
::  TODO: think about whether nonces are safe when associated with
::  ship/address
::
::  TODO: is it possible to spawn directly to the deposit address?  if
::  so, should we find its parent's owner to control it?
::
::  TODO: polymorphic addresses to save tx space?
::
::  TODO: could remove `ship` from most txs since it's in `from`
::
::  TODO: secp needs to not crash the process when you give it a bad
::  v/recid.  See #4797
::
::  TODO: probably should make opcode 0 a no-op and generally ensure
::  that 0 is not a valid tx, or that it's a no-op that we don't even
::  send to verify (not sure the verify code can handle a 0 tx)
::
::  TODO: add chainId, maybe just everything from
::  signTypedData_v4/EIP-712.  If we use that, we need to determine
::  whether EIP-712 is supported by the relevent wallets.  Looks like
::  ledger does, but maybe not Trezor?  Is there a way to hack around
::  it?
::
::  Okay, my understanding is we can use personal_sign for metamask,
::  trezor, and ledger, which means prepending each piece of signed data
::  with '\19Ethereum Signed Message:\0a' and then the length of the
::  signed data.  We should also include chain_id and maybe other stuff
::  from the domain separator in EIP-712.  But need to follow up on:
::  https://github.com/ethereum/go-ethereum/issues/14794
::
::  In any case, a signature version number sounds like a good idea
::
/+  std
=>  =>  std
::  Laconic bit
::
=|  lac=?
::  Constants
::
|%
::  Transfers on L1 to this address count as depositing to L2
::
::    0x1234567890123456789012345678901234567890
::
++  deposit-address  0x1234.5678.9012.3456.7890.1234.5678.9012.3456.7890
++  log-names
  |%
  ::  Generated with (keccak-256:keccak:crypto (as-octs:mimes:html name))
  ::
  ::  OwnerChanged(uint32,address)
  ++  owner-changed
    0x16d0.f539.d49c.6cad.822b.767a.9445.bfb1.
      cf7e.a6f2.a6c2.b120.a7ea.4cc7.660d.8fda
  ::
  ::  Activated(uint32)
  ++  activated
    0xe74c.0380.9d07.69e1.b1f7.06cc.8414.258c.
      d1f3.b6fe.020c.d15d.0165.c210.ba50.3a0f
  ::
  ::  Spawned(uint32,uint32)
  ++  spawned
    0xb2d3.a6e7.a339.f5c8.ff96.265e.2f03.a010.
      a854.1070.f374.4a24.7090.9644.1508.1546
  ::
  ::  OwnershipTransferred(address,address)
  ++  ownership-transferred
    0x8be0.079c.5316.5914.1344.cd1f.d0a4.f284.
      1949.7f97.22a3.daaf.e3b4.186f.6b64.57e0
  ::
  ::  EscapeRequested(uint32,uint32)
  ++  escape-requested
    0xb4d4.850b.8f21.8218.141c.5665.cba3.79e5.
      3e9b.b015.b51e.8d93.4be7.0210.aead.874a
  ::
  ::  EscapeCanceled(uint32,uint32)
  ++  escape-canceled
    0xd653.bb0e.0bb7.ce83.93e6.24d9.8fbf.17cd.
      a590.2c83.28ed.0cd0.9988.f368.90d9.932a
  ::
  ::  EscapeAccepted(uint32,uint32)
  ++  escape-accepted
    0x7e44.7c9b.1bda.4b17.4b07.96e1.00bf.7f34.
      ebf3.6dbb.7fe6.6549.0b1b.fce6.246a.9da5
  ::
  ::  LostSponsor(uint32,uint32)
  ++  lost-sponsor
    0xd770.4f9a.2519.3dbd.0b0c.b4a8.09fe.ffff.
      a7f1.9d1a.ae88.17a7.1346.c194.4482.10d5
  ::
  ::  ChangedKeys(uint32,bytes32,bytes32,uint32,uint32)
  ++  changed-keys
    0xaa10.e7a0.117d.4323.f1d9.9d63.0ec1.69be.
      bb3a.988e.8957.70e3.5198.7e01.ff54.23d5
  ::
  ::  BrokeContinuity(uint32,uint32)
  ++  broke-continuity
    0x2929.4799.f1c2.1a37.ef83.8e15.f79d.d91b.
      cee2.df99.d63c.d1c1.8ac9.68b1.2951.4e6e
  ::
  ::  ChangedSpawnProxy(uint32,address)
  ++  changed-spawn-proxy
    0x9027.36af.7b3c.efe1.0d9e.840a.ed0d.687e.
      35c8.4095.122b.2505.1a20.ead8.866f.006d
  ::
  ::  ChangedTransferProxy(uint32,address)
  ++  changed-transfer-proxy
    0xcfe3.69b7.197e.7f0c.f067.93ae.2472.a9b1.
      3583.fecb.ed2f.78df.a14d.1f10.796b.847c
  ::
  ::  ChangedManagementProxy(uint32,address)
  ++  changed-management-proxy
    0xab9c.9327.cffd.2acc.168f.afed.be06.139f.
      5f55.cb84.c761.df05.e051.1c25.1e2e.e9bf
  ::
  ::  ChangedVotingProxy(uint32,address)
  ++  changed-voting-proxy
    0xcbd6.269e.c714.57f2.c7b1.a227.74f2.46f6.
      c5a2.eae3.795e.d730.0db5.1768.0c61.c805
  ::
  ::  ChangedDns(string,string,string)
  ++  changed-dns
    0xfafd.04ad.e1da.ae2e.1fdb.0fc1.cc6a.899f.
      d424.063e.d5c9.2120.e67e.0730.53b9.4898
  ::
  ::  ApprovalForAll(address,address,bool)
  ++  approval-for-all
    0x1730.7eab.39ab.6107.e889.9845.ad3d.59bd.
      9653.f200.f220.9204.89ca.2b59.3769.6c31
  --
--  =>
::  Types
|%
::  ethereum address, 20 bytes.
::
+$  nonce     @ud
+$  address   @ux
+$  dominion  ?(%l1 %l2 %spawn)
++  point
  $:  ::  domain
      ::
      =dominion
    ::
      ::  ownership
      ::
      $=  own
      $:  owner=[=address =nonce]
          spawn-proxy=[=address =nonce]
          management-proxy=[=address =nonce]
          voting-proxy=[=address =nonce]
          transfer-proxy=[=address =nonce]
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
++  diff
  $%  [%nonce =ship =proxy =nonce]
      [%operator owner=address operator=address approved=?]
      [%dns domains=(list @t)]
      $:  %point  =ship
          $%  [%rift =rift]
              [%keys =life crypto-suite=@ud =pass]
              [%sponsor sponsor=(unit @p)]
              [%escape to=(unit @p)]
              [%owner =address]
              [%spawn-proxy =address]
              [%management-proxy =address]
              [%voting-proxy =address]
              [%transfer-proxy =address]
              [%dominion =dominion]
  ==  ==  ==
::
+$  state
  $:  =points
      =operators
      dns=(list @t)
  ==
+$  points     (map ship point)
+$  operators  (jug address address)
+$  effects    (list diff)
+$  proxy      ?(%own %spawn %manage %vote %transfer)
+$  roll       (list raw-tx)
+$  raw-tx     [sig=@ raw=octs =tx]
+$  tx         [from=[=ship =proxy] skim-tx]
+$  skim-tx
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
      [%set-transfer-proxy =ship =address]
  ==
::
+$  event-log
  $:  address=@ux
      data=@ux
      topics=(lest @ux)
  ==
+$  input
  $%  [%bat batch=@]
      [%log =event-log]
  ==
::  ECDSA verifier
::
+$  verifier  $-([dat=octs v=@ r=@ s=@] (unit address))
--  =>
::
|%
++  debug
  |*  [meg=@t *]
  ?:  lac
    +<+
  ~>  %slog.[0 meg]
  +<+
::
++  parse-roll
  |=  batch=@
  =|  =roll
  |-  ^+  roll
  ?~  batch
    (flop roll)
  =/  parse-result  (parse-raw-tx batch)
  ::  Parsing failed, abort batch
  ::
  ?~  parse-result
    (debug %parse-failed ~)
  =^  =raw-tx  batch  u.parse-result
  $(roll [raw-tx roll])
::
::  TODO: change batch to be a cursor to avoid allocating atoms
::
++  parse-raw-tx
  |=  batch=@
  ^-  (unit [raw-tx rest=@])
  =/  batch  [len=0 rest=batch]
  |^
  =^  sig  batch  (take 3 65)
  =.  len.batch  0
  =/  orig-batch  rest.batch
  =/  res=(unit [=tx batch=_batch])  parse-tx
  ?~  res
    ~
  :-  ~  :_  rest.batch.u.res
  =/  len-bytes
    ?>  =(0 (mod len.batch.u.res 8))
    (div len.batch.u.res 8)
  [sig [len-bytes (end [0 len.batch.u.res] orig-batch)] tx.u.res]
  ::
  ++  parse-tx
    ^-  (unit [tx _batch])
    =^  from-proxy=@      batch  (take 0 3)
    ?:  (gth from-proxy 4)  (debug %bad-proxy ~)
    =/  =proxy
      ?+  from-proxy  !!  :: checked above that lte 4
        %0  %own
        %1  %spawn
        %2  %manage
        %3  %vote
        %4  %transfer
      ==
    =^  pad               batch  (take 0 5)
    =^  from-ship=ship    batch  (take 3 4)
    =-  ?~  res
          ~
        `[[[from-ship proxy] skim-tx.u.res] batch.u.res]
    ^-  res=(unit [=skim-tx =_batch])
    =^  op   batch  (take 0 7)
    ?+    op  ~>(%slog.[0 %strange-opcode] ~)
        %0
      =^  reset=@         batch  (take 0)
      =^  =ship           batch  (take 3 4)
      =^  =address        batch  (take 3 20)
      `[[%transfer-point ship address =(0 reset)] batch]
    ::
        %1   =^(res batch take-ship-address `[[%spawn res] batch])
        %2
      =^  breach=@        batch  (take 0)
      =^  =ship           batch  (take 3 4)
      =^  encrypt=@       batch  (take 3 32)
      =^  auth=@          batch  (take 3 32)
      =^  crypto-suite=@  batch  (take 3 4)
      `[[%configure-keys ship encrypt auth crypto-suite =(0 breach)] batch]
    ::
        %3   =^(res batch take-escape `[[%escape res] batch])
        %4   =^(res batch take-escape `[[%cancel-escape res] batch])
        %5   =^(res batch take-escape `[[%adopt res] batch])
        %6   =^(res batch take-escape `[[%reject res] batch])
        %7   =^(res batch take-escape `[[%detach res] batch])
        %8
      =^(res batch take-ship-address `[[%set-management-proxy res] batch])
    ::
        %9   =^(res batch take-ship-address `[[%set-spawn-proxy res] batch])
        %10  =^(res batch take-ship-address `[[%set-transfer-proxy res] batch])
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
  --
::
++  verify-sig-and-nonce
  |=  [=verifier =state =raw-tx]
  ^-  ?
  |^
  =/  point  (get-point state ship.from.tx.raw-tx)
  ?>  ?=(^ point)  ::  we never parse more than four bytes
  =/  need=[=address =nonce]
    ?-  proxy.from.tx.raw-tx
      %own       owner.own.u.point
      %spawn     spawn-proxy.own.u.point
      %manage    management-proxy.own.u.point
      %vote      voting-proxy.own.u.point
      %transfer  transfer-proxy.own.u.point
    ==
  ::
  =/  prepared-data=octs
    :-  (add 4 p.raw.raw-tx)
    (can 3 4^nonce.need raw.raw-tx ~)
  =/  signed-data=octs
    =/  len  (ud-to-len p.prepared-data)
    :-  :(add 26 (met 3 len) p.prepared-data)
    %:  can  3
      26^'\19Ethereum Signed Message:\0a'
      (met 3 len)^len
      prepared-data
      ~
    ==
  =/  dress  (verify-sig sig.raw-tx signed-data)
  ?~  dress
    |
  =(address.need u.dress)
  ::  Verify signature and produce signer address
  ::
  ++  verify-sig
    |=  [sig=@ txdata=octs]
    ^-  (unit address)
    |^
    ::  Reversed of the usual r-s-v order because Ethereum integers are
    ::  big-endian
    ::
    =^  v  sig  (take 3)
    =^  s  sig  (take 3 32)
    =^  r  sig  (take 3 32)
    ::  In Ethereum, v is generally 27 + recid, and verifier expects a
    ::  recid.  Old versions of geth used 0 + recid, so most software
    ::  now supports either format.  See:
    ::
    ::  https://github.com/ethereum/go-ethereum/issues/2053
    ::
    =?  v  (gte v 27)  (sub v 27)
    (verifier txdata v r s)
    ::
    ++  take
      |=  =bite
      [(end bite sig) (rsh bite sig)]
    --
  ::  ASCII-encode length
  ::
  ++  ud-to-len
    |=  n=@ud
    ^-  @t
    ?~  n
      *@t
    (cat 3 $(n (div n 10)) (add '0' (mod n 10)))
  --
::
++  ship-rank
  |=  =ship
  ^-  ?(%0 %1 %2 %3 %4)
  ?:  (lth ship 0x100)                    %0
  ?:  (lth ship 0x1.0000)                 %1
  ?:  (lth ship 0x1.0000.0000)            %2
  ?:  (lth ship 0x1.0000.0000.0000.0000)  %3
  %4
::
++  sein                                          ::  autoboss
  |=  who=ship
  ^-  ship
  =/  mir  (ship-rank who)
  ?-  mir
    %0  who
    %1  (end 3 who)
    %2  (end 4 who)
    %3  (end 5 who)
    %4  (end 4 who)
  ==
::
::  TODO: encode sut
::
++  pass-from-eth
  |=  [enc=octs aut=octs sut=@ud]
  ^-  pass
  %^  cat  3  'b'
  ?.  &(=(1 sut) =(p.enc 32) =(p.aut 32))
    (cat 8 0 0)  ::  TODO: fix
  (cat 8 q.aut q.enc)
::  Produces null only if ship is not a galaxy, star, or planet
::
++  get-point
  |=  [=state =ship]
  ^-  (unit point)
  =/  existing  (~(get by points.state) ship)
  ?^  existing
    `u.existing
  =|  =point
  =.  who.sponsor.net.point  (sein ship)
  ?+    (ship-rank ship)  ~>(%slog.[0 %strange-point] ~)
      %0  `point(dominion %l1)
      ?(%1 %2)
    =/  existing-parent  $(ship (sein ship))
    ?~  existing-parent  ~
    :-  ~
    %=    point
        dominion
      ?-  dominion.u.existing-parent
        %l1     %l1
        %l2     %l2
        %spawn  %l2
      ==
    ==
  ==
--  =>
|%
:: Receive log from L1 transaction
::
++  receive-log
  |=  [=state log=event-log]
  ^-  [effects ^state]
  =*  log-name  i.topics.log
  ?:  =(log-name activated:log-names)              `state
  ?:  =(log-name spawned:log-names)                `state
  ?:  =(log-name ownership-transferred:log-names)  `state
  ?:  =(log-name changed-dns:log-names)
    ?>  ?=(~ t.topics.log)
    =/  words  (rip 8 data.log)
    ?>  ?=([c=@ @ b=@ @ a=@ @ @ @ @ ~] words)  ::  TODO: not always true
    =*  one  &5.words
    =*  two  &3.words
    =*  tri  &1.words
    =/  domains  (turn ~[one two tri] |=(a=@ (swp 3 a)))
    :-  [%dns domains]~
    state(dns domains)
  ::
  ?:  =(log-name approval-for-all:log-names)
    ?>  ?=([@ @ ~] t.topics.log)
    =*  owner     i.t.topics.log
    =*  operator  i.t.t.topics.log
    =/  approved  !=(0 data.log)
    :-  [%operator owner operator approved]~
    =-  state(operators -)
    ?:  approved
      (~(put ju operators.state) owner operator)
    (~(del ju operators.state) owner operator)
  ::
  ::  The rest of the logs modify a particular ship, specified in the
  ::  second topic.  We fetch it, and insert the modification back into
  ::  our state.
  ::
  ?>  ?=([@ *] t.topics.log)
  =*  ship=@  i.t.topics.log
  =/  the-point  (get-point state ship)
  ?>  ?=(^ the-point)
  =*  point  u.the-point
  =-  [effects state(points (~(put by points.state) ship new-point))]
  ^-  [=effects new-point=^point]
  ::
  ?:  =(log-name changed-spawn-proxy:log-names)
    ?>  ?=(%l1 -.point)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    ::  Depositing to L2 is represented by a spawn proxy change on L1,
    ::  but it doesn't change the actual spawn proxy.
    ::
    ?:  =(deposit-address to)
      :-  [%point ship %dominion %spawn]~
      point(dominion %spawn)
    :-  [%point ship %spawn-proxy to]~
    point(address.spawn-proxy.own to)
  ::
  ::  The rest can be done by any ship on L1, even if their spawn proxy
  ::  is set to L2
  ::
  ?<  ?=(%l2 -.point)
  ::
  ?:  =(log-name broke-continuity:log-names)
    ?>  ?=(~ t.t.topics.log)
    =*  rift=@  data.log
    :-  [%point ship %rift rift]~
    point(rift.net rift)
  ::
  ?:  =(log-name changed-keys:log-names)
    ?>  ?=(~ t.t.topics.log)
    =/  encryption=@      (cut 8 [3 1] data.log)
    =/  authentication=@  (cut 8 [2 1] data.log)
    ::  TODO: store in state, or add to pass
    ::
    =/  crypto-suite=@    (cut 8 [1 1] data.log)
    =/  life=@            (cut 8 [0 1] data.log)
    =/  =pass  (pass-from-eth 32^encryption 32^authentication crypto-suite)
    :-  [%point ship %keys life crypto-suite pass]~
    point(life.net life, pass.net pass)
  ::
  ?:  =(log-name escape-accepted:log-names)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent=@  i.t.t.topics.log
    :-  [%point ship %sponsor `parent]~
    point(escape.net ~, sponsor.net [%& parent])
  ::
  ?:  =(log-name lost-sponsor:log-names)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent  i.t.t.topics.log
    :-  [%point ship %sponsor ~]~
    point(has.sponsor.net %|)
  ::
  ?:  =(log-name escape-requested:log-names)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent=@  i.t.t.topics.log
    :-  [%point ship %escape `parent]~
    point(escape.net `parent)
  ::
  ?:  =(log-name escape-canceled:log-names)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  parent  i.t.t.topics.log
    :-  [%point ship %escape ~]~
    point(escape.net ~)
  ::
  ?:  =(log-name owner-changed:log-names)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    ::  Depositing to L2 is represented by an ownership change on L1,
    ::  but it doesn't change who actually owns the ship.
    ::
    ?:  =(deposit-address to)
      :-  [%point ship %dominion %l2]~
      point(dominion %l2)
    :-  [%point ship %owner to]~
    point(address.owner.own to)
  ::
  ?:  =(log-name changed-transfer-proxy:log-names)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    :-  [%point ship %transfer-proxy to]~
    point(address.transfer-proxy.own to)
  ::
  ?:  =(log-name changed-management-proxy:log-names)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    :-  [%point ship %management-proxy to]~
    point(address.management-proxy.own to)
  ::
  ?:  =(log-name changed-voting-proxy:log-names)
    ?>  ?=([@ ~] t.t.topics.log)
    =*  to  i.t.t.topics.log
    :-  [%point ship %voting-proxy to]~
    point(address.voting-proxy.own to)
  ::
  ~>  %slog.[0 %unknown-log]
  `point
::
::  Receive batch of L2 transactions
::
++  receive-batch
  |=  [=verifier =state batch=@]
  =/  =roll  (parse-roll batch)
  |-  ^-  [effects ^state]
  ?~  roll
    [~ state]
  ::  Verify signature, else skip tx
  ::
  ?.  (verify-sig-and-nonce verifier state i.roll)
    %+  debug  %l2-sig-failed
    $(roll t.roll)
  ::  Increment nonce, even if it later fails
  ::
  =^  effects-1  points.state  (increment-nonce state from.tx.i.roll)
  ::  Process tx
  ::
  =^  effects-2  state
    =/  tx-result=(unit [effects ^state])  (receive-tx state tx.i.roll)
    ?~  tx-result
      (debug %l2-tx-failed `state)
    u.tx-result
  =^  effects-3  state  $(roll t.roll)
  [:(welp effects-1 effects-2 effects-3) state]
::
++  increment-nonce
  |=  [=state =ship =proxy]
  =/  point  (get-point state ship)
  ?>  ?=(^ point)  ::  we only parsed 4 bytes
  =*  own  own.u.point
  =^  nonce  u.point
    ?-    proxy
        %own
      :-  nonce.owner.own
      u.point(nonce.owner.own +(nonce.owner.own))
    ::
        %spawn
      :-  nonce.spawn-proxy.own
      u.point(nonce.spawn-proxy.own +(nonce.spawn-proxy.own))
    ::
        %manage
      :-  nonce.management-proxy.own
      u.point(nonce.management-proxy.own +(nonce.management-proxy.own))
    ::
        %vote
      :-  nonce.voting-proxy.own
      u.point(nonce.voting-proxy.own +(nonce.voting-proxy.own))
    ::
        %transfer
      :-  nonce.transfer-proxy.own
      u.point(nonce.transfer-proxy.own +(nonce.transfer-proxy.own))
    ==
  ::
  :-  [%nonce ship proxy nonce]~
  (~(put by points.state) ship u.point)
::
::  Receive an individual L2 transaction
::
++  receive-tx
  |=  [=state =tx]
  |^
  ^-  (unit [effects ^state])
  ?-  +<.tx
    %spawn                  (process-spawn +>.tx)
    %transfer-point         (w-point process-transfer-point +>.tx)
    %configure-keys         (w-point process-configure-keys +>.tx)
    %escape                 (w-point process-escape +>.tx)
    %cancel-escape          (w-point process-cancel-escape +>.tx)
    %adopt                  (w-point process-adopt +>.tx)
    %reject                 (w-point process-reject +>.tx)
    %detach                 (w-point process-detach +>.tx)
    %set-management-proxy   (w-point process-set-management-proxy +>.tx)
    %set-spawn-proxy        (w-point process-set-spawn-proxy +>.tx)
    %set-transfer-proxy     (w-point process-set-transfer-proxy +>.tx)
  ==
  ::
  ++  w-point
    |*  [fun=$-([ship point *] (unit [effects point])) =ship rest=*]
    ^-  (unit [effects ^state])
    =/  point  (get-point state ship)
    ?~  point  (debug %strange-ship ~)
    ?.  ?=(%l2 -.u.point)  (debug %ship-not-on-l2 ~)
    =/  res=(unit [=effects new-point=^point])  (fun ship u.point rest)
    ?~  res
      ~
    `[effects.u.res state(points (~(put by points.state) ship new-point.u.res))]
  ::
  ++  process-transfer-point
    |=  [=ship =point to=address reset=?]
    ::  Assert from owner or transfer prxoy
    ::
    ?.  ?&  =(ship ship.from.tx)
            |(=(%own proxy.from.tx) =(%transfer proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::  Execute transfer
    ::
    =/  effects-1
      ~[[%point ship %owner to] [%point ship %transfer-proxy *address]]
    =:  address.owner.own.point           to
        address.transfer-proxy.own.point  *address
      ==
    ::  Execute reset if requested
    ::
    ?.  reset
      `[effects-1 point]
    ::
    =^  effects-2  net.point
      ?:  =(0 life.net.point)
        `net.point
      :-  :~  [%point ship %rift +(rift.net.point)]
              [%point ship %keys +(life.net.point) 0 0]  ::  TODO: 0?
          ==
      [+(life) 0 +(rift) sponsor escape]:net.point
    =/  effects-3
      :~  [%point ship %spawn-proxy *address]
          [%point ship %management-proxy *address]
          [%point ship %voting-proxy *address]
          [%point ship %transfer-proxy *address]
      ==
    =:  address.spawn-proxy.own.point       *address
        address.management-proxy.own.point  *address
        address.voting-proxy.own.point      *address
        address.transfer-proxy.own.point    *address
      ==
    `[:(welp effects-1 effects-2 effects-3) point]
  ::
  ++  process-spawn
    |=  [=ship to=address]
    ^-  (unit [effects ^state])
    =/  parent=^ship  (sein ship)
    ::  Assert parent is on L2
    ::
    =/  parent-point  (get-point state parent)
    ?~  parent-point  ~
    ?.  ?=(?(%l2 %spawn) -.u.parent-point)  ~
    ::  Assert from owner or spawn proxy
    ::
    ?.  ?&  =(parent ship.from.tx)
            |(=(%own proxy.from.tx) =(%spawn proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::  Assert child not already spawned
    ::
    ::  TODO: verify this means the ship exists on neither L1 nor L2
    ::
    ?:  (~(has by points.state) ship)  (debug %spawn-exists ~)
    ::  Assert one-level-down
    ::
    ?.  =(+((ship-rank parent)) (ship-rank ship))  (debug %bad-rank ~)
    ::  TODO check spawnlimit
    ::
    =/  [=effects new-point=point]
      ::  If spawning to self, just do it
      ::
      ?:  ?|  ?&  =(%own proxy.from.tx)
                  =(to address.owner.own.u.parent-point)
              ==
              ?&  =(%spawn proxy.from.tx)
                  =(to address.spawn-proxy.own.u.parent-point)
              ==
          ==
        :-  ~[[%point ship %dominion %l2] [%point ship %owner to]]
        %*  .  *point
          dominion           %l2
          address.owner.own  to
        ==
      ::  Else spawn to parent and set transfer proxy
      ::
      :-  :~  [%point ship %dominion %l2]
              [%point ship %owner address.owner.own.u.parent-point]
              [%point ship %transfer-proxy to]
          ==
      %*  .  *point
        dominion                    %l2
        address.owner.own           address.owner.own.u.parent-point
        address.transfer-proxy.own  to
      ==
    `[effects state(points (~(put by points.state) ship new-point))]
  ::
  ++  process-configure-keys
    |=  [=ship =point encrypt=@ auth=@ crypto-suite=@ breach=?]
    ::
    ?.  ?&  =(ship ship.from.tx)
            |(=(%own proxy.from.tx) =(%manage proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::
    =^  rift-effects  rift.net.point
      ?.  breach
        `rift.net.point
      [[%point ship %rift +(rift.net.point)]~ +(rift.net.point)]
    ::
    =/  =pass  (pass-from-eth 32^encrypt 32^auth crypto-suite)
    =?  net.point  !=(pass.net.point pass)  ::  TODO: check crypto-suite
      net.point(life +(life.net.point), pass pass)
    =/  keys-effects
      ?:  =(pass.net.point pass)  ::  TODO: check will always be true
        ~
      [%point ship %keys life.net.point crypto-suite pass]~
    ::
    `[(welp rift-effects keys-effects) point]
  ::
  ++  process-escape
    |=  [=ship =point parent=ship]
    ?.  ?&  =(ship ship.from.tx)
            |(=(%own proxy.from.tx) =(%manage proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::  TODO: don't allow "peer escape"?
    ::
    ?.  =(+((ship-rank parent)) (ship-rank ship))  (debug %bad-rank ~)
    ::
    :+  ~  [%point ship %escape `parent]~
    point(escape.net `parent)  ::  TODO: omitting a lot of source material?
  ::
  ++  process-cancel-escape
    |=  [=ship =point parent=ship]
    ?.  ?&  =(ship ship.from.tx)
            |(=(%own proxy.from.tx) =(%manage proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::
    :+  ~  [%point ship %escape ~]~
    point(escape.net ~)
  ::
  ++  process-adopt
    |=  [=ship =point parent=ship]
    :: TODO: assert child/parent on L2?
    ::
    ?.  ?&  =(parent ship.from.tx)
            |(=(%own proxy.from.tx) =(%manage proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::
    ?.  =(escape.net.point `parent)  (debug %no-adopt ~)
    :+  ~  [%point ship %sponsor `parent]~
    point(escape.net ~, sponsor.net [%& parent])
  ::
  ++  process-reject
    |=  [=ship =point parent=ship]
    ?.  ?&  =(parent ship.from.tx)
            |(=(%own proxy.from.tx) =(%manage proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::
    ?.  =(escape.net.point `parent)  (debug %no-reject ~)
    :+  ~  [%point ship %escape ~]~
    point(escape.net ~)
  ::
  ++  process-detach
    |=  [=ship =point parent=ship]
    ?.  ?&  =(parent ship.from.tx)
            |(=(%own proxy.from.tx) =(%manage proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::
    ?.  =(who.sponsor.net.point parent)  (debug %no-detach ~)
    :+  ~  [%point ship %sponsor ~]~
    point(has.sponsor.net %|)
  ::
  ++  process-set-management-proxy
    |=  [=ship =point =address]
    ?.  ?&  =(ship ship.from.tx)
            |(=(%own proxy.from.tx) =(%manage proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::
    :+  ~  [%point ship %management-proxy address]~
    point(address.management-proxy.own address)
  ::
  ++  process-set-spawn-proxy
    |=  [=ship =point =address]
    ?.  ?&  =(ship ship.from.tx)
            |(=(%own proxy.from.tx) =(%spawn proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::
    :+  ~  [%point ship %spawn-proxy address]~
    point(address.spawn-proxy.own address)
  ::
  ++  process-set-transfer-proxy
    |=  [=ship =point =address]
    ?.  ?&  =(ship ship.from.tx)
            |(=(%own proxy.from.tx) =(%transfer proxy.from.tx))
        ==
      (debug %bad-permission ~)
    ::
    :+  ~  [%point ship %transfer-proxy address]~
    point(address.transfer-proxy.own address)
  --
--
::
::  State transition function
::
|=  [=verifier =state =input]
^-  [effects ^state]
?:  ?=(%log -.input)
  :: Received log from L1 transaction
  ::
  (receive-log state event-log.input)
::  Received L2 batch
::
%+  debug  %batch
(receive-batch verifier state batch.input)
