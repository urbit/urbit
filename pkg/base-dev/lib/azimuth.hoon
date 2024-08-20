::  azimuth: constants and utilities
::
/+  ethereum
::
=>  =>  [azimuth-types ethereum-types .]
    |%
    +$  complete-ship
      $:  state=point
          history=(list diff-point)  ::TODO  maybe block/event nr?  ::  newest first
          keys=(map life pass)
      ==
    ::
    ++  fleet  (map @p complete-ship)
    ::
    ++  eth-type
      |%
      ++  point
        :~  [%bytes-n 32]   ::  encryptionKey
            [%bytes-n 32]   ::  authenticationKey
            %bool           ::  hasSponsor
            %bool           ::  active
            %bool           ::  escapeRequested
            %uint           ::  sponsor
            %uint           ::  escapeRequestedTo
            %uint           ::  cryptoSuiteVersion
            %uint           ::  keyRevisionNumber
            %uint           ::  continuityNumber
        ==
      ++  deed
        :~  %address        ::  owner
            %address        ::  managementProxy
            %address        ::  spawnProxy
            %address        ::  votingProxy
            %address        ::  transferProxy
        ==
      --
    ::
    ++  eth-noun
      |%
      +$  point
        $:  encryption-key=octs
            authentication-key=octs
            has-sponsor=?
            active=?
            escape-requested=?
            sponsor=@ud
            escape-to=@ud
            crypto-suite=@ud
            key-revision=@ud
            continuity-number=@ud
        ==
      +$  deed
        $:  owner=address
            management-proxy=address
            spawn-proxy=address
            voting-proxy=address
            transfer-proxy=address
        ==
      --
    ::
    ++  function
      |%
      ++  azimuth
        $%  [%points who=@p]
            [%rights who=@p]
            [%get-spawned who=@p]
            [%dns-domains ind=@ud]
        ==
      --
    ::
    ::  #  diffs
    ::
    ++  update
      $%  [%full ships=(map ship point) dns=dnses heard=events]
          [%difs dis=(list (pair event-id diff-azimuth))]
      ==
    ::
    ::  #  constants
    ::
    ::  contract addresses
    ++  contracts  mainnet-contracts
    ++  mainnet-contracts
      |%
      ::  azimuth: data contract
      ::
      ++  azimuth
        0x223c.067f.8cf2.8ae1.73ee.5caf.ea60.ca44.c335.fecb
      ::
      ++  ecliptic
        0x33ee.cbf9.0847.8c10.6146.26a9.d304.bfe1.8b78.dd73
      ::
      ++  linear-star-release
        0x86cd.9cd0.992f.0423.1751.e376.1de4.5cec.ea5d.1801
      ::
      ++  conditional-star-release
        0x8c24.1098.c3d3.498f.e126.1421.633f.d579.86d7.4aea
      ::
      ++  delegated-sending
        0xf790.8ab1.f1e3.52f8.3c5e.bc75.051c.0565.aeae.a5fb
      ::
      ++  naive
        0xeb70.029c.fb3c.53c7.78ea.f68c.d28d.e725.390a.1fe9
      ::
      ::  launch: block number of azimuth deploy
      ::
      ++  launch  6.784.800
      ::
      ::  public: block number of azimuth becoming independent
      ::
      ++  public  7.033.765
      ::
      ++  chain-id  1
      --
    ::
    ::  Testnet contract addresses
    ::
    ++  goerli-contracts
      |%
      ++  azimuth
        0xbb61.fa68.3e4b.9104.18e2.7b00.a143.8a93.6234.df52
      ::
      ++  ecliptic
        0xe129.0a32.9014.5e63.e6a8.ec1e.f661.6906.856d.0c8f
      ::
      ++  linear-star-release
        0x0
      ::
      ++  conditional-star-release
        0x0
      ::
      ++  delegated-sending
        0x0
      ::
      ++  naive
        0x56e3.7137.cdaf.c026.a732.e8e8.40cd.621e.d50b.d210
      ::
      ++  launch  7.834.742
      ++  public  7.834.800
      ++  chain-id  5
      --
    ::
    ::  Local contract addresses
    ::
    ::  These addresses are only reproducible if you use the deploy
    ::  script in bridge
    ::
    ++  local-contracts
      |%
      ++  ecliptic
        0x56db.68f2.9203.ff44.a803.faa2.404a.44ec.bb7a.7480
      ++  azimuth
        0x863d.9c2e.5c4c.1335.96cf.ac29.d552.55f0.d0f8.6381
      ++  delegated-sending
        0xb71c.0b6c.ee1b.cae5.6dfe.95cd.9d3e.41dd.d7ea.fc43
      ++  linear-star-release
        0x3c3.dc12.be65.8158.d1d7.f9e6.6e08.ec40.99c5.68e4
      ++  conditional-star-release
        0x35eb.3b10.2d9c.1b69.ac14.69c1.b1fe.1799.850c.d3eb
      ++  naive
        0x6bb8.8a9b.bd82.be7a.997f.eb01.929c.6ec7.8988.fe12
      ++  launch  0
      ++  public  0
      ++  chain-id  1.337
      --
    ::
      ::  ++  azimuth  0x863d.9c2e.5c4c.1335.96cf.ac29.d552.55f0.d0f8.6381  ::  local bridge
    ::  hashes of ship event signatures
    ++  azimuth-events
      |%
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
      --
    --
::
::  logic
::
|%
++  pass-from-eth
  |=  [enc=octs aut=octs sut=@ud]
  ^-  pass
  %^  cat  3  'b'
  ?.  &(=(1 sut) =(p.enc 32) =(p.aut 32))
    (cat 8 0 0)
  (cat 8 q.aut q.enc)
::
++  point-from-eth
  |=  [who=@p point:eth-noun deed:eth-noun]
  ^-  point
  ::
  ::  ownership
  ::
  :+  :*  owner
          management-proxy
          voting-proxy
          transfer-proxy
      ==
    ::
    ::  network state
    ::
    ?.  active  ~
    :-  ~
    :*  key-revision
      ::
        (pass-from-eth encryption-key authentication-key crypto-suite)
      ::
        continuity-number
      ::
        [has-sponsor `@p`sponsor]
      ::
        ?.  escape-requested  ~
        ``@p`escape-to
    ==
  ::
  ::  spawn state
  ::
  ?.  ?=(?(%czar %king) (clan:title who))  ~
  :-  ~
  :*  spawn-proxy
      ~  ::TODO  call getSpawned to fill this
  ==
::
++  event-log-to-point-diff
  =,  azimuth-events
  =,  abi:ethereum
  |=  log=event-log:rpc:ethereum
  ^-  (unit (pair ship diff-point))
  ~?  ?=(~ mined.log)  %processing-unmined-event
  ::
  ?:  =(i.topics.log owner-changed)
    =/  [who=@ wer=address]
        (decode-topics t.topics.log ~[%uint %address])
    `[who %owner wer]
  ::
  ?:  =(i.topics.log activated)
    =/  who=@
      (decode-topics t.topics.log ~[%uint])
    `[who %activated who]
  ::
  ?:  =(i.topics.log spawned)
    =/  [pre=@ who=@]
        (decode-topics t.topics.log ~[%uint %uint])
    `[pre %spawned who]
  ::
  ?:  =(i.topics.log escape-requested)
    =/  [who=@ wer=@]
        (decode-topics t.topics.log ~[%uint %uint])
    `[who %escape `wer]
  ::
  ?:  =(i.topics.log escape-canceled)
    =/  who=@  (decode-topics t.topics.log ~[%uint])
    `[who %escape ~]
  ::
  ?:  =(i.topics.log escape-accepted)
    =/  [who=@ wer=@]
        (decode-topics t.topics.log ~[%uint %uint])
    `[who %sponsor & wer]
  ::
  ?:  =(i.topics.log lost-sponsor)
    =/  [who=@ pos=@]
        (decode-topics t.topics.log ~[%uint %uint])
    `[who %sponsor | pos]
  ::
  ?:  =(i.topics.log changed-keys)
    =/  who=@  (decode-topics t.topics.log ~[%uint])
    =/  [enc=octs aut=octs sut=@ud rev=@ud]
        %+  decode-results  data.log
        ~[[%bytes-n 32] [%bytes-n 32] %uint %uint]
    `[who %keys rev (pass-from-eth enc aut sut)]
  ::
  ?:  =(i.topics.log broke-continuity)
    =/  who=@  (decode-topics t.topics.log ~[%uint])
    =/  num=@  (decode-results data.log ~[%uint])
    `[who %continuity num]
  ::
  ?:  =(i.topics.log changed-management-proxy)
    =/  [who=@ sox=address]
        (decode-topics t.topics.log ~[%uint %address])
    `[who %management-proxy sox]
  ::
  ?:  =(i.topics.log changed-voting-proxy)
    =/  [who=@ tox=address]
        (decode-topics t.topics.log ~[%uint %address])
    `[who %voting-proxy tox]
  ::
  ?:  =(i.topics.log changed-spawn-proxy)
    =/  [who=@ sox=address]
        (decode-topics t.topics.log ~[%uint %address])
    `[who %spawn-proxy sox]
  ::
  ?:  =(i.topics.log changed-transfer-proxy)
    =/  [who=@ tox=address]
        (decode-topics t.topics.log ~[%uint %address])
    `[who %transfer-proxy tox]
  ::
  ::  warn about unimplemented events, but ignore
  ::  the ones we know are harmless.
  ~?  ?!  .=  i.topics.log
          ::  OwnershipTransferred(address,address)
          0x8be0.079c.5316.5914.1344.cd1f.d0a4.f284.
            1949.7f97.22a3.daaf.e3b4.186f.6b64.57e0
    [%unimplemented-event i.topics.log]
  ~
::
++  apply-point-diff
  |=  [pot=point dif=diff-point]
  ^-  point
  ?-  -.dif
    %full             new.dif
  ::
      %activated
    %_  pot
      net  `[0 0 0 &^(^sein:title who.dif) ~]
      kid  ?.  ?=(?(%czar %king) (clan:title who.dif))  ~
            `[0x0 ~]
    ==
  ::
  ::  ownership
  ::
    %owner           pot(owner.own new.dif)
    %transfer-proxy  pot(transfer-proxy.own new.dif)
    %management-proxy  pot(management-proxy.own new.dif)
    %voting-proxy      pot(voting-proxy.own new.dif)
  ::
  ::  networking
  ::
      ?(%keys %continuity %sponsor %escape)
    ?>  ?=(^ net.pot)
    ?-  -.dif
        %keys
      pot(life.u.net life.dif, pass.u.net pass.dif)
    ::
        %sponsor
      %=  pot
        sponsor.u.net  new.dif
        escape.u.net   ?:(has.new.dif ~ escape.u.net.pot)
      ==
    ::
      %continuity  pot(continuity-number.u.net new.dif)
      %escape      pot(escape.u.net new.dif)
    ==
  ::
  ::  spawning
  ::
      ?(%spawned %spawn-proxy)
    ?>  ?=(^ kid.pot)
    ?-  -.dif
        %spawned
      =-  pot(spawned.u.kid -)
      (~(put in spawned.u.kid.pot) who.dif)
    ::
      %spawn-proxy  pot(spawn-proxy.u.kid new.dif)
    ==
  ==
::
++  parse-id
  |=  id=@t
  ^-  azimuth:function
  |^
    ~|  id
    %+  rash  id
    ;~  pose
      (function %points 'points' shipname)
      (function %get-spawned 'getSpawned' shipname)
      (function %dns-domains 'dnsDomains' dem:ag)
    ==
  ::
  ++  function
    |*  [tag=@tas fun=@t rul=rule]
    ;~(plug (cold tag (jest fun)) (ifix [pal par] rul))
  ::
  ++  shipname
    ;~(pfix sig fed:ag)
  --
::
++  function-to-call
  |%
  ++  azimuth
    |=  cal=azimuth:function
    ^-  [id=@t dat=call-data:rpc:ethereum]
    ?-  -.cal
        %points
      :-  (crip "points({(scow %p who.cal)})")
      ['points(uint32)' ~[uint+`@`who.cal]]
    ::
        %rights
      :-  (crip "rights({(scow %p who.cal)})")
      ['rights(uint32)' ~[uint+`@`who.cal]]
    ::
        %get-spawned
      :-  (crip "getSpawned({(scow %p who.cal)})")
      ['getSpawned(uint32)' ~[uint+`@`who.cal]]
    ::
        %dns-domains
      :-  (crip "dnsDomains({(scow %ud ind.cal)})")
      ['dnsDomains(uint256)' ~[uint+ind.cal]]
    ==
  --
--
