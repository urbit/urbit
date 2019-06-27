=,  ethe
|%
++  ceremony        0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964
++  conditional-sr  0x8c24.1098.c3d3.498f.e126.1421.633f.d579.86d7.4aea
++  linear-sr       0x86cd.9cd0.992f.0423.1751.e376.1de4.5cec.ea5d.1801
::
++  tmp-deed
  $:  owner=address
      transfer-proxy=address
      spawn-proxy=address
      management-proxy=address
      voting-proxy=address
      auth=@ux
      crypt=@ux
  ==
::
++  compare
  |=  [base=beak txs=path addrs=path]
  ~&  %loading-txs
  =+  txs=.^((list transaction) %cx (en-beam:format base (flop txs)))
  ~&  %loading-registry
  =/  reg
    %-  ~(gas by *(map ship tmp-deed))
    %+  murn  .^((list cord) %cx (en-beam:format base (flop addrs)))
    =/  hax
      ;~  pose
        ;~(pfix (jest '0x') hex)
        hex
        (cold `@`0 (jest '""'))
      ==
    |=  lin=cord
    ^-  (unit (pair ship tmp-deed))
    ?:  =('' lin)  ~
    :-  ~
    %+  rash  lin
    ::  ship,ownership,transfer,spawn,mgmt,voting,auth,crypt
    ;~((glue com) dem hax hax hax hax hax hax hax)
  ~&  %checking-transactions
  %+  turn  txs
  |=  transaction
  =+  arg=(args data)
  ?:  (is-function 'transferPoint(uint32,address,bool)' data)
    (check-transfer-point arg reg)
  ?:  (is-function 'setTransferProxy(uint32,address)' data)
    (check-transfer-proxy arg reg)
  ?:  (is-function 'setSpawnProxy(uint32,address)' data)
    (check-spawn-proxy arg reg)
  ?:  (is-function 'setManagementProxy(uint32,address)' data)
    (check-manage-proxy arg reg)
  ?:  (is-function 'setVotingProxy(uint32,address)' data)
    (check-voting-proxy arg reg)
  ?:  (is-function 'configureKeys(uint32,bytes32,bytes32,uint32,bool)' data)
    ~|  arg
    ~|  `@ux`data
    (check-configure-keys arg reg)
  ~
::
++  is-function
  |=  [fun=@t dat=@ux]
  .=  (rsh 3 (sub (met 3 dat) 4) dat)
  %^  rsh  3  28
  (keccak-256:keccak:crypto (met 3 fun) fun)
::
++  args
  |=  dat=@ux
  %-  flop
  %+  rip  8
  (end 3 (sub (met 3 dat) 4) dat)
::
++  check-transfer-point
  |=  [arg=(list @) reg=(map ship tmp-deed)]
  =+  who=(snag 0 arg)
  =+  wer=(snag 1 arg)
  ~|  [`@p`who `@`who]
  ?:  =(wer ceremony)  ~
  =+  (~(got by reg) who)
  ~?  !=(wer owner)
    [%owner-mismatch `@p`who %registry owner %tx `@ux`wer]
  ~
::
++  check-transfer-proxy
  |=  [arg=(list @) reg=(map ship tmp-deed)]
  =+  who=(snag 0 arg)
  =+  wer=(snag 1 arg)
  ~|  [`@p`who `@`who]
  ?:  =(wer ceremony)  ~
  =+  (~(got by reg) who)
  ~?  !=(wer transfer-proxy)
    [%transfer-proxy-mismatch `@p`who %registry transfer-proxy %tx `@ux`wer]
  ~
::
++  check-spawn-proxy
  |=  [arg=(list @) reg=(map ship tmp-deed)]
  =+  who=(snag 0 arg)
  =+  wer=(snag 1 arg)
  ~|  [`@p`who `@`who]
  ~?  ?&  !=(wer conditional-sr)
          !=(wer linear-sr)
        ::
          =+  (~(got by reg) who)
          !=(wer spawn-proxy)
      ==
    [%spawn-proxy-mismatch `@p`who %registry 'todo' %tx `@ux`wer]
  ~
::
++  check-manage-proxy
  |=  [arg=(list @) reg=(map ship tmp-deed)]
  =+  who=(snag 0 arg)
  =+  wer=(snag 1 arg)
  ~|  [`@p`who `@`who]
  =+  (~(got by reg) who)
  ~?  !=(wer management-proxy)
    [%manage-proxy-mismatch `@p`who %registry management-proxy %tx `@ux`wer]
  ~
::
++  check-voting-proxy
  |=  [arg=(list @) reg=(map ship tmp-deed)]
  =+  who=(snag 0 arg)
  =+  wer=(snag 1 arg)
  ~|  [`@p`who `@`who]
  =+  (~(got by reg) who)
  ~?  !=(wer voting-proxy)
    [%voting-proxy-mismatch `@p`who %registry voting-proxy %tx `@ux`wer]
  ~
::
++  check-configure-keys
  |=  [arg=(list @) reg=(map ship tmp-deed)]
  ?:  =(2 (lent arg))  ~&  %probably-zod  ~
  =+  who=(snag 0 arg)
  =+  cry=(snag 1 arg)
  =+  aut=(snag 2 arg)
  ~|  [`@p`who `@`who]
  ?.  (~(has by reg) who)
    ~&  [%skipping-key-check-for `@p`who]
    ~
  =+  (~(got by reg) who)
  ~?  ?&  &(=(0 cry) =(0 aut))
          |(!=(cry crypt) !=(aut auth))
      ==
    :^  %pubkey-mismatch  `@p`who
      [%registry crypt auth]
    [%tx `@ux`cry `@ux`aut]
  ~
--
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [txs=path addrs=path ~]
        ~
    ==
=+  (compare bec txs addrs)
tape+"done"