::  claz: call data generation
::
/-  *claz
/+  *ethereum, azimuth
::
|%
++  read-invites  ::TODO  lib
  |=  file=path
  ^-  (list [=ship ticket=@q =address])
  =+  txt=.^((list cord) %cx file)
  %+  murn  txt
  |=  line=cord
  ^-  (unit [ship @q address])
  ?:  =('' line)  ~
  %-  some
  ~|  line
  %+  rash  line
  ;~  (glue com)
    ;~(pfix sig fed:ag)
    ;~(pfix sig feq:ag)
    ;~(pfix (jest '0x') hex)
  ==
::
++  get-contracts
  |=  =network
  ?+  network  ~&(%careful-fallback-contracts mainnet-contracts:azimuth)
    %mainnet  mainnet-contracts:azimuth
    %goerli   goerli-contracts:azimuth
  ==
::
++  encode-claz-call
  |=  =call
  ?-  -.call
    %create-galaxy         (create-galaxy:dat +.call)
    %spawn                 (spawn:dat +.call)
    %configure-keys        (configure-keys:dat +.call)
    %set-management-proxy  (set-management-proxy:dat +.call)
    %set-voting-proxy      (set-voting-proxy:dat +.call)
    %set-spawn-proxy       (set-spawn-proxy:dat +.call)
    %transfer-ship         (transfer-ship:dat +.call)
    %set-transfer-proxy    (set-transfer-proxy:dat +.call)
    %adopt                 (adopt:dat +.call)
    %start-document-poll   (start-document-poll:dat +.call)
    %cast-document-vote    (cast-document-vote:dat +.call)
    %start-upgrade-poll    (start-upgrade-poll:dat +.call)
    %cast-upgrade-vote     (cast-upgrade-vote:dat +.call)
  ::
    %send-point  (send-point:dat +.call)
  ::
    %approve-batch-transfer  (approve-batch-transfer:dat +.call)
    %transfer-batch          (transfer-batch:dat +.call)
    %withdraw                (withdraw:dat +.call)
  ==
::
+$  call-data  call-data:rpc
++  dat
  |%
  ++  enc
    |*  cal=$-(* call-data)
    (cork cal encode-call:rpc)
  ::
  ++  create-galaxy           (enc create-galaxy:cal)
  ++  spawn                   (enc spawn:cal)
  ++  configure-keys          (enc configure-keys:cal)
  ++  set-spawn-proxy         (enc set-spawn-proxy:cal)
  ++  transfer-ship           (enc transfer-ship:cal)
  ++  set-management-proxy    (enc set-management-proxy:cal)
  ++  set-voting-proxy        (enc set-voting-proxy:cal)
  ++  set-transfer-proxy      (enc set-transfer-proxy:cal)
  ++  set-dns-domains         (enc set-dns-domains:cal)
  ++  upgrade-to              (enc upgrade-to:cal)
  ++  transfer-ownership      (enc transfer-ownership:cal)
  ++  adopt                   (enc adopt:cal)
  ++  start-document-poll     (enc start-document-poll:cal)
  ++  cast-document-vote      (enc cast-document-vote:cal)
  ++  start-upgrade-poll      (enc start-upgrade-poll:cal)
  ++  cast-upgrade-vote       (enc cast-upgrade-vote:cal)
  ::
  ++  register-linear         (enc register-linear:cal)
  ++  register-conditional    (enc register-conditional:cal)
  ++  deposit                 (enc deposit:cal)
  ::
  ++  send-point              (enc send-point:cal)
  ::
  ++  approve-batch-transfer  (enc approve-batch-transfer:cal)
  ++  transfer-batch          (enc transfer-batch:cal)
  ++  withdraw                (enc withdraw:cal)
  --
::
::TODO  lib
++  cal
  |%
  ++  create-galaxy
    |=  [gal=ship to=address]
    ^-  call-data
    ?>  =(%czar (clan:title gal))
    :-  'createGalaxy(uint8,address)'
    :~  [%uint `@`gal]
        [%address to]
    ==
  ::
  ++  spawn
    |=  [who=ship to=address]
    ^-  call-data
    ?>  ?=(?(%king %duke) (clan:title who))
    :-  'spawn(uint32,address)'
    :~  [%uint `@`who]
        [%address to]
    ==
  ::
  ++  configure-keys
    |=  [who=ship crypt=@ auth=@]
    ^-  call-data
    ::TODO  maybe disable asserts?
    ?>  (lte (met 3 crypt) 32)
    ?>  (lte (met 3 auth) 32)
    :-  'configureKeys(uint32,bytes32,bytes32,uint32,bool)'
    :~  [%uint `@`who]
        [%bytes-n 32^crypt]
        [%bytes-n 32^auth]
        [%uint 1]
        [%bool |]
    ==
  ::
  ++  set-management-proxy
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setManagementProxy(uint32,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  set-voting-proxy
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setVotingProxy(uint8,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  set-spawn-proxy
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setSpawnProxy(uint16,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  transfer-ship
    |=  [who=ship to=address]
    ^-  call-data
    :-  'transferPoint(uint32,address,bool)'
    :~  [%uint `@`who]
        [%address to]
        [%bool |]
    ==
  ::
  ++  set-transfer-proxy
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setTransferProxy(uint32,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  start-document-poll
    |=  [gal=ship hash=@]
    ^-  call-data
    ?>  =(%czar (clan:title gal))
    :-  'startDocumentPoll(uint8,bytes32)'
    :~  [%uint `@`gal]
        [%bytes-n 32^hash]
    ==
  ::
  ++  cast-document-vote
    |=  [gal=ship hash=@ support=?]
    ^-  call-data
    ?>  =(%czar (clan:title gal))
    :-  'castDocumentVote(uint8,bytes32,bool)'
    :~  [%uint `@`gal]
        [%bytes-n 32^hash]
        [%bool support]
    ==
  ::
  ++  start-upgrade-poll
    |=  [gal=ship =address]
    ^-  call-data
    ?>  =(%czar (clan:title gal))
    :-  'startUpgradePoll(uint8,address)'
    :~  [%uint `@`gal]
        [%address address]
    ==
  ::
  ++  cast-upgrade-vote
    |=  [gal=ship =address support=?]
    ^-  call-data
    ?>  =(%czar (clan:title gal))
    :-  'castUpgradeVote(uint8,address,bool)'
    :~  [%uint `@`gal]
        [%address address]
        [%bool support]
    ==
  ::
  ::
  ++  set-dns-domains
    |=  [pri=tape sec=tape ter=tape]
    ^-  call-data
    :-  'setDnsDomains(string,string,string)'
    :~  [%string pri]
        [%string sec]
        [%string ter]
    ==
  ::
  ++  upgrade-to
    |=  to=address
    ^-  call-data
    :-  'upgradeTo(address)'
    :~  [%address to]
    ==
  ::
  ::
  ++  transfer-ownership  ::  of contract
    |=  to=address
    ^-  call-data
    :-  'transferOwnership(address)'
    :~  [%address to]
    ==
  ::
  ++  adopt
    |=  who=ship
    ^-  call-data
    :-  'adopt(uint32)'
    :~  [%uint `@`who]
    ==
  ::
  ::
  ++  register-linear
    |=  $:  to=address
            windup=@ud
            stars=@ud
            rate=@ud
            rate-unit=@ud
        ==
    ^-  call-data
    :-  'register(address,uint256,uint16,uint16,uint256)'
    :~  [%address to]
        [%uint windup]
        [%uint stars]
        [%uint rate]
        [%uint rate-unit]
    ==
  ::
  ++  register-conditional
    |=  $:  to=address
            b1=@ud
            b2=@ud
            b3=@ud
            rate=@ud
            rate-unit=@ud
        ==
    ^-  call-data
    :-  'register(address,uint16[],uint16,uint256)'
    :~  [%address to]
        [%array ~[uint+b1 uint+b2 uint+b3]]
        [%uint rate]
        [%uint rate-unit]
    ==
  ::
  ++  deposit
    |=  [to=address star=ship]
    ^-  call-data
    :-  'deposit(address,uint16)'
    :~  [%address to]
        [%uint `@`star]
    ==
  ::
  ++  send-point
    |=  [as=ship point=ship to=address]
    ^-  call-data
    :-  'sendPoint(uint32,uint32,address)'
    :~  [%uint `@`as]
        [%uint `@`point]
        [%address to]
    ==
  ::
  ++  approve-batch-transfer
    |=  to=address
    ^-  call-data
    :-  'approveBatchTransfer(address)'
    :~  [%address to]
    ==
  ::
  ++  transfer-batch
    |=  from=address
    ^-  call-data
    :-  'transferBatch(address)'
    :~  [%address from]
    ==
  ::
  ++  withdraw
    |=  to=address
    ^-  call-data
    :-  'withdraw(address)'
    :~  [%address to]
    ==
  ::
  ::  read calls
  ::
  ++  rights
    |=  =ship
    ^-  call-data
    :-  'rights(uint32)'
    :~  [%uint `@`ship]
    ==
  ::
  ++  get-pool
    |=  =ship
    ^-  call-data
    :-  'getPool(uint32)'
    :~  [%uint `@`ship]
    ==
  ::
  ++  pools
    |=  [pool=@ud star=ship]
    ^-  call-data
    :-  'pools(uint32,uint16)'
    :~  [%uint pool]
        [%uint `@`star]
    ==
  --
--
