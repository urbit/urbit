::
=,  ethe
=,  ethereum
::
=|  pk=@
=|  addr=address
=|  gas-price=@ud
=|  now=@da
::
|_  [nonce=@ud transactions=(list cord)]
::
+*  this  .
::
+$  rights
  $:  own=address
      transfer=address
      spawn=(unit address)
      net=(unit [crypt=@ux auth=@ux])
  ==
::
+$  chart
  %+  map  ship  ::  galaxies
  %+  map  ship  ::  stars
  %-  set  ship  ::  planets
::
::TODO  into zuse
++  address-from-prv
  =,  secp256k1:secp:crypto
  =,  keccak:crypto
  |=  pk=@
  %^  end  3  20
  %+  keccak-256  64
  %^  rev  3  64
  %-  serialize-point
  (priv-to-pub pk)
::
++  tape-to-ux
  |=  t=tape
  (scan t zero-ux)
::
++  zero-ux
  ;~(pfix (jest '0x') hex)
::
++  init
  |=  [n=@da g=@ud]
  ^+  this
  =/  pkf
    .^  (list cord)  %cx
        /(scot %p ~zod)/home/(scot %da now)/pk/txt
    ==
  ?>  ?=(^ pkf)
  =+  prv=(rash i.pkf ;~(pfix (jest '0x') hex))
  %_  this
    now         n
    gas-price   g
    pk          prv
    addr        (address-from-prv prv)
  ==
::
++  get-account-proxies
  ^-  (list [own=address manage=address delegate=(unit address)])
  =/  lis=(list cord)
    .^  (list cord)  %cx
        /(scot %p ~zod)/home/(scot %da now)/accounts/txt
    ==
  %+  turn  lis
  |=  c=cord
  %+  rash  c
  ;~  (glue com)
    zero-ux
    zero-ux
    (punt zero-ux)
  ==
::
++  get-ship-deeds
  ^-  (list [who=ship rights])
  =/  lis=(list cord)
    .^  (list cord)  %cx
        /(scot %p ~zod)/home/(scot %da now)/deeds/txt
    ==
  %+  turn  lis
  |=  c=cord
  %+  rash  c
  ;~  (glue com)
    ;~(pfix sig fed:ag)
    zero-ux
    zero-ux
    (punt zero-ux)
    (punt ;~(plug zero-ux ;~(pfix com zero-ux)))
  ==
::
++  write-tx
  |=  tx=transaction
  =-  this(transactions [- transactions])
  (crip '0' 'x' ((x-co:co 0) (sign-transaction tx pk)))
::
++  complete
  ~&  [%writing-transactions (lent transactions)]
  (flop transactions)
::
++  get-contract-address
  =+  dat=(encode-atoms:rlp:ethereum ~[addr nonce])
  =+  wid=(met 3 dat)
  %^  end  3  20
  (keccak-256:keccak:crypto wid (rev 3 wid dat))
::
++  deploy-contract
  |=  [wat=cord arg=(list data)]
  =/  cod=(list cord)
    .^  (list cord)  %cx
        /(scot %p ~zod)/home/(scot %da now)/contracts/[wat]/txt
    ==
  ?>  ?=(^ cod)
  %-  tape-to-ux
  (weld (trip i.cod) (encode-args arg))
::
++  do-deploy
  |=  [wat=cord arg=(list data)]
  ^-  [address _this]
  :-  get-contract-address
  %^  do  0x0  6.000.000
  (deploy-contract wat arg)
::
++  do
  ::TODO  maybe reconsider encode-call interface, if we end up wanting @ux
  ::      as or more often than we want tapes
  |=  [to=address gas=@ud dat=$@(@ux tape)]
  %-  write-tx(nonce +(nonce))
  :*  nonce
      gas-price
      gas
      to
      0
      `@`?@(dat dat (tape-to-ux dat))
      0x1
  ==
::
++  sequence
  |=  [won=@da gasp=@ud]
  =.  this  (init(now won) won gasp)
  ::NOTE  we do these first so that we are sure we have sane files,
  ::      without waiting for that answer
  =+  accounts=get-account-proxies
  =+  deeds=get-ship-deeds
  =/  deed-map=(map ship rights)
    (~(gas by *(map ship rights)) deeds)
  ~&  'Deed data sanity check...'
  =/  galaxies=chart
    %+  roll  deeds
    |=  [[who=ship *] net=chart]
    ^+  net
    =+  par=(sein:title who)
    ~|  [%need-parent par %for who]
    ?>  ?|  =(who par)
            ?&  (~(has by deed-map) par)
              ::
                =+  net:(~(got by deed-map) par)
                ?=(^ -)
            ==
        ==
    %-  ~(put by net)
    ^-  [ship (map ship (set ship))]
    ?+  (clan:title who)  !!
      %czar  [who (fall (~(get by net) who) ~)]
      %king  :-  par
             =+  gm=(fall (~(get by net) par) ~)
             %+  ~(put by gm)  who
             (fall (~(get by gm) who) ~)
      %duke  =+  gs=(sein:title par)
             :-  gs
             =+  gm=(fall (~(get by net) gs) ~)
             =+  sm=(fall (~(get by gm) par) ~)
             %+  ~(put by gm)  par
             (~(put in sm) who)
    ==
  ::
  ::  contract deployment
  ::
  ~&  'Deploying ships...'
  =^  ships  this
    (do-deploy 'ships' ~)
  ~&  'Deploying polls...'
  =^  polls  this
    %+  do-deploy  'polls'
    ~[uint+1.209.600 uint+604.800]  ::TODO  decide on values
  ~&  'Deploying claims...'
  =^  claims  this
    %+  do-deploy  'claims'
    ~[address+ships]
  ~&  'Deploying constitution-ceremony...'
  =^  constit  this
    %+  do-deploy  'constitution-ceremony'
    :~  [%address 0x0]
        [%address ships]
        [%address polls]
        [%address 0x0]  ::TODO  standard ens registry
        [%string "urbit-eth"]  ::TODO  ens domain
        [%string "constitution"]  ::TODO  ens subdomain
        [%address claims]
    ==
  ::
  ::  contract configuration
  ::
  ~&  'Transferring contract ownership...'
  =.  this
    %^  do  ships  50.000
    ((cork transfer-ownership:cal encode-call) constit)
  =.  this
    %^  do  polls  50.000
    (transfer-ownership:dat constit)
  ::
  ::TODO  deploy linearstarrelease, conditionalstarrelease
  ::TODO  deploy censures, delegatedsending
  ::
  ::  owner proxy configuration
  ::
  =*  do-constit  (cury (cury do constit) 6.000.000)
  ~&  'Assigning managers and delegates...'
  |-
  ?^  accounts
    =*  acc  i.accounts
    =.  this
      %-  do-constit
      (set-manager-for:dat own.acc manage.acc)
    =?  this  ?=(^ delegate.acc)
      %-  do-constit
      (set-delegate-for:dat own.acc u.delegate.acc)
    $(accounts t.accounts)
  ::
  ::  ship deeding and configuration
  ::
  ~&  'Deeding regular assets...'
  =/  gs
    %+  sort  ~(tap by galaxies)
    |=  [[a=ship *] [b=ship *]]
    (lth a b)
  ~&  galaxies
  ~&  gs
  |-
  ~&  [(lent gs) 'galaxies remaining']
  ?^  gs
    =*  gal  p.i.gs
    =*  sas  q.i.gs
    =+  gad=(~(got by deed-map) gal)
    ~&  [gal ~(key by sas)]
    =.  this
      %-  do-constit
      (create-galaxy:dat gal)
    =?  this  ?=(^ spawn.gad)
      %-  do-constit
      (set-spawn-proxy:dat gal u.spawn.gad)
    =?  this  ?=(^ net.gad)
      %-  do-constit
      (configure-keys:dat gal u.net.gad)
    ::
    =/  ss
      %+  sort  ~(tap by sas)
      |=  [[a=ship *] [b=ship *]]
      (lth a b)
    |-
    ?^  ss
      =*  sar  p.i.ss
      =*  pas  q.i.ss
      =+  sad=(~(got by deed-map) sar)
      ~&  [sar pas]
      =.  this
        %-  do-constit
        (spawn:dat sar own.sad)
      =?  this  ?=(^ spawn.sad)
        %-  do-constit
        (set-spawn-proxy:dat sar u.spawn.sad)
      =?  this  ?=(^ net.sad)
        %-  do-constit
        (configure-keys:dat sar u.net.sad)
      ::
      =+  ps=(sort ~(tap in pas) lth)
      |-
      ?^  ps
        =*  pan  i.ps
        =+  pad=(~(got by deed-map) pan)
        ~&  pan
        =.  this
          %-  do-constit
          (spawn:dat pan own.pad)
        =?  this  ?=(^ net.pad)
          %-  do-constit
          (configure-keys:dat pan u.net.pad)
        =.  this
          %-  do-constit
          (transfer-ship:dat pan own.pad)
        =.  this
          %-  do-constit
          (set-transfer-proxy-for:dat pan transfer.pad)
        ~&  %next-planet
        $(ps t.ps)
      ::
      =.  this
        %-  do-constit
        (transfer-ship:dat sar own.gad)
      =.  this
        %-  do-constit
        (set-transfer-proxy-for:dat sar transfer.gad)
      ~&  %next-star
      ^$(ss t.ss)
    ::
    =.  this
      %-  do-constit
      (transfer-ship:dat gal own.gad)
    =.  this
      %-  do-constit
      (set-transfer-proxy-for:dat gal transfer.gad)
    ~&  %next-galaxy
    ^$(gs t.gs)
  complete
  ::TODO  deploy true-constitution(ceremony-constitution, ships, polls,
  ::                               ensRegistry, 'urbit-eth', 'constitution',
  ::                               claims)
  ::TODO  upgrade-to
::
::TODO  most of these should later be cleaned and go in ++constitution
::
++  dat
  |%
  ++  enc
    |*  cal=$-(* call-data)
    (cork cal encode-call)
  ::
  ++  create-galaxy           (enc create-galaxy:cal)
  ++  spawn                   (enc spawn:cal)
  ++  configure-keys          (enc configure-keys:cal)
  ++  set-spawn-proxy         (enc set-spawn-proxy:cal)
  ++  transfer-ship           (enc transfer-ship:cal)
  ++  set-manager-for         (enc set-manager-for:cal)
  ++  set-delegate-for        (enc set-delegate-for:cal)
  ++  set-transfer-proxy-for  (enc set-transfer-proxy-for:cal)
  ++  upgrade-to              (enc upgrade-to:cal)
  ++  transfer-ownership      (enc transfer-ownership:cal)
  --
::
++  cal
  |%
  ++  create-galaxy
    |=  gal=ship
    ^-  call-data
    ?>  =(%czar (clan:title gal))
    :-  'createGalaxy(uint8,address)'
    ^-  (list data)
    :~  [%uint `@`gal]
        [%address addr]
    ==
  ::
  ++  spawn
    |=  [who=ship to=address]
    ^-  call-data
    ?>  ?=(?(%king %duke) (clan:title who))
    :-  'spawn(uint32,address)'
    :~  [%uint `@`who]
        [%address addr]
    ==
  ::
  ++  configure-keys
    |=  [who=ship crypt=@ auth=@]
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
    :-  'transferShip(uint32,address,bool)'
    :~  [%uint `@`who]
        [%address to]
        [%bool |]
    ==
  ::
  ++  set-manager-for
    |=  [who=address proxy=address]
    ^-  call-data
    :-  'setManagerFor(address,address)'
    :~  [%address who]
        [%address proxy]
    ==
  ::
  ++  set-delegate-for
    |=  [who=address proxy=address]
    ^-  call-data
    :-  'setDelegateFor(address,address)'
    :~  [%address who]
        [%address proxy]
    ==
  ::
  ++  set-transfer-proxy-for
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setTransferProxyFor(uint32,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  upgrade-to
    |=  to=address
    ^-  call-data
    :-  'upgradeTo(address)'
    :~  [%address to]
    ==
  ::
  ++  transfer-ownership  ::  of contract
    |=  to=address
    ^-  call-data
    :-  'transferOwnership(address)'
    :~  [%address to]
    ==
  --
--
