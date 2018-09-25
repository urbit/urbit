::
=,  ethe
=,  ethereum
::
=|  pk=@
=|  addr=address
=|  gas-price=@ud
=|  now=@da
::
|_  $:  nonce=@ud                                       ::  next tx id
        transactions=(list cord)                        ::  generated txs
        constitution=address                            ::  deployed address
    ==
::
++  this  .
::
+$  rights
  $:  own=address
      manage=address
      delegate=(unit address)
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
++  get-file
  |=  pax=path
  .^  (list cord)  %cx
      (weld /(scot %p ~zod)/home/(scot %da now) pax)
  ==
::
++  order-shiplist
  |=  [[a=ship *] [b=ship *]]
  (lth a b)
::
++  init
  |=  [n=@da g=@ud]
  ^+  this
  =+  pkf=(get-file /pk/txt)
  ?>  ?=(^ pkf)
  =+  prv=(rash i.pkf ;~(pfix (jest '0x') hex))
  %_  this
    now         n
    gas-price   g
    pk          prv
    addr        (address-from-prv prv)
  ==
::
++  get-ship-deeds
  ^-  (list [who=ship rights])
  =+  lis=(get-file /deeds/txt)
  %+  turn  lis
  |=  c=cord
  %+  rash  c
  ;~  (glue com)
    ;~(pfix sig fed:ag)
    zero-ux
    zero-ux
    (punt zero-ux)
    zero-ux
    (punt zero-ux)
    (punt ;~(plug zero-ux ;~(pfix com zero-ux)))
  ==
::
++  get-linear-deeds
  ^-  %-  list
      $:  who=ship
          windup=@ud
          rate=@ud
          rate-unit=@ud
          rights
      ==
  =+  lis=(get-file /deeds-linear/txt)
  %+  turn  lis
  |=  c=cord
  %+  rash  c
  ;~  (glue com)
    ;~(pfix sig fed:ag)
    dum:ag
    dum:ag
    dum:ag
    zero-ux
    zero-ux
    (punt zero-ux)
    zero-ux
    (punt zero-ux)
    (punt ;~(plug zero-ux ;~(pfix com zero-ux)))
  ==
::
++  write-tx
  |=  tx=transaction
  ^+  this
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
  =+  cod=(get-file /contracts/[wat]/txt)
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
  ^+  this
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
  ::
  ::  data loading
  ::
  ::NOTE  we do these first so that we are sure we have sane files,
  ::      without waiting for that answer
  =+  deeds=get-ship-deeds
  =/  deed-map=(map ship rights)
    (~(gas by *(map ship rights)) deeds)
  =+  linears=get-linear-deeds
  ~&  'Deed data sanity check...'
  =/  galaxy-deeds=chart
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
  =.  constitution  constit
  ~&  'Deploying linear-star-release...'
  =^  linear-star-release  this
    %+  do-deploy  'linear-star-release'
    ~[address+ships]
  ::TODO  deploy conditionalstarrelease
  ::TODO  deploy censures, delegatedsending
  ::
  ::  contract configuration
  ::
  ~&  'Transferring contract ownership...'
  =.  this
    %^  do  ships  50.000
    (transfer-ownership:dat constit)
  =.  this
    %^  do  polls  50.000
    (transfer-ownership:dat constit)
  ::
  ::  simple deeding
  ::
  ~&  'Deeding regular assets...'
  =*  do-constit  (cury (cury do constit) 300.000)
  =/  galaxies  (sort ~(tap by galaxy-deeds) order-shiplist)
  |-
  ?^  galaxies
    ~&  [(lent galaxies) 'galaxies remaining']
    =*  galaxy  p.i.galaxies
    =+  gal-deed=(~(got by deed-map) galaxy)
    ~&  galaxy
    =.  this
      (prepare-ship galaxy [manage delegate spawn net]:gal-deed)
    ::
    =/  stars  (sort ~(tap by q.i.galaxies) order-shiplist)
    |-
    ?^  stars
      =*  star  p.i.stars
      =+  sta-deed=(~(got by deed-map) star)
      ~&  star
      =.  this
        (prepare-ship star [manage delegate spawn net]:sta-deed)
      ::
      =+  planets=(sort ~(tap in q.i.stars) lth)
      |-
      ?^  planets
        =*  planet  i.planets
        =+  pla-deed=(~(got by deed-map) planet)
        =.  this
          (prepare-ship planet [manage delegate ~ net]:pla-deed)
        =.  this
          (send-ship planet own.pla-deed transfer.pla-deed)
        $(planets t.planets)
      ::
      =.  this
        (send-ship star own.sta-deed transfer.sta-deed)
      ^$(stars t.stars)
    ::
    =.  this
      (send-ship galaxy own.gal-deed transfer.gal-deed)
    ^$(galaxies t.galaxies)
  ::
  ::  linear release deeding
  ::
  ::TODO  rewrite to take non-standard cases into account
  ~&  'Deeding linear release assets...'
  =*  do-linear  (cury (cury do linear-star-release) 350.000)
  =/  galaxies  (sort linears order-shiplist)
  |-
  ?^  galaxies
    ~&  [(lent galaxies) 'galaxies remaining']
    =*  galaxy  who.i.galaxies
    =*  gal-deed  i.galaxies
    ~&  galaxy
    =.  this
      (prepare-ship galaxy manage.gal-deed delegate.gal-deed `linear-star-release net.gal-deed)
    ::
    =.  this
      %-  do-linear
      %-  register-linear:dat
      :*  own.gal-deed
          windup.gal-deed
          rate.gal-deed
          rate-unit.gal-deed
      ==
    =+  stars=(gulf 1 255)
    |-
    ?^  stars
      =.  this
        %-  do-linear
        %-  deposit-linear:dat
        [own.gal-deed (cat 3 galaxy i.stars)]
      $(stars t.stars)
    ::
    =.  this
      %-  do-constit
      (set-spawn-proxy:dat galaxy (fall spawn.gal-deed 0x0))
    =.  this
      (send-ship galaxy own.gal-deed transfer.gal-deed)
    ^$(galaxies t.galaxies)
  ::
  ::  concluding ceremony
  ::
  ~&  'Deploying constitution-final...'
  =^  constit-final  this
    %+  do-deploy  'constitution-final'
    :~  [%address constit]
        [%address ships]
        [%address polls]
        [%address 0x0]  ::TODO  standard ens registry
        [%string "urbit-eth"]  ::TODO  ens domain
        [%string "constitution"]  ::TODO  ens subdomain
        [%address claims]
    ==
  =.  this
    ::NOTE  currently included bytecode has on-upgrade ens functionality
    ::      stripped out to make this not fail despite 0x0 dns contract
    %-  do-constit
    (upgrade-to:dat constit-final)
  complete
::
::  create or spawn a ship, configure its spawn proxy and pubkeys
++  prepare-ship
  |=  $:  who=ship
          manage=address
          voting=(unit address)
          spawn=(unit address)
          keys=(unit [@ux @ux])
      ==
  ^+  this
  =+  wat=(clan:title who)
  =*  do-c  (cury (cury do constitution) 300.000)
  =.  this
    ?:  ?=(%czar wat)
      (do-c (create-galaxy:dat who))
    (do-c (spawn:dat who))
  =.  this
    (do-c (set-management-proxy:dat who manage))
  =?  this  &(?=(^ voting) ?=(%czar wat))
    (do-c (set-voting-proxy:dat who u.voting))
  =?  this  &(?=(^ spawn) !?=(%duke wat))
    (do-c (set-spawn-proxy:dat who u.spawn))
  =?  this  ?=(^ keys)
    (do-c (configure-keys:dat who u.keys))
  this
::
::  transfer a ship to a new owner, set a transfer proxy
++  send-ship
  |=  [who=ship own=address transfer=address]
  ^+  this
  =*  do-c  (cury (cury do constitution) 300.000)
  =.  this
    (do-c (transfer-ship:dat who own))
  (do-c (set-transfer-proxy-for:dat who transfer))
::
::  call data generation
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
  ++  set-management-proxy    (enc set-management-proxy:cal)
  ++  set-voting-proxy        (enc set-voting-proxy:cal)
  ++  set-transfer-proxy-for  (enc set-transfer-proxy-for:cal)
  ++  upgrade-to              (enc upgrade-to:cal)
  ++  transfer-ownership      (enc transfer-ownership:cal)
  ++  register-linear         (enc register-linear:cal)
  ++  deposit-linear          (enc deposit-linear:cal)
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
    |=  who=ship
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
    :-  'transferShip(uint32,address,bool)'
    :~  [%uint `@`who]
        [%address to]
        [%bool |]
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
  ::
  ++  transfer-ownership  ::  of contract
    |=  to=address
    ^-  call-data
    :-  'transferOwnership(address)'
    :~  [%address to]
    ==
  ::
  ::
  ++  register-linear
    |=  $:  to=address
            windup=@ud
            rate=@ud
            rate-unit=@ud
        ==
    ^-  call-data
    :-  'register(address,uint256,uint16,uint16,uint256)'
    :~  [%address to]
        [%uint windup]
        [%uint 255]
        [%uint rate]
        [%uint rate-unit]
    ==
  ::
  ++  deposit-linear
    |=  [to=address star=ship]
    ^-  call-data
    :-  'deposit(address,uint16)'
    :~  [%address to]
        [%uint `@`star]
    ==
  --
--
