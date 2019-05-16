::  claz: command line azimuth, for the power-user
::
/-  sole-sur=sole
/+  sole-lib=sole
::
=,  azimuth
=,  ethereum
=,  rpc
=,  key
::
|%
++  state
  $:  cli=shell
  ==
::
++  shell
  $:  id=bone
      say=sole-share:sole-sur
  ==
::
++  command
  $%  [%generate =path =batch]
  ==
::
++  batch
  $%  [%single =network nonce=@ud =call]
  ==
::
++  network
  $?  %main
      %ropsten
      %fake
      [%other id=@]
  ==
::
++  call
  $%  [%create-galaxy gal=ship to=address]
      [%spawn who=ship to=address]
      [%configure-keys who=ship crypt=@ auth=@]
      [%set-management-proxy who=ship proxy=address]
      [%set-voting-proxy who=ship proxy=address]
      [%set-spawn-proxy who=ship proxy=address]
      [%transfer-ship who=ship to=address]
      [%set-transfer-proxy who=ship proxy=address]
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
      [%info wire desk nori:clay]
      [%rest wire @da]
      [%wait wire @da]
  ==
::
::
++  ecliptic  `address`0x6ac0.7b7c.4601.b5ce.11de.8dfe.6335.b871.c7c4.dd4d
--
::
|_  [=bowl:gall state]
++  this  .
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  [~ ..prep]
::
++  poke-noun
  |=  =command
  ^-  (quip move _this)
  ?-  -.command
      %generate
    =-  [[- ~] this]
    %+  write-file-transactions
      path.command
    ?-  -.batch.command
      %single  [(single +.batch.command) ~]
    ==
  ==
::
++  tape-to-ux
  |=  t=tape
  (scan t zero-ux)
::
++  zero-ux
  ;~(pfix (jest '0x') hex)
::
++  write-file-transactions
  |=  [pax=path tox=(list transaction)]
  ^-  move
  ?>  ?=([@ desk @ *] pax)
  :*  ost.bowl
      %info
      (weld /write pax)
      :: our.bowl
      `desk`i.t.pax
      =-  &+[t.t.t.pax -]~
      =/  y  .^(arch %cy pax)
      ?~  fil.y
        ins+eth-txs+!>(tox)
      mut+eth-txs+!>(tox)
  ==
::
++  do
  ::TODO  maybe reconsider encode-call interface, if we end up wanting @ux
  ::      as or more often than we want tapes
  |=  [=network nonce=@ud to=address gas=@ud dat=$@(@ux tape)]
  ^-  transaction
  :*  nonce
      8.000.000.000.000
      600.000
      to
      0
      `@`?@(dat dat (tape-to-ux dat))
      ?-  network
        %main       0x1
        %ropsten    0x3
        %fake       `@ux``@`1.337
        [%other *]  id.network
      ==
  ==
::
++  single
  |=  [=network nonce=@ud =call]
  ^-  transaction
  =-  (do network nonce ecliptic 5.000.000.000 -)
  ?-  -.call
    %create-galaxy  (create-galaxy:dat +.call)
    %spawn  (spawn:dat +.call)
    %configure-keys  (configure-keys:dat +.call)
    %set-management-proxy  (set-management-proxy:dat +.call)
    %set-voting-proxy  (set-voting-proxy:dat +.call)
    %set-spawn-proxy  (set-spawn-proxy:dat +.call)
    %transfer-ship  (transfer-ship:dat +.call)
    %set-transfer-proxy  (set-transfer-proxy:dat +.call)
  ==
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
  ++  set-transfer-proxy      (enc set-transfer-proxy:cal)
  ++  set-dns-domains         (enc set-dns-domains:cal)
  ++  upgrade-to              (enc upgrade-to:cal)
  ++  transfer-ownership      (enc transfer-ownership:cal)
  ++  register-linear         (enc register-linear:cal)
  ++  register-conditional    (enc register-conditional:cal)
  ++  deposit                 (enc deposit:cal)
  --
::
++  cal
  |%
  ++  create-galaxy
    |=  [gal=ship to=address]
    ^-  call-data
    ?>  =(%czar (clan:title gal))
    :-  'createGalaxy(uint8,address)'
    ^-  (list data)
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
  ::
  ++  register-linear
    |=  $:  to=address
            windup=@ud
            stars=@ud
            rate=@ud
            rate-unit=@ud
        ==
    ^-  call-data
    ~&  [%register-linear stars to]
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
  --
::
:: ++  peer-sole
::   |=  =path
::   =.  id.cli  ost.bowl
::   TODO...
:: ::
:: ++  sh
::   |_  she=shell
::   ::
::   ::  #  %resolve
::   +|  %resolve
::   ::
::   ++  sh-done
::     ::  stores changes to the cli.
::     ::
::     ^+  +>
::     +>(cli she)
::   ::
::   ::  #
::   ::  #  %emitters
::   ::  #
::   ::    arms that create outward changes.
::   +|  %emitters
::   ::
::   ++  sh-fact
::     ::  adds a console effect to ++ta's moves.
::     ::
::     |=  fec/sole-effect:sole-sur
::     ^+  +>
::     +>(moves [[id.she %diff %sole-effect fec] moves])
::   ::
::   ++  sh-prod
::     ::    show prompt
::     ::
::     ::  makes and stores a move to modify the cli
::     ::  prompt to display the current audience.
::     ::
::     ^+  .
::     %+  sh-fact  %pro
::     :+  &  %talk-line
::     ^-  tape
::     =/  rew/(pair (pair cord cord) audience)
::         [['[' ']'] active.she]
::     =+  cha=(~(get by bound) q.rew)
::     ?^  cha  ~[u.cha ' ']
::     =+  por=~(ar-prom ar q.rew)
::     (weld `tape`[p.p.rew por] `tape`[q.p.rew ' ' ~])
::   ::
::   --
--