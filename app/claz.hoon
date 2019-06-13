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
      inp=in-progress
  ==
::
::  state & commands
::
++  shell
  $:  id=bone
      say=sole-share:sole-sur
  ==
::
++  command
  $%  [%generate =path =network as=address =batch]
  ==
::
++  network
  $?  %main
      %ropsten
      %fake
      [%other id=@]
  ==
::
++  batch
  $%  [%single =call]
      [%deed deeds-json=cord]
      [%lock what=(list ship) to=address =lockup]
  ==
::
++  lockup
  $%  [%linear windup-years=@ud unlock-years=@ud]
      [%conditional [b1=@ud b2=@ud b3=@ud] unlock-years-per-batch=@ud]
  ==
::
++  rights
  $:  own=address
      manage=(unit address)
      voting=(unit address)
      transfer=(unit address)
      spawn=(unit address)
      net=(unit [crypt=@ux auth=@ux])
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
::  monadic structures
::
::  in-progress: monad currently in progress
::
++  in-progress
  %-  unit
  $%  [%command command=eval-form:eval:null-glad]
  ==
::
::  null-glad: monad that produces nothing, "just effects"
::
++  null-glad   (glad ,~)
::
::  glad-input: ~ for initialization, value for node response
::
++  glad-input  (unit response:rpc:jstd)
::
::  glad-output-raw: moves + intermediate monad state/result
::
++  glad-output-raw
  |*  a=mold
  $~  [~ %done *a]
  $:  moves=(list move)
      $=  next
      $%  [%wait ~]
          [%cont self=(glad-form-raw a)]
          [%fail err=tang]
          [%done value=a]
      ==
  ==
::
::  glad-form-raw: shape of monad function
::
++  glad-form-raw
  |*  a=mold
  $-(glad-input (glad-output-raw a))
::
::  glad-fail: procudes failure result
::
++  glad-fail
  |=  err=tang
  |=  glad-input
  [~ %fail err]
::
::  glad: monad object for monads that produce moves at intermediate steps
::
++  glad
  |*  a=mold
  |%
  ++  output  (glad-output-raw a)
  ++  form    (glad-form-raw a)
  ::
  ::  pure: produce intermediate result
  ::
  ++  pure
    |=  arg=a
    ^-  form
    |=  glad-input
    [~ %done arg]
  ::
  ::  bind: run m-b until it's done. once done, call fun with its result
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(glad-form-raw b) fun=$-(b form)]
    ^-  form
    |=  input=glad-input
    =/  b-res=(glad-output-raw b)
      (m-b input)
    ^-  output
    :-  moves.b-res
    ?-  -.next.b-res
      %wait  [%wait ~]
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail err.next.b-res]
      %done  [%cont (fun value.next.b-res)]
    ==
  ::
  ::  eval: call take with the latest input to kick monad object into action
  ::
  ++  eval
    |%
    +$  eval-form
      $:  =form
      ==
    ::
    ::  from-form:  eval-form from form
    ::
    ++  from-form
      |=  =form
      ^-  eval-form
      form
    ::
    ::  eval-result: how far +take got
    ::
    +$  eval-result
      $%  [%next ~]
          [%fail err=tang]
          [%done value=a]
      ==
    ::
    ::  take: run the monad operations as far as they can go right now
    ::
    ++  take
      =|  moves=(list move)
      |=  [=eval-form =our=wire =glad-input]
      ^-  [[(list move) =eval-result] _eval-form]
      ::  run the current function
      ::
      =/  =output  (form.eval-form glad-input)
      ::  add moves
      ::
      =.  moves
        (weld moves moves.output)
      ::  case-wise handle next steps
      ::
      ?-  -.next.output
        %wait  [[moves %next ~] eval-form]
        %fail  [[moves %fail err.next.output] eval-form]
        %done  [[moves %done value.next.output] eval-form]
      ::
          %cont
        ::  recurse to run continuation (next function in monad),
        ::  which is always started off with "initialization" input
        ::
        %_  $
          form.eval-form   self.next.output
          glad-input       ~
        ==
      ==
    --
  --
::
::  effects
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
      [%info wire desk nori:clay]
      [%rest wire @da]
      [%wait wire @da]
  ==
::
::  constants
::
++  ecliptic  `address`0x6ac0.7b7c.4601.b5ce.11de.8dfe.6335.b871.c7c4.dd4d
--
::
|_  [=bowl:gall state]
++  this  .
::
::  entrypoints
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  [~ ..prep]
::
++  poke-noun
  |=  =command
  ^-  (quip move _this)
  ::  create active monad, store in state
  ::
  =.  inp
    %-  some
    :-  %command
    %-  from-form:eval:null-glad
    (deal-with-command command)
  ::  kick off monad
  ::
  (take-command-sigh / ~)
::
++  sigh-tang-nonce
  |=  [=wire =tang]
  ^-  (quip move _this)
  =.  tang  [leaf+"claz failed" tang]
  [~ (fail-command tang)]
::
++  sigh-json-rpc-response-command
  |=  [=wire =response:rpc:jstd]
  ^-  (quip move _this)
  (take-command-sigh wire `response)
::
++  take-command-sigh
  |=  [=wire response=glad-input]
  ^-  (quip move _this)
  ::  we expect this to be called only if we have an in-progress monad
  ::
  ?~  inp
    ~|(%no-in-progress !!)
  :: ?.  ?=(%command -.u.inp)  ::NOTE  mint-vain rn
  ::   ~|([%unexpected-response -.u.inp] !!)
  ::  kick in-progress monad with response, updating it with the next callable
  ::  it spits out
  ::
  =/  m  null-glad
  =^  r=[moves=(list move) =eval-result:eval:m]  command.u.inp
    (take:eval:m command.u.inp wire response)
  :-  moves.r
  ::  continue depending on the eval result
  ::
  ?-  -.eval-result.r
    ::  not done, don't change app state further
    ::
    %next  this
    ::  failed, clean & update app state
    ::
    %fail  (fail-command err.eval-result.r)
    ::  succeeded, finalize & update app state
    ::
    %done  (done-command value.eval-result.r)
  ==
::
::  monadic helpers
::
::  fail-command: handle fail of nonce-fetching monad
::
++  fail-command
  |=  err=tang
  ^+  this
  ~&  'command processing failed'
  ::TODO  error printing
  this(inp ~)
::
::  done-command: handle result of nonce-fetching monad
::
++  done-command
  |=  ~
  ^+  this
  ~&  %command-done
  this(inp ~)
::
::  just-do: emit effects from monad without further processing
::
++  just-do
  |=  =move
  ^-  form:null-glad
  |=  glad-input
  [[move ~] %done ~]
::
::  get-next-nonce: monad function for fetching a nonce
::
++  get-next-nonce
  |=  for=address
  =/  m  (glad ,@ud)
  ^-  form:m
  ;<  =json  bind:m
    %+  do-request-expect-json-result  `'some-id'
    ^-  request
    [%eth-get-transaction-count for]
  ^-  form:m
  ?.  ?=(%s -.json)
    (glad-fail *tang) ::TODO  proper error, "unexpected json"
  %-  pure:m
  (rash p.json ;~(pfix (jest '0x') hex))
::
++  do-request
  |=  [rid=(unit @t) =request]
  %+  do-hiss  %json-rpc-response
  ^-  hiss:eyre
  %+  json-request
    ::TODO  vary per network
    (need (de-purl:html 'http://eth-mainnet.urbit.org:8545'))
  (request-to-json rid request)
::
++  do-hiss
  |=  [=mark =hiss:eyre]
  ^-  form:null-glad
  |=  glad-input
  ^-  output:null-glad
  =-  [[[ost.bowl -] ~] %done ~]
  ::TODO  wire in sample?
  [%hiss /command ~ %json-rpc-response %hiss hiss]
::
++  expect-response
  =/  m  (glad response:rpc:jstd)
  ^-  form:m
  |=  in=glad-input
  ?~  in  [~ %wait ~]
  [~ %done u.in]
::
++  do-request-expect-json-result
  |=  [rid=(unit @) =request]
  =/  m  (glad json)
  ;<  ~  bind:m
    (do-request rid request)
  ;<  =response:rpc:jstd  bind:m
    expect-response
  ?.  ?=(%result -.response)
    (glad-fail *tang) ::TODO  make pretty error message
  (pure:m res.response)
::
::  transaction generation logic
::
++  deal-with-command
  |=  =command
  =/  m  null-glad
  ^-  form:m
  ;<  nonce=@ud  bind:m  (get-next-nonce as.command)
  ^-  form:m
  %-  just-do
  ?-  -.command
      %generate
    %+  write-file-transactions
      path.command
    ?-  -.batch.command
      %single  [(single nonce [network as +.batch]:command) ~]
      %deed    (deed nonce [network as +.batch]:command)
      %lock    (lock nonce [network as +.batch]:command)
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
  |=  [=network nonce=@ud to=address dat=$@(@ux tape)]
  ^-  transaction
  :*  nonce
      8.000.000.000.000  ::TODO  global config
      600.000  ::TODO  global config
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
  |=  [nonce=@ud =network as=address =call]
  ^-  transaction
  =-  (do network nonce ecliptic -)
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
++  deed
  |=  [nonce=@ud =network as=address deeds-json=cord]
  ^-  (list transaction)
  =/  deeds=(list [=ship rights])
    (parse-registration deeds-json)
  ::TODO  split per spawn proxy
  =|  txs=(list transaction)
  |^  ::  $
    ?~  deeds  (flop txs)
    =*  deed  i.deeds
    =.  txs
      ?.  ?=(%czar (clan:title ship.deed))
        %-  do-here
        (spawn:dat ship.deed as)
      ~|  %galaxy-held-by-ceremony
      ?>  =(0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964 as)
      ~&  [%assuming-galaxy-owned-by-ceremony ship.deed]
      txs
    =?  txs  ?=(^ net.deed)
      %-  do-here
      (configure-keys:dat [ship u.net]:deed)
    =?  txs  ?=(^ manage.deed)
      %-  do-here
      (set-management-proxy:dat [ship u.manage]:deed)
    =?  txs  ?=(^ voting.deed)
      %-  do-here
      (set-voting-proxy:dat [ship u.voting]:deed)
    =?  txs  ?=(^ spawn.deed)
      %-  do-here
      (set-spawn-proxy:dat [ship u.spawn]:deed)
    =.  txs
      %-  do-here
      (transfer-ship:dat [ship own]:deed)
    $(deeds t.deeds)
  ::
  ::TODO  maybe-do, take dat gat and unit argument
  ++  do-here
    |=  dat=tape
    :_  txs
    (do network (add nonce (lent txs)) ecliptic dat)
  --
::
++  parse-registration
  |=  reg=cord
  ^-  (list [=ship rights])
  ~|  %registration-json-insane
  =+  jon=(need (de-json:html reg))
  ~|  %registration-json-invalid
  ?>  ?=(%o -.jon)
  =.  p.jon  (~(del by p.jon) 'idCode')
  %+  turn  ~(tap by p.jon)
  |=  [who=@t deed=json]
  ^-  [ship rights]
  :-  (rash who dum:ag)
  ?>  ?=(%a -.deed)
  ::  array has contents of:
  ::  [owner, transfer, spawn, mgmt, delegate, auth_key, crypt_key]
  ~|  [%registration-incomplete deed (lent p.deed)]
  ?>  =(7 (lent p.deed))
  =<  :*  (. 0 %address)       ::  owner
          (. 3 %unit-address)  ::  management
          (. 4 %unit-address)  ::  voting
          (. 1 %unit-address)  ::  transfer
          (. 2 %unit-address)  ::  spawn
          (both (. 6 %key) (. 5 %key))  ::  crypt, auth
      ==
  |*  [i=@ud what=?(%address %unit-address %key)]
  =+  j=(snag i p.deed)
  ~|  [%registration-invalid-value what j]
  ?>  ?=(%s -.j)
  %+  rash  p.j
  =+  adr=;~(pfix (jest '0x') hex)
  ?-  what
    %address       adr
    %unit-address  ;~(pose (stag ~ adr) (cold ~ (jest '')))
    %key           ;~(pose (stag ~ hex) (cold ~ (jest '')))
  ==
::
::TODO  need secondary kind of lockup logic, where
::      1) we need to batch-transfer stars to the ceremony
::      2) (not forget to register and) deposit already-active stars
++  lock
  |=  [nonce=@ud =network as=address what=(list ship) to=address =lockup]
  ^-  (list transaction)
  ~&  %assuming-lockup-done-by-ceremony
  ~&  %assuming-ceremony-controls-parents
  =.  what  ::  expand galaxies into stars
    %-  zing
    %+  turn  what
    |=  s=ship
    ^-  (list ship)
    ?.  =(%czar (clan:title s))  [s]~
    (turn (gulf 1 255) |=(k=@ud (cat 3 s k)))
  =/  parents
    =-  ~(tap in -)
    %+  roll  what
    |=  [s=ship ss=(set ship)]
    ?>  =(%king (clan:title s))
    (~(put in ss) (^sein:title s))
  ~|  %invalid-lockup-ships
  ?>  ~|  %does-this-also-work
      ?|  ?=(%linear -.lockup)
          =(`@`(lent what) :(add b1.lockup b2.lockup b3.lockup))
      ==
  =/  contract=address
    ?-  -.lockup
      %linear       0x86cd.9cd0.992f.0423.1751.e376.1de4.5cec.ea5d.1801
      %conditional  0x8c24.1098.c3d3.498f.e126.1421.633f.d579.86d7.4aea
    ==
  =|  txs=(list transaction)
  |^
    ?^  parents
      =.  txs
        %-  do-here
        (set-spawn-proxy:dat i.parents contract)
      $(parents t.parents)
    =.  txs
      %-  do-here
      ?-  -.lockup
        %linear       (register-linear to (lent what) +.lockup)
        %conditional  (register-conditional to +.lockup)
      ==
    |-
    ?~  what  (flop txs)
    =.  txs
      %-  do-here
      (deposit:dat to i.what)
    $(what t.what)
  ::TODO  maybe-do, take dat gat and unit argument
  ++  do-here
    |=  dat=tape
    :_  txs
    (do network (add nonce (lent txs)) contract dat)
  --
::
++  register-linear
  |=  [to=address stars=@ud windup-years=@ud unlock-years=@ud]
  %-  register-linear:dat
  :*  to
      (mul windup-years yer:yo)
      stars
      (div (mul unlock-years yer:yo) stars)
      1
  ==
::
++  register-conditional
  |=  [to=address [b1=@ud b2=@ud b3=@ud] unlock-years-per-batch=@ud]
  %-  register-conditional:dat
  =-  [`address`to b1 b2 b3 `@ud`- 1]
  (div (mul unlock-years-per-batch yer:yo) :(add b1 b2 b3))
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