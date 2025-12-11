::  claz: command line azimuth, for the power-user
::
/+  *claz, *ethereum, *azimuth, verb, default-agent
::
|%
+$  state-0
  $:  %0
      in-progress=(unit command)
  ==
::
+$  rpc-result  [id=@t res=@t]
+$  card  card:agent:gall
::
++  node-url           'http://eth-mainnet.urbit.org:8545'
--
::
=|  state-0
=*  state  -
%+  verb  |
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
      bec   byk.bowl(r da+now.bowl)
  ::
  ++  on-init   on-init:def
  ++  on-save   !>(state)
  ++  on-load   on-load:def
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title [our src]:bowl)
    ?.  ?=(%noun mark)  [~ this]
    ?^  in-progress
      ~&  %still-running-please-try-again-later
      [~ this]
    =/  =command  !<(command vase)
    :_  this(in-progress `command)
    (prepare-for-command:do command)
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=([%prepare *] wire)
      (on-agent:def wire sign)
    ?-  -.sign
        %poke-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
      :_  this(in-progress ~)
      [(leave-spider:do wire our.bowl)]~
    ::
        %watch-ack
      ?~  p.sign
        [~ this]
      =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listen to thread"
      %-  (slog tank u.p.sign)
      [~ this(in-progress ~)]
    ::
        %kick
      [~ this(in-progress ~)]
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %thread-fail
        =+  !<([=term =tang] q.cage.sign)
        %-  (slog leaf+"{(trip dap.bowl)} failed" leaf+<term> tang)
        [~ this(in-progress ~)]
      ::
          %thread-done
        =+  prep=!<(prep-result q.cage.sign)
        ?~  in-progress
          ~&  [dap.bowl 'did preparations, but lost command']
          [~ this]
        :_  this(in-progress ~)
        [(generate:do u.in-progress prep)]~
      ==
    ==
  ::
  ++  on-peek   on-peek:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  bec  byk.bowl(r da+now.bowl)
::
++  poke-spider
  |=  [=path our=@p =cage]
  ^-  card
  [%pass path %agent [our %spider] %poke cage]
::
++  watch-spider
  |=  [=path our=@p =sub=path]
  ^-  card
  [%pass path %agent [our %spider] %watch sub-path]
::
++  leave-spider
  |=  [=path our=@p]
  ^-  card
  [%pass path %agent [our %spider] %leave ~]
::
++  prepare-for-command
  |=  =command
  ^-  (list card)
  =/  new-tid=@ta
    :((cury cat 3) dap.bowl '--' (scot %uv eny.bowl))
  =/  args
    [~ `new-tid bec %claz-prep-command !>([node-url command])]
  :~  (watch-spider /prepare our.bowl /thread-result/[new-tid])
      (poke-spider /prepare our.bowl %spider-start !>(args))
  ==
::
::  transaction generation logic
::
++  generate
  |=  [=command prep=prep-result]
  ^-  card
  ?>  ?=(%nonce -.prep)
  ?-  -.command
      %generate
    %+  write-file-transactions
      path.command
    (batch-to-transactions nonce.prep [network as batch]:command)
  ==
::
++  batch-to-transactions
  |=  [nonce=@ud =network as=address =batch]
  ^-  (list transaction:rpc)
  ?-  -.batch
    %single     [(single nonce network as +.batch) ~]
    %deed       (deed nonce network as +.batch)
    %invites    (invites nonce network as +.batch)
    %lock-prep  (lock-prep nonce network as +.batch)
    %lock       (lock nonce network as +.batch)
    ::
      %more
    =|  txs=(list transaction:rpc)
    =*  batches  batches.batch
    |-
    ?~  batches  txs
    =/  new-txs=(list transaction:rpc)
      ^$(batch i.batches)
    %_  $
      txs      (weld txs new-txs)
      nonce    (add nonce (lent new-txs))
      batches  t.batches
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
  |=  [=path tox=(list transaction:rpc)]
  ^-  card
  ?>  ?=([@ desk @ *] path)
  =-  [%pass [%write path] %arvo %c %info -]
  :-  `desk`i.t.path
  =-  &+[t.t.t.path -]~
  =/  y  .^(arch %cy path)
  ?~  fil.y
    ins+eth-txs+!>(tox)
  mut+eth-txs+!>(tox)
::
++  do
  ::TODO  maybe reconsider encode-call interface, if we end up wanting @ux
  ::      as or more often than we want tapes
  |=  [=network nonce=@ud to=address dat=$@(@ux tape)]
  ^-  transaction:rpc
  :*  nonce
      8.000.000.000  ::TODO  global config
      600.000  ::TODO  global config
      to
      0
      `@`?@(dat dat (tape-to-ux dat))
    ::
      ?-  network
        %mainnet    0x1
        %goerli     0x5
        %fakenet    `@ux``@`1.337
        [%other *]  id.network
      ==
  ==
::
++  single
  |=  [nonce=@ud =network as=address =call]
  ^-  transaction:rpc
  =-  (do network nonce contract data)
  ^-  [data=tape contract=address]
  :-  (encode-claz-call call)
  =/  contracts  (get-contracts network)
  ?+  -.call  ecliptic:contracts
    %send-point  delegated-sending:contracts
  ::
      ?(%approve-batch-transfer %transfer-batch %withdraw)
    linear-star-release:contracts
  ==
::
++  deed
  |=  [nonce=@ud =network as=address deeds-json=cord]
  ^-  (list transaction:rpc)
  =/  deeds=(list [=ship rights])
    (parse-registration deeds-json)
  ::TODO  split per spawn proxy
  =|  txs=(list transaction:rpc)
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
    (do network (add nonce (lent txs)) ecliptic:(get-contracts network) dat)
  --
::
++  invites
  |=  [nonce=@ud =network as=address as-who=ship file=path]
  ^-  (list transaction:rpc)
  =/  friends=(list [=ship @q =address])
    (read-invites file)
  =|  txs=(list transaction:rpc)
  |-
  ?~  friends  (flop txs)
  =*  friend  i.friends
  =;  tx=transaction:rpc
    $(txs [tx txs], friends t.friends)
  %-  do
  :*  network
      (add nonce (lent txs))
      delegated-sending:(get-contracts network)
      (send-point:dat as-who [ship address]:friend)
  ==
::
++  parse-registration
  |=  reg=cord
  ^-  (list [=ship rights])
  ~|  %registration-json-insane
  =+  jon=(need (de:json:html reg))
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
++  lock-prep
  |=  [nonce=@ud =network as=address what=(list ship)]
  ^-  (list transaction:rpc)
  ~&  %assuming-lockup-on-mainnet
  =|  txs=(list transaction:rpc)
  |^
    ?~  what  (flop txs)
    =.  txs
      %-  do-here
      (spawn:dat i.what as)
    =.  txs
      %-  do-here
      %+  transfer-ship:dat  i.what
      ~&  %assuming-lockup-done-by-ceremony
      0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964
    $(what t.what)
  ++  do-here
    |=  dat=tape
    :_  txs
    (do network (add nonce (lent txs)) ecliptic:mainnet-contracts dat)
  --
::
::TODO  support distinguishing/switching between usable lockup methods
::      automagically
++  lock
  |=  $:  nonce=@ud
          =network
          as=address
          how=?(%spawn %transfer)
          what=(list ship)
          to=address
          =lockup
      ==
  ^-  (list transaction:rpc)
  ::  verify lockup sanity
  ::
  ~|  %invalid-lockup-ships
  ?>  ?|  ?=(%linear -.lockup)
          =(`@`(lent what) :(add b1.lockup b2.lockup b3.lockup))
      ==
  ::  expand galaxies into stars
  ::
  =.  what
    %-  zing
    %+  turn  what
    |=  s=ship
    ^-  (list ship)
    ?.  =(%czar (clan:title s))  [s]~
    (turn (gulf 1 255) |=(k=@ud (cat 3 s k)))
  =/  lockup-contract=address
    ?-  -.lockup
      %linear       0x86cd.9cd0.992f.0423.1751.e376.1de4.5cec.ea5d.1801
      %conditional  0x8c24.1098.c3d3.498f.e126.1421.633f.d579.86d7.4aea
    ==
  %-  flop
  =|  txs=(list transaction:rpc)
  ^+  txs
  |^
    ::  registration
    ::
    =.  txs
      %+  do-here  lockup-contract
      ?-  -.lockup
        %linear       (register-linear to (lent what) +.lockup)
        %conditional  (register-conditional to +.lockup)
      ==
    ::  context-dependent setup
    ::
    =.  txs
      ?-  how
        ::  %spawn: set spawn proxy of parents
        ::
          %spawn
        ~&  %assuming-ceremony-controls-parents
        =/  parents
          =-  ~(tap in -)
          %+  roll  what
          |=  [s=ship ss=(set ship)]
          ?>  =(%king (clan:title s))
          (~(put in ss) (^sein:title s))
        |-
        ?~  parents  !! ::txs
        =.  txs
          %+  do-here  ecliptic:mainnet-contracts
          (set-spawn-proxy:dat i.parents lockup-contract)
        $(parents t.parents)
      ::
        ::  %transfer: set transfer proxy of stars
        ::
          %transfer
        ~&  %assuming-ceremony-controls-stars
        |-
        ?~  what  txs
        =.  txs
          %+  do-here  ecliptic:mainnet-contracts
          (set-transfer-proxy:dat i.what lockup-contract)
        =.  txs
          %+  do-here  lockup-contract
          (deposit:dat to i.what)
        $(what t.what)
      ==
    ::  depositing
    ::
    |-
    ?~  what  txs
    :: =.  txs
    ::   %+  do-here  lockup-contract
    ::   (deposit:dat to i.what)
    $(what t.what)
  ++  do-here
    |=  [contract=address dat=tape]
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
      1
      (div (mul unlock-years yer:yo) stars)
  ==
::
++  register-conditional
  |=  [to=address [b1=@ud b2=@ud b3=@ud] unlock-years-per-batch=@ud]
  %-  register-conditional:dat
  :*  to
      b1  b2  b3
      1
      (div (mul unlock-years-per-batch yer:yo) :(add b1 b2 b3))
  ==
::
--
