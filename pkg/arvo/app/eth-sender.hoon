::  eth-sender: utility for signing & sending eth-txs
::
::    usage:
::
::    sign txs for gasses of 2 and 11 gwei; (~ for default gwei set)
::    store at path
::      :eth-sender [%sign %/txs %/txs/eth-txs %/pk/txt ~[2 0]]
::
::    read nonce range from signed transactions at path
::      :eth-sender [%read %txs/txt]
::
::    send all but first 50 txs from path to mainnet,
::    waiting for confirms every 4 txs
::      :eth-sender [%send %/txs/txt 4 `index+50 ~]
::
/+  ethereum, default-agent, verb
::
|%
++  state-0
  $:  %0
      ~
  ==
::
+$  tx-range
  $:  how=?(%nonce %index)                   ::  tx nonce / index in file
      wat=$@(@ud [start=@ud end=@ud])        ::  inclusive. end optional
  ==
::
+$  command
  $%  [%sign out=path in=path key=path gweis=(list @ud)]
      [%read =path]
    ::
      $:  %send
          txs=path
          step-size=@ud
          range=(unit tx-range)
          nodes=(list [id=@tas url=@t])
      ==
  ==
::
+$  card  card:agent:gall
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
    =/  =command  !<(command vase)
    ?-  -.command
        %read
      ~&  (read-transactions:do +.command)
      [~ this]
    ::
        %sign
      :_  this
      (sign-transactions:do +.command)
    ::
        %send
      :_  this
      (send-transactions:do +.command)
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=([%send *] wire)
      (on-agent:def wire sign)
    ?-  -.sign
        %poke-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
      :_  this
      [(leave-spider:do wire our.bowl)]~
    ::
        %watch-ack
      ?~  p.sign
        [~ this]
      =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listen to thread"
      %-  (slog tank u.p.sign)
      [~ this]
    ::
        %kick
      [~ this]
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %thread-fail
        =+  !<([=term =tang] q.cage.sign)
        %-  (slog leaf+"{(trip dap.bowl)} failed" leaf+<term> tang)
        [~ this]
      ::
          %thread-done
        ~&  ['all submitted to' t.wire]
        [~ this]
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
++  start-txs-send
  |=  [[node-id=@tas node=@t] step=@ud txs=(list @ux)]
  ^-  (list card)
  =/  tid=@ta
    :((cury cat 3) dap.bowl '--' node-id '--' (scot %uv eny.bowl))
  =/  args  [~ `tid bec %eth-send-txs !>([node step txs])]
  :~  (watch-spider /send/[tid] our.bowl /thread-result/[tid])
      (poke-spider /send/[tid] our.bowl %spider-start !>(args))
  ==
::
::
++  get-file
  |=  =path
  ~|  path
  .^  (list cord)
    %cx
    (scot %p our.bowl)
    %base
    (scot %da now.bowl)
    path
  ==
::
++  write-file-wain
  |=  [=path tox=(list cord)]
  ^-  card
  ?>  ?=([@ desk @ *] path)
  =-  [%pass [%write path] %arvo %c %info -]
  :-  i.t.path
  =-  &+[t.t.t.path -]~
  =/  y  .^(arch %cy path)
  ?~  fil.y
    ins+txt+!>(tox)
  mut+txt+!>(tox)
::
++  write-file-transactions
  |=  [=path tox=(list transaction:rpc:ethereum)]
  ^-  card
  ?>  ?=([@ desk @ *] path)
  =-  [%pass [%write path] %arvo %c %info -]
  :-  i.t.path
  =-  &+[t.t.t.path -]~
  =/  y  .^(arch %cy path)
  ?~  fil.y
    ins+eth-txs+!>(tox)
  mut+eth-txs+!>(tox)
::
::
++  read-transactions
  |=  =path
  ^-  tape
  =+  tox=.^((list cord) %cx path)
  =+  [first last]=(read-nonces tox)
  %+  weld
    "Found nonces {(scow %ud first)} through {(scow %ud last)}"
  " in {(scow %ud (lent tox))} transactions."
::
++  read-nonces
  |=  tox=(list cord)
  |^  ^-  [@ud @ud]
      ?:  =(~ tox)  ::NOTE  tmi
        [0 0]
      :-  (read-nonce (snag 0 tox))
      (read-nonce (snag (dec (lent tox)) tox))
  ::
  ++  read-nonce
    |=  tex=cord
    ^-  @ud
    %+  snag  0
    %-  decode-atoms:rlp:ethereum
    (tape-to-ux (trip tex))
  --
::
::
++  sign-transactions
  |=  [out=path in=path key=path gasses=(list @ud)]
  ^-  (list card)
  %+  turn
    ?.  =(~ gasses)  gasses
    ::  default gwei set
    ~[4 8 12 20 32]
  |=  gas=@ud
  %+  write-file-wain
    ::  add gas amount to path
    =+  end=(dec (lent out))
    =-  (weld (scag end out) -)
    ?:  =(0 gas)  [(snag end out) /txt]
    :_  /txt
    (cat 3 (snag end out) (crip '-' ((d-co:co 1) gas)))
  ::
  %-  sign
  :+  in  key
  ::  modify tx gas if non-zero gwei specified
  ?:  =(0 gas)  ~
  `(mul gas 1.000.000.000)
::
++  sign
  =,  rpc:ethereum
  |=  [in=path key=path gas=(unit @ud)]
  ^-  (list cord)
  ?>  ?=([@ @ @ *] key)
  =/  pkf  (get-file t.t.t.key)
  ?>  ?=(^ pkf)
  =/  pk  (rash i.pkf ;~(pfix (jest '0x') hex))
  =/  txs  .^((list transaction) %cx in)
  =/  enumerated
    =/  n  1
    |-  ^-  (list [@ud transaction])
    ?~  txs
      ~
    [[n i.txs] $(n +(n), txs t.txs)]
  %+  turn  enumerated
  |=  [n=@ud tx=transaction]
  ~?  =(0 (mod n 100))  [%signing n]
  =?  gas-price.tx  ?=(^ gas)  u.gas
  (crip '0' 'x' ((x-co:co 0) (sign-transaction:key:ethereum tx pk)))
::
++  send-transactions
  |=  [=path step=@ud range=(unit tx-range) nodes=(list [id=@tas url=@t])]
  ^-  (list card)
  =?  nodes  =(~ nodes)
    :~  geth+'http://eth-mainnet.urbit.org:8545'
    ==
  ~&  'loading txs...'
  =/  tox=(list cord)  .^((list cord) %cx path)
  =?  tox  ?=(^ range)
    (txs-in-range tox u.range)
  =/  txs=(list @ux)
    %+  turn  tox
    (cork trip tape-to-ux)
  ~&  ['sending txs:' (lent txs)]
  %-  zing
  %+  turn  nodes
  |=  node=[@tas @t]
  (start-txs-send node step txs)
::
++  txs-in-range
  |=  [tox=(list cord) =tx-range]
  ^+  tox
  =*  ran  wat.tx-range
  ?-  how.tx-range
      %index
    ?@  ran
      (slag ran tox)
    %+  slag  start.ran
    (scag end.ran tox)
  ::
      %nonce
    =+  [first last]=(read-nonces tox)
    ?:  !=((lent tox) +((sub last first)))
      ~|  ['probably non-contiguous set of transactions' -]
      !!
    ?@  ran
      (slag (sub ran first) tox)
    %+  slag  (sub start.ran first)
    (scag (sub +(end.ran) first) tox)
  ==
::
++  tape-to-ux
  |=  t=tape
  (scan t ;~(pfix (jest '0x') hex))
--
