::  roller-cli: CLI for L2 Azimuth Rollers
::
::  The CLI subscribes, upon the user's request, to a roller (local or remote)
::  sending an address that the CLI user controls, and receving the naive state
::  associated to the points controlled by that address; the history of the
::  L2 txs for it, if any; an index of the points (per proxy) controlled by the
::  address; and sponsor/owner indices, if the address controls any points.
::
::  Upon sending a command, the roller will register the L2 tx, add it to its
::  pending queue if valid, and notifies us of any change in its state, but also
::  of any of the cnanges that our tx has in the naive state, so we can update
::  the information for the points we control.
::
::  These updates come in the form on point:update and tx:update. The first is
::  used to update the naive state an the ownership and sponsorhips indices, and
::  the second for the state of the transactions we submit.
::
/-  *dice
/+  dice,
    naive,
    lib=naive-transactions,
    shoe,
    verb,
    dbug,
    default-agent,
    ethereum
|%
+$  app-state
  $:  %0
      ::TODO
      ::chain-id=@
      addresses=(map @ux [roller=ship pk=@])
      :: TODO: keep track of sessions?
      ::
      :: sessions=(map sole-id session)
      =points:naive
      =owners
      =sponsors
      =history
  ==
::
+$  card         card:shoe
--
::  Utils
::
=>  |%
    ++  sign-tx
      |=  [chain-id=@ pk=@ =nonce:naive =tx:naive]
      ^-  octs
      =/  sign-data=@uvI
        %-  hash-tx:lib
        (unsigned-tx:lib chain-id nonce (gen-tx-octs:lib tx))
      =;  [v=@ r=@ s=@]
        (cad:naive 3 1^v 32^s 32^r ~)
      (ecdsa-raw-sign:secp256k1:secp:crypto sign-data pk)
    --
::  Cards
::
=>  |%
    ++  to-shoe
      |=  [to=(list @ta) =shoe-effect:shoe]
      shoe+to^shoe-effect
    ::
    ++  track-address
      |=  [roller=ship =address:ethereum]
      ^-  card
      =/  =wire  /connect/[(scot %ux address)]
      [%pass wire %agent [roller %roller] %watch wire]
    ::
    ++  track-transactions
      |=  [roller=ship =address:ethereum]
      ^-  card
      =/  =wire  /txs/[(scot %ux address)]
      [%pass wire %agent [roller %roller] %watch wire]
    ::
    ++  track-points
      |=  [roller=ship =address:ethereum]
      ^-  card
      =/  =wire  /points/[(scot %ux address)]
      [%pass wire %agent [roller %roller] %watch wire]
    ::
    ++  submit-tx
      |=  [roller=ship =address:ethereum sig=@ =tx:naive]
      ^-  card
      :*  %pass
          /submit
          %agent
          [roller %roller]
          %poke
          roller-action+!>([%submit | address sig %don tx])
      ==
    --
::
=|  app-state
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe command-cli)
^-  (shoe:shoe command-cli)
::  Main
::
=<
  |_  =bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bowl)
      des   ~(. (default:shoe this command-cli) bowl)
      do    ~(. +> bowl)
  ::
  ++  on-init   on-init:def
  ++  on-save   !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    `this(state !<(app-state old))
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+    mark  (on-poke:def mark vase)
        %cli-command
      =+  !<(poke=command-cli vase)
      (on-command *@ta poke)
    ==
  ::
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ::  +on-peek: scry paths
  ::
  ::    /x/ships/[0x1234.abcd]  ->  %noun  (list ship)
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    |^
    ?+  path  ~
      [%x %ships @ ~]  (ships i.t.t.path)
    ==
    ::
    ++  ships
      |=  wat=@t
      :+  ~  ~
      :-  %noun
      !>  ^-  (list ship)
      ?~  address=(slaw %ux wat)  ~
      =/  proxies=(list proxy:naive)
        ~[%own %spawn %manage %vote %transfer]
      %+  roll  proxies
      |=  [=proxy:naive ships=(list ship)]
      %+  welp
        ~(tap in (~(get ju owners) [proxy u.address]))
      ships
    --
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    |^
    ?+  wire  (on-agent:def wire sign)
      [%connect @ ~]  (init-data i.t.wire sign)
      [%txs @ ~]      (transactions i.t.wire sign)
      [%points @ ~]   (points i.t.wire sign)
    ==
    ::
    ++  init-data
      |=  [wat=@t =sign:agent:gall]
      ^-  (quip card _this)
      ?+  -.sign  (on-agent:def wire sign)
          %fact
        ?+  p.cage.sign  (on-agent:def wire sign)
            %roller-data
          ?~  addr=(slaw %ux wat)  (on-agent:def wire sign)
          =+  !<(=roller-data q.cage.sign)
          =,  roller-data
          =:  ::chain-id.state  chain-id
              points.state    points
              owners.state    owners
              sponsors.state  sponsors
              history.state   (~(put by history.state) u.addr history)
            ==
          [~ this]
        ==
      ==
    ::
    ++  transactions
      |=  [wat=@t =sign:agent:gall]
      ^-  (quip card _this)
      ?+  -.sign  (on-agent:def wire sign)
          %fact
        ?~  addr=(slaw %ux wat)  (on-agent:def wire sign)
        ?+  p.cage.sign  (on-agent:def wire sign)
            %tx
          ?~  addr=(slaw %ux wat)  (on-agent:def wire sign)
          =+  !<(=update q.cage.sign)
          ?>  ?=(%tx -.update)
          =/  hash=tape
            =/  keccak=tape
              %+  scow  %ux
              (hash-tx:lib raw.raw-tx.pend-tx.update)
            =+  len=(lent keccak)
            ;:  weld
              (swag [0 6] keccak)
              "..."
              (swag [(sub len 4) len] keccak)
            ==
          =^  *  history.state
           (update-history:dice history.state [[pend-tx]~ status]:update)
          =/  console=tape
            "Tx hash: {hash} -> {(trip status.update)}"
          :_  this
          :_  ~
          :-  %shoe
          :-  ~
          :-  %sole
          ?.  =(src our):bowl
            [%txt console]
          [%klr [[`%br ~ `%g] [(crip console)]~]~]
        ==
      ==
    ::
    ++  points
      |=  [wat=@t =sign:agent:gall]
      ^-  (quip card _this)
      ?+  -.sign  (on-agent:def wire sign)
          %fact
        ?+  p.cage.sign  (on-agent:def wire sign)
            %point
          ?~  addr=(slaw %ux wat)  (on-agent:def wire sign)
          =+  !<(=update q.cage.sign)
          ?>  ?=(%point -.update)
          =,  update
          =^  *  sponsors.state
            (sponsorship:dice diff ship new old sponsors.state)
          =^  *  owners.state
            (ownership:dice diff ship new old owners.state)
          =.  points.state  (put:orp:dice points.state ship new)
          =/  console=tape
            %+  weld
              "Point update: {(scow %p ship)} -> "
            "{(trip proxy.to)} - {(scow %ux address.to)}"
          :_  this
          :_  ~
          :-  %shoe
          :-  ~
          :-  %sole
          ?.  =(src our):bowl
            [%txt console]
          [%klr [[`%br ~ `%g] [(crip console)]~]~]
        ==
      ==
    --
  ::
  ++  on-arvo         on-arvo:def
  ++  on-fail         on-fail:def
  ++  command-parser  build-parser:do
  ::
  ++  tab-list
    |=  sole-id=@ta
    ^-  (list [@t tank])
    :~  ['txs' leaf+"list available L2 transaction"]
        ['submit' leaf+"sends| a L2 transaction to the Roller"]
        ['cancel' leaf+"cancels a (pending) L2 transaction"]
        ['history' leaf+"shows all current submitted transactions"]
        ['track' leaf+"loads an ethereum address and tracks points and L2 txs"]
    ==
  ::
  ++  on-command
    |=  [sole-id=@ta command=command-cli]
    ^-  (quip card _this)
    =^  cards  state
      ?+  -.command  !!
          %connect  (~(connect commands:do sole-id) +.command)
          %submit   (~(submit commands:do sole-id) +.command)
          %history  (~(history commands:do sole-id) +.command)
          %point    (~(point commands:do sole-id) +.command)
          %ships    (~(ships commands:do sole-id) +.command)
      ==
    [cards this]
  ::
  ++  can-connect
    |=  sole-id=@ta
    ^-  ?
    ?|  =(~zod src.bowl)
        (team:title [our src]:bowl)
    ==
  ::
  ++  on-connect      on-connect:des
  ++  on-disconnect   on-disconnect:des
  --
::
|_  =bowl:gall
++  commands
  |_  sole-id=@ta
  ::
  ++  connect
    |=  [roller=ship =address:ethereum pk=@]
    ^-  (quip card _state)
    =/  [to=(list _sole-id) fec=shoe-effect:shoe]
      :-  [sole-id]~
      :-  %sole
      =/  =tape
        "Listening to updates for {(scow %ux address)} from {(scow %p roller)}"
      ?.  =(src our):bowl
        [%txt tape]
      [%klr [[`%br ~ `%g] [(crip tape)]~]~]
    ?~  pk=(de:base16:mimes:html pk)
      ~&  >>>  "private key has incorrect format"
      `state
    =.  addresses
      %+  ~(put by addresses)  address
      [roller q.u.pk]
    :_  state
    =;  cards=(list card)
      ?:  =(*@ta sole-id)  cards
      [(to-shoe to fec) cards]
    :~  (track-address roller address)
        (track-points roller address)
        (track-transactions roller address)
    ==
  ::
  ++  ships
    |=  =address:ethereum
    ^-  (quip card _state)
    ::  TODO: console formatting
    ::
    ~&  (controlled-ships:dice address owners.state)
    [~ state]
  ::
  ++  point
    |=  =ship
    ^-  (quip card _state)
    ::  TODO: console formatting
    ::
    ~&  (get:orp:dice points.state ship)
    [~ state]
  ::
  ++  history
    |=  =address:ethereum
    ^-  (quip card _state)
    :_  state
    =;  [to=(list _sole-id) fec=shoe-effect:shoe]
      [%shoe to fec]~
    :-  [sole-id]~
    :^  %table
        ~[t+'signing ship' t+'type' t+'status' t+'hash' t+'time']
      ~[14 20 9 13 26]
    ?~  history=(~(get by history.state) address)
      ~
    %+  turn  (tap:orh:dice u.history)
    |=  [=time roll-tx]
    |^  ~[p+ship t+type t+status t+pack-hash t+pack-time]
    ::
    ++  pack-time
      %-  crip
      (scag 21 (scow %da time))
    ::
    ++  pack-address
      =+  addr=(scow %ux address)
      =+  len=(lent addr)
      :-  %t
      %-  crip
      ;:  weld
        (swag [0 6] addr)
        "..."
        (swag [(sub len 4) len] addr)
      ==
    ::
    ++  pack-hash
      =+  hash=(scow %ux hash)
      =+  len=(lent hash)
      %-  crip
      ;:  weld
        (swag [0 6] hash)
        "..."
      (swag [(sub len 4) len] hash)
      ==
    --
  ::
  ++  submit
    |=  [=address:ethereum =tx:naive]
    ^-  (quip card _state)
    ::  TODO: to state
    ::
    =/  chain-id=@  1.337
    ?~  roller=(~(get by addresses) address)
      ~&  >>>  'can\'t find address'
      `state
    =/  [roller=ship pk=@]  u.roller
    ?~  owned=(~(get ju owners) [proxy.from.tx address])
      ~&  >>>  'address doesn\'t control any points'
      `state
    ?.  (~(has in ^-((set @p) owned)) ship.from.tx)
      ~&  >>>  'can\'t find point'
      `state
    =/  =nonce:naive
      (get-nonce:dice (got:orp:dice points ship.from.tx) proxy.from.tx)
    =/  =keccak
      %-  hash-tx:lib
      (unsigned-tx:lib chain-id nonce (gen-tx-octs:lib tx))
    =/  sig=octs  (sign-tx chain-id pk nonce tx)
    :_  state
    [(submit-tx roller address q.sig tx)]~
  --
::
++  build-parser
  |=  sole-id=@ta
  ^+  |~(nail *(like [? command-cli]))
  ::  wait for 'enter' to run the command
  ::
  |^
  %+  stag  |
  ;~  pose
    ;~((glue ace) (tag %connect) connect)
    ;~(plug (tag %point) ;~(pfix sig fed:ag))
    ;~(plug (tag %history) ;~(pfix (jest ' 0x') hex))
    ;~((glue ace) (tag %submit) submit)
    ;~((glue ace) (tag %ships) address)
  ==
  ::
  :: TODO (from /app/chat-cli) into stdlib ?
  ++  tag      |*(a=@tas (cold a (jest a)))
  ++  roller   ;~(pfix sig fed:ag)
  ++  address  ;~(pfix (jest '0x') hex)
  ++  connect
    (cook ,[ship address:ethereum @] ;~((glue ace) roller address hex))
  ::
  ++  sponsorship
    %-  perk
    :~  %escape
        %cancel-escape
        %adopt
        %reject
        %detach
    ==
  ::
  ++  ownership
    %-  perk
    :~  %set-management-proxy
        %set-spawn-proxy
        %set-transfer-proxy
    ==
  ::
  ++  proxies
    (perk %own %spawn %manage %vote %transfer ~)
  ::
  ++  submit
    %+  cook  ,[address:naive tx:naive]
    ;~  (glue ace)
      address
      ::  from=[ship proxy:naive]
      ::
      %+  cook  ,[ship proxy:naive]
      %+  ifix  [sel ser]
      ;~((glue ace) ;~(pfix sig fed:ag) proxies)
      ::  skim-tx:naive
      ::
      %+  cook  ,skim-tx:naive
      ;~  pose
        ::  [%transfer-point =address reset=?]
        ::
        ;~  (glue ace)
          (perk %transfer-point ~)
          address
          ;~(pose (cold & (just 'y')) (cold | (just 'n')))
        ==
        ::  [%spawn ship address:naive]
        ::
        ;~  (glue ace)
          (perk %spawn ~)
          ;~(pfix sig fed:ag)
          address
        ==
        :: [%configure-keys encrypt=@ auth=@ crypto-suite=@ breach=?]
        ::
        ;~  (glue ace)
          (perk %configure-keys ~)
          address
          address
          dem
          ;~(pose (cold & (just 'y')) (cold | (just 'n')))
        ==
        :: [?([%escape %cancel-escape %adopt %reject %detach]) ship]
        ::
        ;~((glue ace) sponsorship ;~(pfix sig fed:ag))
        :: $:  ?([%set-management-proxy %set-spawn-proxy %set-transfer-proxy])
        ::     address
        :: ==
        ::
        ;~((glue ace) ownership address)
      ==
    ==
  --
--
