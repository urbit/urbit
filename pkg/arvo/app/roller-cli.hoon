::  roller-cli: CLI for L2 Azimuth Rollers
::
::    TODO: commands
::
:: client   |    roller
:: _________|___________
::                                 - CLI command
:: [%track 0x1234.abcd]
::                                  - init subscriptions to the roller
:: watch  --> /point/[0x1234.abcd]  - point updates
:: watch  --> /tx/[0x1234.abcd]     - tx status updates
::
:: ----------------------
::                                 Submit Txs
:: poke   --tx-->                  +take-tx
::
:: ---------------------
::                                 Receive Tx status updates
:: watch  /tx/[0x1234.abcd]
::
::  <--(list roller-tx:dice)--     - init
::  <--[address roller-tx:dice]--  - update
::  <--%kick-sub--                 ?:  ?=(?(%confirmed %failed) tx-status)
:: ---------------------
::                                 Receive Point updates (i.e. nonces)
:: watch  /point/[0x1234.abcd]
::
::  <-(list point:naive)-          - init
::  <-[address point:naive]-       - update
::
/-  *dice
/+  *dice,
    naive,
    lib=naive-transactions,
    *fake-roller,
    shoe,
    verb,
    dbug,
    default-agent,
    ethereum
|%
+$  app-state
  $:  %0
      :: TODO: keep track of sessions
      ::
      :: sessions=(map sole-id session)
      points=(jug address:ethereum [ship point:naive])
      history=(jug address:ethereum roller-tx)
      unsigned-txs=(jug address:ethereum [keccak tx:naive])
      :: TODO: track pub/prv keys
      ::
      :: keys=(list (pair address:ethereum address:ethereum)
  ==
::
+$  card  card:shoe
::
+$  command
  $%  ::  List all possible L2 tx types
      ::
      [%l2-tx ~]  :: ?
      ::  Loads a new address (login?)
      ::    — should require signing?
      ::    - it subscribes to the Roller, for updates to it
      ::    - innitially receives a list of points (if any) it controls
      ::
      :: [%track pubkey=address:ethereum prvkey=address:ethereum]
      [%track address:ethereum]
      ::  Table of all submitted txs, by address
      ::
      [%history address:ethereum]
      ::  Table of all unsigned txs, by address
      ::
      [%show-unsigned ~]
      ::  Signs and Submit an unsigned txs (signed)
      ::
      [%submit address:ethereum tx:naive]
      ::  Cancels a submitted (but pending) txs
      ::
      [%cancel ~]
      ::  Ships owned by an address
      ::
      [%ships address:ethereum]
      ::  Point data for a given ship
      ::
      [%point address:ethereum ship]
      ::  Example flow
      ::
      [%example-flow ~]
  ==
--
=|  app-state
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe command)
^-  (shoe:shoe command)
:: =>  |%
::     ++  get-address-points
::       |=  [roller=@p =address:ethereum]
::       :*  %pass
::           /roller-points
::           %agent
::           [roller %azimuth]
::           %watch
::           /address/[address]
::       ==
::     --
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    des   ~(. (default:shoe this command) bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(app-state old))]
  :: |^
  :: =+  !<(old-state=app-states old)
  :: |-
  :: ?-  -.old-state
  ::   %0  $(old-state [%1 ~ ~ ~])
  ::   %1  $(old-state [%2 ~ ~ ~])
  ::   %2  [~ this(state old-state)]
  :: ==
  :: ++  app-states   $%([%0 ~] [%1 *] app-state)
  :: --
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ~&
    =/  addr=@ux
      0x6def.fb0c.afdb.11d1.75f1.23f6.891a.a64f.01c2.4f7d
    :: %+  turn
      ~(tap in (~(get ju points) addr))
    :: head
  [~ this]
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
    ?~  addr=(slaw %ux wat)  ~
    %+  turn
      ~(tap in (~(get ju points) u.addr))
    head
  --
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  wire  (on-agent:def wire sign)
    [%points @ ~]  (get-points i.t.wire sign)
    [%txs @ ~]     (get-txs i.t.wire sign)
  ==
  ::
  ++  get-points
    |=  [wat=@t =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %points
        ?~  addr=(slaw %ux wat)  (on-agent:def wire sign)
        =+  !<(points=(list [ship point:naive]) q.cage.sign)
        =.  points.state
          %-  ~(gas ju points.state)
          (turn points (cork same (lead u.addr)))
        [~ this]
      ::
          %point
        ?~  addr=(slaw %ux wat)  (on-agent:def wire sign)
        =+  !<(new-point=[=ship =point:naive] q.cage.sign)
        ::  TODO: handle multiple sole sessions?
        ::
        :: =/  sez=(list [=sole-id =session])
        ::   ~(tap by sessions)
        =/  console=tape
          "Point update ({(scow %p ship.new-point)})"
        =.  points.state
          ::  FIXME: doesn't properly update point
          ::  handle proper insert/deletion of points
          ::  to account for ownership changes/nonce updates
          ::
          =;  [is-owner=? old=(unit [=ship =point:naive])]
            ?~  old  points.state
            =.  points.state
              (~(del ju points.state) u.addr u.old)
            ?.  is-owner  points.state
            (~(put ju points.state) [u.addr new-point])
          =/  points=(list [=ship =point:naive])
            ~(tap in (~(get ju points.state) u.addr))
          |-  ^-  [? (unit [ship point:naive])]
          |^
          ?~  points  [| ~]
          ?.  =(ship.i.points ship.new-point)
            $(points t.points)
          :-  is-owner
          `[ship.new-point point.i.points]
          ::
          ++  is-owner
            =*  own  own.point.new-point
            ?|  =(u.addr address.owner.own)
                =(u.addr address.spawn-proxy.own)
                =(u.addr address.management-proxy.own)
                =(u.addr address.voting-proxy.own)
                =(u.addr address.transfer-proxy.own)
            ==
          --
          :: %-  ~(run in points)
          :: |=  old=[=ship =point:naive]
          :: ?.  =(ship.old ship.new)
          ::    old
          :: point.new
          :: (~(put ju points.state) [u.addr point])
        ~&  :-  %ships
            (turn ~(tap in (~(get ju points.state) u.addr)) head)
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
  ++  get-txs
    |=  [wat=@t =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %fact
      ?~  addr=(slaw %ux wat)  (on-agent:def wire sign)
      ?+  p.cage.sign  (on-agent:def wire sign)
          %txs
        =+  !<(txs=(list roller-tx) q.cage.sign)
        =.  history.state
          %-  ~(gas ju history.state)
          (turn txs (cork same (lead u.addr)))
        [~ this]
      ::
          %tx
        ?~  addr=(slaw %ux wat)  (on-agent:def wire sign)
        =+  !<(=roller-tx q.cage.sign)
        =/  hash=tape
          =+  hash=(scow %ux hash.roller-tx)
          =+  len=(lent hash)
          ;:  weld
            (swag [0 6] hash)
            "..."
            (swag [(sub len 4) len] hash)
          ==
        =/  console=tape
          "Tx hash: {hash} -> {(trip status.roller-tx)}"
        =.  history.state  (update-tx u.addr roller-tx)
        :: ~&  console
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
  ++  update-tx
    |=  [=address:ethereum =roller-tx]
    %.  [address roller-tx]
    ?+    status.roller-tx  ~(put ju history.state)
        %pending
      ~(put ju history.state)
    ::
        %sending
      %~  put  ju
      %-  ~(del ju history.state)
      [address roller-tx(status %pending)]
    ::
        %confirmed
      %~  put  ju
      %-  ~(del ju history.state)
      [address roller-tx(status %sending)]
    ::
        %failed
      ::  TODO: make it not ugly
      ::
      %~  put  ju
      %-  %~  del  ju
        %-  ~(del ju history.state)
        [address roller-tx(status %sending)]
      [address roller-tx(status %pending)]
    ==
  --
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  command-parser
  |=  sole-id=@ta
  ^+  |~(nail *(like [? command]))
  ::  wait for 'enter' to run the command
  ::
  |^
  %+  stag  |
  :: (perk %demo %row %table %track %submit %history ~)
  ;~  pose
    ;~(plug (tag %track) ;~(pfix (jest ' 0x') hex))
    ;~(plug (tag %example-flow) (easy ~))
    ;~(plug (tag %history) ;~(pfix (jest ' 0x') hex))
    ;~((glue ace) (tag %submit) submit)
    ;~((glue ace) (tag %ships) address)
    ;~((glue ace) (tag %point) address ;~(pfix sig fed:ag))
  ==
  ::
  ++  tag      |*(a=@tas (cold a (jest a)))  :: TODO (from /app/chat-cli) into stdlib
  ++  address  ;~(pfix (jest '0x') hex)
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
  |=  [sole-id=@ta =command]
  ^-  (quip card _this)
  |^
  ?+  -.command  !!
      %track    (track +.command)
      %submit   (submit +.command)
      %history  (history +.command)
      %ships    (ships +.command)
      %point    (point +.command)
      %example-flow  example-flow
  ==
  ::
  ++  example-flow
    ^-  (quip card _this)
    =/  address=@t  '0x6deffb0cafdb11d175f123f6891aa64f01c24f7d '
    =/  spawn=@t    '0xf48062ae8bafd6ef19cd6cb89db93a0d0ca6ce26'
    =/  track=@t    'track 0x6deffb0cafdb11d175f123f6891aa64f01c24f7d'
    =/  ships=@t    'ships 0x6deffb0cafdb11d175f123f6891aa64f01c24f7d'
    =/  tx1=@t
      %-  crip
      :~  'submit '
           address
          '[~wanzod own] '
          'set-spawn-proxy '
          address
      ==
    =/  tx2=@t
      %-  crip
      :~  'submit '
           address
          '[~wanzod own] '
          'spawn '
          '~modlep-fosreg '
          spawn
      ==
    =/  failed-tx=@t
      %-  crip
      :~  'submit '
          '0x6deffb0cafdb11d175f123f6891aa64f01c24f7d '
          '[~wanzod own] '
          'spawn '
          '~marzod '
          '0xf'
      ==
    =/  example-a=@t  '- lists ships controlled by the given address  :: '
    =/  example-b=@t  '- receives updates signed by the given address :: '
    =/  example-c=@t  '- this tx will fail :: '
    :_  this
    :_  ~
    ^-  card
    :-  %shoe
    ^-  [(list _sole-id) shoe-effect:shoe]
    :-  [sole-id]~
    ^-  shoe-effect:shoe
    :-  %sole
    ?.  =(src our):bowl
      [%txt "1234"]
    :-  %mor
    :~  [%klr ~[[[~ ~ `%g] [example-a]~] [``~ [ships]~]]]
        [%klr ~[[[~ ~ `%b] [example-b]~] [``~ [track]~]]]
        [%klr ~[[[~ ~ `%r] [example-c]~] [``~ [failed-tx]~]]]
    ==
  ::
  ++  submit
    |=  [=address:ethereum =tx:naive]
    ^-  (quip card _this)
    =/  owner=(unit [=nonce:naive =point:naive])
      =/  points=(list [=ship =point:naive])
        ~(tap in (~(get ju points) address))
      |-  ^-  (unit [nonce:naive point:naive])
      ?~  points  ~
      ?.  =(ship.from.tx ship.i.points)
        $(points t.points)
      `(get-owner point.i.points proxy.from.tx)
      :: =<  `[nonce point.i.points]
      :: (proxy-from-point:naive proxy.from.tx point.i.points)
    ?~  owner  ~&  "empty points"  [~ this]
    =/  =keccak
      %-  hash-tx:lib
      (unsigned-tx:lib 1.337 nonce.u.owner (gen-tx-octs:lib tx))
    =/  sig=octs  (fake-sig tx address nonce.u.owner)
    =.  points
      %+  ~(put ju points)  address
      [ship.from.tx point.u.owner]
    :_  this
    :_  ~
    :*  %pass
        /pokepath
        %agent
        [our.bowl %roller]
        %poke
        roller-action+!>([%submit | address q.sig %don tx])
    ==
  ::
  ++  track
    |=  =address:ethereum
    ^-  (quip card _this)
    =/  [to=(list _sole-id) fec=shoe-effect:shoe]
      :-  [sole-id]~
      :-  %sole
      =/  =tape  "Listening to updates for {(scow %ux address)}"
      ?.  =(src our):bowl
        [%txt tape]
      [%klr [[`%br ~ `%g] [(crip tape)]~]~]
    :: :_  this(keys (snoc keys address))
    :_  this
    :~  [%shoe to fec]
        :^  %pass  /points/[(scot %ux address)]  %agent
        [[our.bowl %roller] %watch /points/[(scot %ux address)]]
      ::
        :^  %pass  /txs/[(scot %ux address)]  %agent
        [[our.bowl %roller] %watch /txs/[(scot %ux address)]]
    ==
  ::
  ++  history
    |=  =address:ethereum
    ^-  (quip card _this)
    :_  this
    =;  [to=(list _sole-id) fec=shoe-effect:shoe]
      [%shoe to fec]~
    :-  [sole-id]~
    :^  %table
        :: ~[t+'address' t+'signing ship' t+'type' t+'status' t+'hash']
        ~[t+'signing ship' t+'type' t+'status' t+'hash' t+'time']
      ~[14 20 9 13 26]
    %+  turn
      %+  sort  ~(tap in (~(get ju history.state) address))
      |=([a=roller-tx b=roller-tx] (lth time.a time.b))
    |=  roller-tx
    |^  ~[p+ship t+type t+status pack-hash t+(scot %da time)]
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
      :-  %t
      %-  crip
      ;:  weld
        (swag [0 6] hash)
        "..."
       (swag [(sub len 4) len] hash)
      ==
    --
  ::
  ++  ships
    |=  =address:ethereum
    ^-  (quip card _this)
    ~&  ships+(turn ~(tap in (~(get ju points) address)) head)
    [~ this]
  ::
  ++  point
    |=  [=address:ethereum =ship]
    ^-  (quip card _this)
    =/  points=(set [@p point:naive])
      (~(get ju points.state) address)
    ~&  %+  skim  ~(tap in points)
        |=([s=@p =point:naive] =(s ship))
    [~ this]
  --
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
