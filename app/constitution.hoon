/-  constitution, ethereum, json-rpc
/+  ethereum
::TODO  =,  ethereum / constitution causes bail fail. find minimal repro
=>  [. constitution ethereum]
=,  eyre
|%
++  state
  $:  ships=registry
      block=@ud
      filter=(unit @ud)
      ships-c=address
  ==
::
++  move  [bone card]                                   ::  [target side-effect]
++  card                                                ::  side-effect
  $%  [%peer wire gill:gall path]
      [%hiss wire (unit user:eyre) mark [%hiss hiss]]
  ==
--
::
|_  {bol=bowl:gall state}
::
++  prep
  |=  old=(unit *)
  :: ?~  old
    init
  :: [~ ..prep(fid u.old)]
::
++  init
  =/  sc=address
    0xa9c7.9b9c.5e4e.1fdc.69c1.
      9fc0.6232.64c8.da50.7a22
  :_  ..init(ships-c sc)
  =-  [ost.bol -]~
  %+  rpc-req  /init
  %-  batch-read-request
  %+  turn  (gulf ~zod ~per) :: ~fes)
  |=  p=@p
  :+  `(scot %p p)  sc
  ['getShipData(uint32)' ~[uint+`@`p]]
::
++  rpc-req
  |=  [w=wire j=json]
  ^-  card
  :^  %hiss  w  ~
  :+  %json-rpc-response  %hiss
  =-  (json-request - j)
  =+  (need (de-purl:html 'http://localhost:8545'))
  -(p.p |)
::
++  sigh-json-rpc-response-init
  |=  [w=wire rep=response:json-rpc]
  ~&  [%res rep]
  ?>  ?=(%batch -.rep)
  =-  ~&  [%ship-data `(list (pair ship hull))`-]
      [~ +>.$(ships (~(gas in ships) -))]
  %+  murn  bas.rep
  ::TODO  ++parse-ship-data into lib
  |=  r=response:json-rpc
  ^-  (unit (pair ship hull))
  ?:  ?=(%error -.r)  ~  ::TODO  retry on error?
  ?>  ?=(%result -.r)
  ?>  ?=(%s -.res.r)
  =/  des=(list data)
    %+  decode-results  p.res.r
    :~  %address  %uint  %uint  %uint  %uint  [%bytes-n 32]
        %uint     %uint  %uint  %bool  %address
    ==
  ::  don't care about latent ships.
  ::?:  =([%uint 0] (snag 1 des))  ~
  :+  ~  (slav %p id.r)
  (hull-from-data des)
::
::TODO  there definitely needs to be a helper function of some kind,
::      but is there a way for the type system to be aware of the return
::      type if we ask for ie ~[%address %uint %bool] data as a noun?
++  hull-from-data
  |=  das=(list data)
  ^-  hull
  ::NOTE  we expect das to contain, on order:
  ::　    address pilot
  ::　    uint8 state
  ::　    uint64 locked
  ::　    uint64 completed
  ::　    uint16 children
  ::　    bytes32 key
  ::　    uint256 revision
  ::　    uint32 sponsor
  ::　    uint32 escape
  ::　    bool escaping
  ::　    address transferrer
  :*  =+  own=(snag 0 das)
      ?>  ?=(%address -.own)
      p.own
    ::
      =+  cic=(snag 4 das)
      ?>  ?=(%uint -.cic)
      p.cic
    ::
      ::TODO  new key model
      =+  puk=(snag 5 das)
      ?>  ?=(%bytes-n -.puk)
      ?>  =(p.p.puk 32)  ::  valid key length
      q.p.puk
    ::
      0  ::TODO  key rev
    ::
      ^-  @p
      =+  sop=(snag 7 das)
      ?>  ?=(%uint -.sop)
      `@`p.sop
    ::
      =+  act=(snag 9 das)
      ?>  ?=(%bool -.act)
      ~&  p.act
      ?.  p.act  ~
      =+  esc=(snag 8 das)
      ?>  ?=(%uint -.esc)
      ``@`p.esc
  ==
::
++  poke-noun
  |=  a/@
  =+  ships-c=0xa9c7.9b9c.5e4e.1fdc.69c1.9fc0.6232.64c8.da50.7a22
  ?:  =(a 1)
    %+  send-rpc-req  /block
    (request-to-json `'eth-blocknum' [%eth-block-number ~])
  ?:  =(a 2)
    %+  send-rpc-req  /call
    %-  batch-read-request
    :~  :+  `'ships-of 0x0'
          ships-c
        ['getOwnedShips(address)' ~[address+0x0]]
      ::
        :+  `'ship-data 0'
          ships-c
        ['getShipData(uint32)' ~[uint+0]]
    ==
  ?:  =(a 3)
    ~&  %making-filter
    %+  send-rpc-req  /new-filter
    %+  request-to-json  `'new-filter'
    [%eth-new-filter ~ ~ ~[ships-c] ~]
  ?:  =(a 4)
    ~&  [%asking-filter-update (need filter)]
    %+  send-rpc-req  /filter-update
    %+  request-to-json  `'req-iq'
    [%eth-get-filter-changes (need filter)]
  [~ +>.$]
::
++  send-move
  |=  c/card
  [[ost.bol c]~ +>.$]
::
++  send-rpc-req
  |=  [w=wire j=json]
  %^  send-move  %hiss  w
  :^  ~  %json-rpc-response  %hiss
  =-  (json-request - j)
  =+  (need (de-purl:html 'http://localhost:8545'))
  -(p.p |)
::
++  sigh-tang
  |=  [w=wire t=tang]
  ~&  [%failed-sigh]
  ~&  (turn t (cury wash [0 80]))
  [~ +>.$]
::
++  sigh-json-rpc-response
  |=  [w=wire r=response:json-rpc]
  ~&  [%rpc-resp w r]
  [~ +>.$]
--
