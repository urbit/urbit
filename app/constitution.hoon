/-  constitution, ethereum, json-rpc
/+  ethereum
::TODO  =,  ethereum / constitution causes bail fail. find minimal repro
=>  [. constitution ethereum]
=,  eyre
|%
++  state
  $:  ships=(map @p hull)
      block=@ud                                         ::  last heard
      filter=(unit @ud)                                 ::  our filter id
  ==
::
++  complete-ship
  $:  state=hull
      history=(list diff-hull)                          ::  newest first
      keys=(map @ud (pair @ @))
  ==
::
+=  move  [bone card]                                   ::  [target side-effect]
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
    ta-save:ta-init:ta
  :: [~ ..prep(fid u.old)]
::
++  ta
  |_  $:  moves=(list move)                             ::  side-effects
          reqs=(list (pair (unit @t) request))          ::  rpc requests
          wir=wire                                      ::  wire for reqs
      ==
  +*  this  .
  ::
  ++  ta-save
    ^-  (quip move _+>)
    =-  [[`move`- `(list move)`(flop moves)] ..ta]
    ^-  move
    :-  `bone`ost.bol
    ^-  card
    %+  rpc-request:ca  wir
    `json`a+(turn (flop reqs) request-to-json)
  ::
  ++  ta-move
    |=  mov=move
    %_(+> moves [mov moves])
  ::
  ++  ta-card
    |=  car=card
    (ta-move [ost.bol car])
  ::
  ++  ta-request
    |=  [id=(unit @t) req=request]
    %_(+> reqs [[id req] reqs])
  ::
  ++  ta-read
    |=  cal=ships:function
    =-  %+  ta-request  `id
        :+  %eth-call
          [~ ships:contracts ~ ~ ~ (encode-call dat)]
        [%label %latest]
    ::TODO  probably turn the below into a lib arm
    ^-  [id=@t dat=call-data]
    ?-  -.cal
        %ships
      :-  (crip "ships({(scow %p who.cal)})")
      ['ships(uint32)' ~[uint+`@`who.cal]]
    ==
  ::
  ++  ta-read-ships
    |=  who=(list @p)
    ~&  [%ta-read-ships ~(key by ships)]
    %^  spir  who  this
    |=([p=@p _this] (ta-read %ships p))
  ::
  ::
  ++  ta-init
    %-  ta-read-ships(wir /init)
    (gulf ~zod ~per) ::TODO ~fes)
  ::
  ++  ta-init-result
    |=  rep=response:json-rpc
    ^+  this
    ?>  ?=(%batch -.rep)
    =.  wir  /init
    %^  spir  bas.rep  this
    |=  [r=response:json-rpc this=_this]
    ^+  this
    ?<  ?=(%batch -.r)
    ~&  id.r
    ?:  ?=(%error -.r)
      ~&  [%rpc-error message.r]
      this
    ?>  ?=(%s -.res.r)
    =/  hul=hull:eth-noun
      (decode-results p.res.r hull:eth-type)
    ?.  active.hul  this
    =/  who=@p
      %+  rash  id.r
      (ifix [(jest 'ships(~') (just ')')] fed:ag)
    =.  ships
      %+  ~(put by ships)  who
      (hull-from-eth hul)
    ~&  [%stored ~(key by ships)]
    (ta-read-ships (kids who))
  --
::
::  arms for card generation
++  ca
  |%
  ++  rpc-request
    |=  [w=wire j=json]
    ^-  card
    :^  %hiss  w  ~
    :+  %json-rpc-response  %hiss
    =-  (json-request - j)
    =+  (need (de-purl:html 'http://localhost:8545'))
    -(p.p |)
  --
::
++  spir
  :>
  :>  a: list
  :>  b: state
  :>  c: gate from list-item and state to new state
  :>  produces: new state
  |*  [a=(list) b=* c=_|=(^ +<+)]
  =>  .(c `$-([_?>(?=(^ a) i.a) _b] _b)`c)
  :>  transformed list and updated state
  |-  ^+  b
  ?~  a  b
  $(a t.a, b (c i.a b))
::
++  kids
  |=  pre=@p
  ^-  (list @p)
  =/  wyd=bloq
    ?+  (clan:title pre)  0
      %czar   3
      %king   4
      %duke   5
    ==
  %+  turn
    (gulf 1 (dec (pow 2 (bex wyd))))
  ?:  =(~zod pre)
    |=(a=@p (lsh 3 1 a))
  |=(a=@p (cat wyd pre a))
::
::TODO  there definitely needs to be a helper function of some kind,
::      but is there a way for the type system to be aware of the return
::      type if we ask for ie ~[%address %uint %bool] data as a noun?
++  hull-from-eth
  |=  hul=hull:eth-noun
  ^-  hull
  =,  hul
  :*  owner
      spawn-count
    ::
      ?>  =(32 p.encryption-key)
      `@`q.encryption-key
    ::
      ?>  =(32 p.authentication-key)
      `@`q.authentication-key
    ::
      key-revision
      `@p`sponsor
    ::
      ?.  escape-requested  ~
      ``@p`escape-to
    ::
      spawn-proxy
      transfer-proxy
  ==
::
++  poke-noun
  |=  a/@
  ?:  =(a 0)
    ~&  [%have-ships ~(key by ships)]
    [~ +>.$]
  [~ +>.$]
::
++  sigh-tang
  |=  [w=wire t=tang]
  ~&  [%failed-sigh]
  ~&  (turn t (cury wash [0 80]))
  [~ +>.$]
::
++  sigh-json-rpc-response-init
  |=  [w=wire r=response:json-rpc]
  ~&  %got-init-response
  ta-save:(ta-init-result:ta r)
::
++  sigh-json-rpc-response
  |=  [w=wire r=response:json-rpc]
  ~&  [%rpc-resp w r]
  [~ +>.$]
--
