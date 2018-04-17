/-  constitution, ethereum, json-rpc
/+  constitution, ethereum
::TODO  =,  ethereum / constitution causes bail fail. find minimal repro
=>  [. constitution ^constitution ethereum]
=,  eyre
|%
++  state
  $:  ships=(map @p hull)
      block=@ud                                         ::  last heard
      dns=[pri=@t sec=@t ter=@t]
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
  :: ta-save:ta
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
    =-  [(weld - (flop moves)) ..ta]
    ^-  (list move)
    ?~  reqs  ~
    :_  ~
    :-  ost.bol
    %+  rpc-request:ca  wir
    a+(turn (flop reqs) request-to-json)
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
    ::
        %get-spawned
      :-  (crip "getSpawned({(scow %p who.cal)})")
      ['getSpawned(uint32)' ~[uint+`@`who.cal]]
    ::
        %dns-domains
      :-  (crip "dnsDomains({(scow %ud ind.cal)})")
      ['dnsDomains(uint8)' ~[uint+ind.cal]]
    ==
  ::
  ++  ta-read-ships
    |=  who=(list @p)
    ?~  who  this
    $(who t.who, this (ta-read %ships i.who))
  ::
  ::
  ++  ta-init
    %-  ta-read-ships(wir /init)
    (gulf ~zod ~nec) ::TODO ~fes)
  ::
  ++  ta-init-results
    |=  rep=response:json-rpc
    ^+  this
    ?>  ?=(%batch -.rep)
    =.  wir  /init
    |-  ^+  +>.^$
    ?~  bas.rep  +>.^$
    =.  +>.^$  (ta-init-result i.bas.rep)
    $(bas.rep t.bas.rep)
  ::
  ++  ta-init-result
    |=  rep=response:json-rpc
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%rpc-error message.rep]
      ::TODO  retry or something
      +>.$
    ?>  ?=(%s -.res.rep)
    =+  cal=(parse-id id.rep)
    ?:  ?=(%get-spawned -.cal)
      =/  kis=(list @p)
        %-  (list @p)  ::NOTE  because arrays are still typeless
        (decode-results p.res.rep [%array %uint]~)
      =.  ships
        %+  ~(put by ships)  who.cal
        =+  (~(got by ships) who.cal)
        -(spawned (~(gas in spawned) kis))
      (ta-read-ships kis)
    ::?>  ?=(%ships -.cal)
    ?>  ?=(%s -.res.rep)
    =/  hul=hull:eth-noun
      (decode-results p.res.rep hull:eth-type)
    ?.  active.hul  +>.$
    =.  ships
      %+  ~(put by ships)  who.cal
      (hull-from-eth hul)
    ?:  =(0 spawn-count.hul)  +>.$
    (ta-read %get-spawned who.cal)
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
    ::
      ?>  =(32 p.encryption-key)
      `@`q.encryption-key
    ::
      ?>  =(32 p.authentication-key)
      `@`q.authentication-key
    ::
      key-revision
    ::
      spawn-count
    ::
      ~
    ::
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
  ta-save:(ta-init-results:ta r)
::
++  sigh-json-rpc-response
  |=  [w=wire r=response:json-rpc]
  ~&  [%rpc-resp w r]
  [~ +>.$]
--
