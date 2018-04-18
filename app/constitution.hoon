/-  constitution, ethereum, json-rpc
/+  constitution, ethereum
::TODO  =,  ethereum / constitution causes bail fail. find minimal repro
=>  [. constitution ^constitution ethereum]
=,  eyre
|%
++  state
  $:  ships=(map @p hull)
      dns=[pri=@t sec=@t ter=@t]
      latest-block=@ud                                  ::  last heard
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
    :_  +>
    =-  (weld - (flop moves))
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
    =+  (ships:function-to-call cal)
    %+  ta-request  `id
    :+  %eth-call
      [~ ships:contracts ~ ~ ~ (encode-call dat)]
    [%label %latest]
  ::
  ++  ta-read-ships
    |=  who=(list @p)
    ?~  who  this
    $(who t.who, this (ta-read %ships i.who))
  ::
  ::
  ++  ta-new-filter
    %+  ta-request  `'new filter'
    :*  %eth-new-filter
        `[%number +(latest-block)]
        ~
        ~[ships:contracts]
        ~
    ==
  ::
  ++  ta-take-filter
    |=  rep=response:json-rpc
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%filter-rpc-error message.rep]
      ::TODO  retry or something
      +>
    =.  filter  `(parse-eth-new-filter-res res.rep)
    ta-read-filter
  ::
  ++  ta-poll-filter
    =.  wir  /init
    ?~  filter  ta-new-filter
    %+  ta-request  `'poll filter'
    [%eth-get-filter-changes u.filter]
  ::
  ++  ta-read-filter
    =.  wir  /init
    ?~  filter  ta-new-filter
    %+  ta-request  `'filter logs'
    [%eth-get-filter-logs u.filter]
  ::
  ++  ta-take-filter-results
    |=  rep=response:json-rpc
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ?.  =('filter not found' message.rep)
        ~&  [%filter-rpc-error message.rep]
        +>
      ta-new-filter
    ?>  ?=(%a -.res.rep)
    =*  changes  p.res.rep
    ~&  [%filter-changes (lent changes)]
    |-  ^+  +>.^$
    ?~  changes  +>.^$
    =.  +>.^$  (ta-take-filter-result i.changes)
    $(changes t.changes)
  ::
  ++  ta-take-filter-result
    |=  res=json
    =,  dejs:format
    =+  log=(parse-event-log res)
    ?~  mined.log  +>.$
    ?:  =(event.log changed-dns:ships-events)
      ::TODO  update dns in state
      =+  (decode-results data.log ~[%string %string %string])
      +>.$
    ~&  (turn (event-log-to-hull-diffs log) (cork tail head))
    +>.$
  ::
  ::
  ++  ta-init
    =.  wir  /init
    =<  ta-new-filter
    ::TODO  =<  ta-read-dns
    %-  ta-read-ships
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
    ?:  =('new filter' id.rep)
      (ta-take-filter rep)
    ?:  ?|  =('poll filter' id.rep)
            =('filter logs' id.rep)
        ==
      (ta-take-filter-results rep)
    ?:  ?=(%error -.rep)
      ~&  [%init-rpc-error message.rep]
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
    ?>  ?=(%ships -.cal)
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
  ==
::
++  poke-noun
  |=  a/@
  ^-  (quip move _+>)
  ?:  =(a 0)
    ~&  [%have-ships ~(key by ships)]
    [~ +>.$]
  ?:  =(a 1)
    ta-save:ta-poll-filter:ta
  ?:  =(a 2)
    ta-save:ta-new-filter:ta
  ?:  =(a 3)
    ta-save:ta-read-filter:ta
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
++  sigh-json-rpc-response-filter-poll
  |=  [w=wire r=response:json-rpc]
  ~&  %got-filter-poll-results
  ta-save:(ta-take-filter-results:ta r)
::
++  sigh-json-rpc-response
  |=  [w=wire r=response:json-rpc]
  ~&  [%rpc-resp w r]
  [~ +>.$]
--
