/-  constitution, ethereum, json-rpc
/+  constitution, ethereum
::TODO  =,  ethereum / constitution causes bail fail. find minimal repro
=>  [. constitution ^constitution ethereum]
=,  eyre
|%
++  state
  $:  ships=(map @p complete-ship)
      checking=(map @p hull)
      dns=[pri=@t sec=@t ter=@t]
      latest-block=@ud                                  ::  last heard
      filter=@ud                                        ::  our filter id
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
  ++  ta-request-single
    |=  [wir=wire id=(unit @t) req=request]
    %-  ta-card
    %+  rpc-request:ca  wir
    (request-to-json id req)
  ::
  ++  ta-read
    |=  cal=ships:function
    =+  (ships:function-to-call cal)
    %+  ta-request  `id
    :+  %eth-call
      [~ ships:contracts ~ ~ ~ (encode-call dat)]
    ::NOTE  we can't make read calls to not the latest block. however,
    ::      you risk getting data that filter polling hasn't yet seen,
    ::      so probably kick the filter before doing any important reads.
    [%label %latest]
  ::
  ++  ta-read-ships
    |=  who=(list @p)
    ?~  who  +>
    =.  +>  (ta-read %ships i.who)
    $(who t.who)
  ::
  ::
  ++  ta-new-filter
    %-  ta-request-single
    :+  /filter/new  `'new filter'
    :*  %eth-new-filter
        `[%number +(latest-block)]  ::TODO  or Ships origin block when 0
        ~
        ~[ships:contracts]
        ~
    ==
  ::
  ++  ta-read-filter
    %-  ta-request-single
    :+  /filter  `'filter logs'
    [%eth-get-filter-logs filter]
  ::
  ++  ta-poll-filter
    %-  ta-request-single
    :+  /filter  `'poll filter'
    [%eth-get-filter-changes filter]
  ::
  ::
  ++  ta-init  ta-new-filter
  ::
  ++  ta-run-check
    |=  save=?
    =.  wir  (weld /read ?:(save /reset /verify))
    (ta-read-ships (gulf ~zod ~nec))  ::TODO  ~fes
  ::
  ::
  ++  ta-take-filter
    |=  rep=response:json-rpc
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%filter-error--retrying message.rep]
      ta-new-filter
    =.  filter  (parse-eth-new-filter-res res.rep)
    ta-read-filter
  ::
  ++  ta-take-filter-results
    |=  rep=response:json-rpc
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ?.  =('filter not found' message.rep)
        ~&  [%unhandled-filter-error message.rep]
        +>
      ~&  %filter-timed-out--recreating
      ta-new-filter
    ::TODO  kick for poll
    ?>  ?=(%a -.res.rep)
    =*  changes  p.res.rep
    ~&  [%filter-changes (lent changes)]
    |-  ^+  +>.^$
    ?~  changes  +>.^$
    =.  +>.^$
      (ta-take-event-log (parse-event-log i.changes))
    $(changes t.changes)
  ::
  ++  ta-take-event-log
    |=  log=event-log
    ^+  +>
    ?~  mined.log
      ~&  %ignoring-unmined-event
      +>
    ::
    =?  latest-block  (gth block-number.u.mined.log latest-block)
      block-number.u.mined.log
    ::
    ?:  =(event.log changed-dns:ships-events)
      =+  ^-  [pri=tape sec=tape ter=tape]
        (decode-results data.log ~[%string %string %string])
      %_  +>.$
        pri.dns  (crip pri)
        sec.dns  (crip sec)
        ter.dns  (crip ter)
      ==
    ::
    =+  dis=(event-log-to-hull-diffs log)
    |-  ^+  +>.^$
    ?~  dis  +>.^$
    $(dis t.dis, ships (store-hull-change i.dis))
  ::
  ::
  ++  ta-take-read-results
    |=  [rep=response:json-rpc save=?]
    ^+  +>
    ?>  ?=(%batch -.rep)
    =.  wir  (weld /read ?:(save /reset /verify))
    |-  ^+  +>.^$
    ?~  bas.rep  +>.^$
    =.  +>.^$
      (ta-take-read-result i.bas.rep save)
    $(bas.rep t.bas.rep)
  ::
  ++  ta-take-read-result
    |=  [rep=response:json-rpc save=?]
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%unhandled-read-error id.rep message.rep]
      +>
    =/  cal=ships:function  (parse-id id.rep)
    ::TODO  think about a better way to structure the comparison code below
    ?-  -.cal  ::  ~&([%unhandled-read-result -.cal] +>.$)
        %ships
      ?>  ?=(%s -.res.rep)
      =/  hul=hull:eth-noun
        ~|  [id.rep p.res.rep]
        (decode-results p.res.rep hull:eth-type)
      ::  ignore inactive ships
      ?.  active.hul  +>.$
      ::  we store the read data for now, and only compare with state once we
      ::  have completed it by learning the spawned ships.
      =.  checking
        (~(put by checking) who.cal (hull-from-eth hul))
      (ta-read %get-spawned who.cal)
    ::
        %get-spawned
      ?>  ?=(%s -.res.rep)
      =+  hul=(~(got by checking) who.cal)
      =/  kis=(list @p)
        ::TODO  can we let this be if we're cool with just @ ?
        %-  (list @p)  ::NOTE  because arrays are still typeless
        (decode-results p.res.rep [%array %uint]~)
      =.  hul  hul(spawned (~(gas in *(set @p)) kis))
      ::
      =+  have=(~(get by ships) who.cal)
      =.  ships
        ?~  have
          ~&  [%completely-missing who.cal]
          ?.  save  ships
          ~&  [%storing-chain-version-of who.cal]
          (store-hull-change who.cal %full hul)
        ::
        =*  huv  state.u.have
        ?:  =(huv hul)  ships
        ~&  [%differs-from-chain-version who.cal]
        ~&  [%what %have %chain]
        ::TODO  can we maybe re-use some ++redo code to simplify this?
        ~?  !=(owner.huv owner.hul)
          :-  %owner-differs
          [owner.huv owner.hul]
        ~?  !=(encryption-key.huv encryption-key.hul)
          :-  %encryption-key-differs
          [encryption-key.huv encryption-key.hul]
        ~?  !=(authentication-key.huv authentication-key.hul)
          :-  %authentication-key-differs
          [authentication-key.huv authentication-key.hul]
        ~?  !=(key-revision.huv key-revision.hul)
          :-  %key-revision-differs
          [key-revision.huv key-revision.hul]
        ~?  !=(spawn-count.huv spawn-count.hul)
          :-  %spawn-count-differs
          [spawn-count.huv spawn-count.hul]
        ~?  !=(spawned.huv spawned.hul)
          :-  %spawned-differs
          [spawned.huv spawned.hul]
        ~?  !=(sponsor.huv sponsor.hul)
          :-  %sponsor-differs
          [sponsor.huv sponsor.hul]
        ~?  !=(escape.huv escape.hul)
          :-  %escape-differs
          [escape.huv escape.hul]
        ~?  !=(spawn-proxy.huv spawn-proxy.hul)
          :-  %spawn-proxy-differs
          [spawn-proxy.huv spawn-proxy.hul]
        ~?  !=(transfer-proxy.huv transfer-proxy.hul)
          :-  %transfer-proxy-differs
          [transfer-proxy.huv transfer-proxy.hul]
        ::
        ~&  %$
        ?.  save  ships
        ~&  [%storing-chain-version-of who.cal]
        (store-hull-change who.cal %full hul)
      ::
      =.  checking  (~(del by checking) who.cal)
      (ta-read-ships kis)
    ==
  ::
  ++  store-hull-change
    |=  [who=@p dif=diff-hull]
    ^+  ships
    ::  if new, first dif must be %full
    ?>  |((~(has by ships) who) ?=(%full -.dif))
    =+  old=(fall (~(get by ships) who) *complete-ship)
    ::  catch key changes, store them in the key map
    =?  keys.old  ?=(%keys -.dif)
      ~?  &((gth rev.dif 0) !(~(has by keys.old) (dec rev.dif)))
        [%missing-previous-key-rev who (dec rev.dif)]
      (~(put by keys.old) rev.dif enc.dif aut.dif)
    ::  for full, store the new keys in case we don't have them yet
    =?  keys.old  ?=(%full -.dif)
      =,  new.dif
      ~?  &((gth key-revision 0) !(~(has by keys.old) (dec key-revision)))
        [%missing-previous-key-rev who (dec key-revision)]
      %+  ~(put by keys.old)  key-revision
      [encryption-key authentication-key]
    =.  state.old     (apply-hull-diff state.old dif)
    =.  history.old   [dif history.old]
    ::  apply dif to ship state
    (~(put by ships) who old)
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
  ^-  (quip move _+>)
  ?>  =(src.bol our.bol)
  ?:  =(a 0)
    ~&  [%have-ships ~(key by ships)]
    [~ +>.$]
  ?:  =(a 1)
    ta-save:ta-poll-filter:ta
  ?:  =(a 2)
    ta-save:ta-new-filter:ta
  ?:  =(a 3)
    ta-save:ta-read-filter:ta
  ?:  =(a 4)
    ta-save:(ta-run-check:ta |)
  ?:  =(a 5)
    ta-save:(ta-run-check:ta &)
  [~ +>.$]
::
++  sigh-tang
  |=  [w=wire t=tang]
  ~&  [%failed-sigh w]
  ~&  (turn t (cury wash [0 80]))
  [~ +>.$]
::
::  when we get a new filter: read it, kick timer
::  when we get log or poll results: apply them
++  sigh-json-rpc-response-filter
  |=  [w=wire r=response:json-rpc]
  ~&  [%got-filter-results w]
  =<  ta-save
  ?:  ?=([%new *] w)
    (ta-take-filter:ta r)
  (ta-take-filter-results:ta r)
::
::  when we get read results: verify/reset
++  sigh-json-rpc-response-read
  |=  [w=wire r=response:json-rpc]
  =<  ta-save
  ?+  w  ~&(%unknown-read-reason ta)
    [%verify ~]   (ta-take-read-results:ta r |)
    [%reset ~]    (ta-take-read-results:ta r &)
  ==
::
++  sigh-json-rpc-response
  |=  [w=wire r=response:json-rpc]
  ~&  [%rpc-resp w r]
  [~ +>.$]
--
