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
      heard=(set (pair @ud @ud))
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
::
++  delta
  $?  diff-constitution
  $%  [%checking who=@p part=(unit hull)]
      [%filter id=@ud]
  ==  ==
::
::
+=  move  [bone card]                                   ::  [target side-effect]
++  card                                                ::  side-effect
  $%  [%peer wire gill:gall path]
      [%diff %constitution-diff diff-constitution]
      [%hiss wire (unit user:eyre) mark [%hiss hiss]]
      [%wait wire @da]
  ==
--
::
|_  {bol=bowl:gall state}
::
++  prep
  |=  old=(unit *)::state)
  ^-  (quip move _+>)
  :: ?~  old
    %-  complete
    ta-save:ta-init:ta
  :: [~ ..prep(+<+ u.old)]
::
++  complete
  |=  [des=(list delta) mos=(list move)]
  ^-  (quip move _+>)
  :-  (weld mos (share des))
  da-save:(da-changes:da des)
::
++  share
  |=  des=(list delta)
  ^-  (list move)
  %-  zing
  %+  turn  des
  |=  det=delta
  ^-  (list move)
  %+  murn  ~(tap by sup.bol)
  |=  [b=bone s=ship p=path]
  ^-  (unit move)
  ?.  ?=([%state *] p)  ~
  ?:  ?=(?(%checking %filter) -.det)  ~
  `[b %diff %constitution-diff det]
::
++  ta
  |_  $:  moves=(list move)                             ::  side-effects
          deltas=(list delta)
          reqs=(list (pair (unit @t) request))          ::  rpc requests
          wir=wire                                      ::  wire for reqs
      ==
  ::
  ++  ta-save
    ^-  [des=(list delta) mos=(list move)]
    :-  (flop deltas)
    =-  (weld - (flop moves))
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
  ++  ta-change
    |=  det=delta
    %_(+> deltas [det deltas])
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
  ++  ta-read-dns
    =+  inx=(gulf 0 2)
    |-
    ?~  inx  ..ta-read-dns
    =.  ..ta-read-dns  (ta-read %dns-domains i.inx)
    $(inx t.inx)
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
  ++  ta-wait-poll
    %-  ta-card
    ::NOTE  may adjust wrt filter timeout
    [%wait /poll (add now.bol ~m4)]
  ::
  ::
  ++  ta-init  ta-new-filter
  ::
  ++  ta-run-check
    |=  save=?
    =.  wir  (weld /read ?:(save /reset /verify))
    =<  ta-read-dns
    (ta-read-ships (gulf ~zod ~nec))  ::TODO  ~fes
  ::
  ::
  ++  ta-take-filter
    |=  rep=response:json-rpc
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%filter-error--retrying message.rep]
      ta-new-filter
    =+  fit=(parse-eth-new-filter-res res.rep)
    =.  +>.$  (ta-change %filter fit)
    ta-read-filter(filter fit)
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
    =.  +>  ta-wait-poll
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
    ::TODO  if the block number is less than latest, that means we got
    ::      events out of order somehow and should probably reset.
    ::
    =*  place  u.mined.log
    ?:  (~(has in heard) block-number.place log-index.place)
      ~&  %ignoring-duplicate-event
      +>
    =.  +>.$
      %+  ta-change  %heard
      [block-number.place log-index.place]
    ::
    ?:  =(event.log changed-dns:ships-events)
      =+  ^-  [pri=tape sec=tape ter=tape]
        (decode-results data.log ~[%string %string %string])
      =?  +>.$  !=(pri.dns (crip pri))
        (ta-change %dns 0 (crip pri))
      =?  +>.$  !=(sec.dns (crip sec))
        (ta-change %dns 1 (crip sec))
      =?  +>.$  !=(ter.dns (crip ter))
        (ta-change %dns 2 (crip ter))
      +>.$
    ::
    =+  dis=(event-log-to-hull-diffs log)
    |-  ^+  +>.^$
    ?~  dis  +>.^$
    =.  +>.^$  (ta-change %hull i.dis)
    $(dis t.dis)
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
      =.  +>.$  (ta-read %get-spawned who.cal)
      (ta-change %checking who.cal `(hull-from-eth hul))
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
      =.  +>.$
        ?~  have
          ~&  [%completely-missing who.cal]
          ?.  save  +>.$
          ~&  [%storing-chain-version-of who.cal]
          (ta-change %hull who.cal %full hul)
        ::
        =*  huv  state.u.have
        ?:  =(huv hul)  +>.$
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
        ?.  save  +>.$
        ~&  [%storing-chain-version-of who.cal]
        (ta-change %hull who.cal %full hul)
      ::
      =.  +>.$  (ta-read-ships kis)
      (ta-change %checking who.cal ~)
    ::
        %dns-domains
      ?>  ?=(%s -.res.rep)
      =+  dom=(crip (decode-results p.res.rep ~[%string]))
      ?:  =(0 ind.cal)
        ?:  =(pri.dns dom)  +>.$
        ~&  [%primary-dns-differs pri.dns dom]
        ?.  save  +>.$
        (ta-change %dns 0 dom)
      ?:  =(1 ind.cal)
        ?:  =(sec.dns dom)  +>.$
        ~&  [%secondary-dns-differs sec.dns dom]
        ?.  save  +>.$
        (ta-change %dns 1 dom)
      ?:  =(2 ind.cal)
        ?:  =(ter.dns dom)  +>.$
        ~&  [%tertiary-dns-differs ter.dns dom]
        ?.  save  +>.$
        (ta-change %dns 2 dom)
      !!
    ==
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
::  arms for delta application
++  da
  |%
  ++  da-save  ..da
  ::
  ++  da-changes
    |=  des=(list delta)
    ?~  des  +>.$
    =.  +>.$  (da-change i.des)
    $(des t.des)
  ::
  ++  da-change
    |=  det=delta
    ^+  +>
    ?-  -.det
      %hull       (da-change-hull +.det)
      %dns        (da-change-dns +.det)
      %heard      (da-add-heard +.det)
      %checking   (da-change-checking +.det)
      %filter     (da-change-filter +.det)
    ==
  ::
  ++  da-change-hull
    |=  [who=@p dif=diff-hull]
    =-  +>.$(ships -)
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
  ::
  ++  da-change-dns
    |=  [ind=@ud new=@t]
    ?:  =(0 ind)  +>(pri.dns new)
    ?:  =(1 ind)  +>(sec.dns new)
    ?:  =(2 ind)  +>(ter.dns new)
    !!
  ::
  ++  da-add-heard
    |=  [block=@ud log=@ud]
    =-  +>.$(heard har, latest-block las)
    ^-  [har=(set (pair @ud @ud)) las=@ud]
    :-  (~(put in heard) block log)
    (max latest-block block)
  ::
  ++  da-change-checking
    |=  [who=@p tmp=(unit hull)]
    =-  +>.$(checking -)
    ?~  tmp  (~(del by checking) who)
    (~(put by checking) who u.tmp)
  ::
  ++  da-change-filter
    |=  id=@ud
    +>(filter id)
  --
::
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
  %-  complete
  ?:  =(a 0)
    ~&  [%have-ships ~(key by ships)]
    ~&  [%zod (~(get by ships) ~zod)]
    ta-save:ta
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
  [~ ~]
::
++  sigh-tang
  |=  [w=wire t=tang]
  ~&  [%failed-sigh w]
  ~&  (turn t (cury wash [0 80]))
  [~ +>.$]
::
::  when we get the timer: poll filter
++  wake-poll
  |=  [w=wire ~]
  %-  complete
  ta-save:ta-poll-filter:ta
::
::  when we get a new filter: read it, kick timer
::  when we get log or poll results: apply them
++  sigh-json-rpc-response-filter
  |=  [w=wire r=response:json-rpc]
  ~&  [%got-filter-results w]
  %-  complete
  =<  ta-save
  ?:  ?=([%new *] w)
    (ta-take-filter:ta r)
  (ta-take-filter-results:ta r)
::
::  when we get read results: verify/reset
++  sigh-json-rpc-response-read
  |=  [w=wire r=response:json-rpc]
  %-  complete
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
