/-  constitution, ethereum, json-rpc
/+  constitution, ethereum
::TODO  =,  ethereum / constitution causes bail fail. find minimal repro
=>  [. constitution ^constitution ethereum]
=,  eyre
|%
++  state
  $:  ships=fleet
      checking=(map @p hull)
      dns=dnses
      heard=events
      latest-block=@ud                                  ::  last heard
      filter=@ud                                        ::  our filter id
      config=configuration
  ==
::
++  configuration
  $:  src=source
      poll-time=@dr
  ==
::
++  source
  $%  [%ship who=@p]
      [%rpc url=purl]
  ==
::
::
+=  move  [bone card]                                   ::  [target side-effect]
++  card                                                ::  side-effect
  $%  [%peer wire gill:gall path]
      [%pull wire gill:gall ~]
      [%diff %constitution-update update]
      [%hiss wire (unit user:eyre) mark [%hiss hiss]]
      [%wait wire @da]
  ==
--
::
|_  {bol=bowl:gall state}
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _+>)
  :: ?~  old
    ta-save:ta-init:ta
  :: [~ ..prep(+<+ u.old)]
::
++  ta
  |_  $:  moves=(list move)                             ::  side-effects
          diffs=(list diff-constitution)
          reqs=(list (pair (unit @t) request))          ::  rpc requests
          wir=wire                                      ::  wire for reqs
      ==
  ::
  ++  ta-save
    ^-  (quip move _+>)
    :_  ..ta
    =-  (weld - (flop moves))
    %+  weld
      ^-  (list move)
      ?~  reqs  ~
      =-  [ost.bol -]~
      %+  rpc-request:ca  wir
      a+(turn (flop reqs) request-to-json)
    ?:  =(0 (lent diffs))  ~  ::TODO  this is a tmi workaround
    =.  diffs  (flop diffs)
    ^-  (list move)
    %+  murn  ~(tap by sup.bol)
    |=  [b=bone s=ship p=path]
    ^-  (unit move)
    ?.  ?=([%state *] p)  ~
    `[b (updates:ca diffs)]
  ::
  ++  ta-move
    |=  mov=move
    %_(+> moves [mov moves])
  ::
  ++  ta-moves
    |=  mos=(list move)
    %_(+> moves (weld (flop mos) moves))
  ::
  ++  ta-card
    |=  car=card
    (ta-move [ost.bol car])
  ::
  ++  ta-to-all
    |=  upd=update
    %-  ta-moves
    %+  murn  ~(tap by sup.bol)
    |=  [b=bone s=ship p=path]
    ^-  (unit move)
    ?.  ?=([%state *] p)  ~
    `[b %diff %constitution-update upd]
  ::
  ++  ta-change
    |=  dif=diff-constitution
    (da(diffs [dif diffs]) [dif]~)
  ::
  ++  ta-changes
    |=  dis=(list diff-constitution)
    (da(diffs (weld (flop dis) diffs)) dis)
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
    [%wait /poll (add now.bol poll-time.config)]
  ::
  ::
  ++  ta-init
    =.  poll-time.config  ~m4
    =+  bos=(sein:title our.bol)
    ?.  =(our.bol bos)
      =.  src.config  [%ship bos]
      (ta-card (subscribe-to:ca bos))
    =+  (need (de-purl:html 'http://localhost:8545'))
    =.  src.config  [%rpc -(p.p |)]
    ta-new-filter
  ::
  ++  ta-run-check
    |=  save=?
    =.  wir  (weld /read ?:(save /reset /verify))
    =<  ta-read-dns
    (ta-read-ships (gulf ~zod ~nec))  ::TODO  ~fes
  ::
  ::
  ++  ta-serve  (ta-card full-state:ca)
  ::
  ++  ta-assume
    |=  [s=fleet d=dnses h=events]
    ?:  &(=(s ships) =(d dns) =(h heard))  +>
    ~&  [%ta-assume ~(wyt by s) ~(wyt in h)]
    (ta-to-all(ships s, dns d, heard h) %full s d h)
  ::
  ++  ta-accept
    |=  dis=(list diff-constitution)
    ~&  [%ta-accept (lent dis)]
    (ta-changes dis)
  ::
  ::
  ++  ta-take-filter
    |=  rep=response:json-rpc
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%filter-error--retrying message.rep]
      ta-new-filter
    =-  ta-read-filter(filter -)
    (parse-eth-new-filter-res res.rep)
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
      =.  checking  (~(put by checking) who.cal (hull-from-eth hul))
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
      =.  checking  (~(del by checking) who.cal)
      (ta-read-ships kis)
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
  ::
  ::
  ++  da
    |=  dis=(list diff-constitution)
    ^+  +>
    |^  ?~  dis  +>.^$
        =.  ..da
          =*  dif  i.dis
          ?-  -.dif
            %hull   (da-hull +.dif)
            %dns    (da-dns +.dif)
            %heard  (da-heard +.dif)
          ==
        $(dis t.dis)
    ::
    ++  da-hull
      |=  [who=@p dif=diff-hull]
      =-  ..da(ships -)
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
    ++  da-dns
      |=  [ind=@ud new=@t]
      ?:  =(0 ind)  ..da(pri.dns new)
      ?:  =(1 ind)  ..da(sec.dns new)
      ?:  =(2 ind)  ..da(ter.dns new)
      !!
    ::
    ++  da-heard
      |=  [block=@ud log=@ud]
      =-  ..da(heard har, latest-block las)
      ^-  [har=(set (pair @ud @ud)) las=@ud]
      :-  (~(put in heard) block log)
      (max latest-block block)
    --
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
    ?>  ?=(%rpc -.src.config)
    url.src.config
  ::
  ++  subscribe-to
    |=  who=@p
    ^-  card
    :*  %peer
        /source/(scot %p who)
        [who dap.bol]
        /state
    ==
  ::
  ++  unsubscribe-from
    |=  who=@p
    ^-  card
    :*  %pull
        /source/(scot %p who)
        [who dap.bol]
        ~
    ==
  ::
  ++  full-state
    ^-  card
    :+  %diff  %constitution-update
    [%full ships dns heard]
  ::
  ++  updates
    |=  dis=(list diff-constitution)
    ^-  card
    [%diff %constitution-update %diff dis]
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
  =<  ta-save
  ?:  =(a 0)
    ~&  [%have-ships ~(key by ships)]
    ~&  [%zod (~(get by ships) ~zod)]
    ta
  ?:  =(a 1)  ta-poll-filter:ta
  ?:  =(a 2)  ta-new-filter:ta
  ?:  =(a 3)  ta-read-filter:ta
  ?:  =(a 4)  (ta-run-check:ta |)
  ?:  =(a 5)  (ta-run-check:ta &)
  ta
::
::
++  peer-state
  |=  p=path
  ^-  (quip move _+>)
  ~&  %peer-state
  ~?  ?=(^ p)  [%ignoring-specific-state p]
  ta-save:ta-serve:ta
::
++  diff-constitution-update
  |=  [w=wire u=update]
  ^-  (quip move _+>)
  =<  ta-save
  ?-  -.u
    %full   (ta-assume:ta +.u)
    %diff   (ta-accept:ta +.u)
  ==
::
::
::  when we get the timer: poll filter
++  wake-poll
  |=  [w=wire ~]
  ^-  (quip move _+>)
  ?.  ?=(%rpc -.src.config)  [~ +>]
  ~&  [%waking-for-poll ost.bol now.bol]
  ::TODO  maybe we need a way to get rid of double timers if they ever occur?
  ta-save:ta-poll-filter:ta
::
++  sigh-tang
  |=  [w=wire t=tang]
  ^-  (quip move _+>)
  ~&  [%failed-sigh w]
  ~&  (turn t (cury wash [0 80]))
  ::TODO  actually do error handling, be sure to continue the thing the request
  ::      was trying to do.
  [~ +>.$]
::
::  when we get a new filter: read it, kick timer
::  when we get log or poll results: apply them
++  sigh-json-rpc-response-filter
  |=  [w=wire r=response:json-rpc]
  ^-  (quip move _+>)
  ?.  ?=(%rpc -.src.config)  [~ +>]
  =<  ta-save
  ?:  ?=([%new *] w)
    (ta-take-filter:ta r)
  (ta-take-filter-results:ta r)
::
::  when we get read results: verify/reset
++  sigh-json-rpc-response-read
  |=  [w=wire r=response:json-rpc]
  ^-  (quip move _+>)
  ?.  ?=(%rpc -.src.config)  [~ +>]
  =<  ta-save
  ?+  w  ~&(%unknown-read-reason ta)
    [%verify ~]   (ta-take-read-results:ta r |)
    [%reset ~]    (ta-take-read-results:ta r &)
  ==
--
