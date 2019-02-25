::  watcher: ethereum event log collector
::
/+  *eth-watcher
::
=,  ethereum
=,  rpc
::
|%
++  state
  $:  eyes=(map name eye)
  ==
::
++  eye
  $:  config
      latest-block=@ud
      filter-id=@ud
      poll-timer=(unit @da)
      snapshot
      sap=history
  ==
::
++  history
  $:  interval=_100
      max-count=_10
      count=@ud
      latest-block=@ud
      snaps=(qeu snapshot)
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire (unit user:eyre) mark %hiss hiss:eyre]
      [%wait wire @da]
      [%rest @da]
      [%info wire desk nori:clay]
      [%diff %eth-watcher-update update]
      [%quit ~]
  ==
--
::
|_  [bowl:gall state]
::
++  prep
  |=  old=(unit state)
  ?~  old
    [~ ..prep]
  [~ ..prep(+<+ u.old)]
::
++  poke-noun
  |=  [what=?(%save %load) =name]
  ^-  (quip move _+>)
  =+  eye=(fall (~(get by eyes) name) *eye)
  ?-  what
      %save
    =/  pax=path
      /(scot %p our)/home/(scot %da now)/watcher/[name]/jam
    :_  +>.$
    :_  ~
    ^-  move
    :*  ost
        %info
        /jamfile
        (foal:space:userlib pax [%jam !>((jam eye))])
    ==
  ::
      %load
    =.  eyes
      %+  ~(put by eyes)  name
      =-  (^eye (cue .^(@ %cx -)))
      /(scot %p our)/home/(scot %da now)/watcher/[name]/jam
    done:new-filter:(open:watcher name)
  ==
::
++  poke-eth-watcher-action
  |=  act=action
  ^-  (quip move _+>)
  ?-  -.act
      %watch
    done:(init:watcher +.act)
  ::
      %clear
    wipe:(open:watcher +.act)
  ==
::
++  peek-x
  |=  pax=path
  ^-  (unit (unit [%noun *]))
  ?.  ?=([@ *] pax)  ~
  =+  eye=(~(get by eyes) i.pax)
  ?~  eye  [~ ~]
  ::  /name: all logs
  ::
  ?~  t.pax  ``[%noun logs.u.eye]
  ::  /name/num: most recent num logs
  ::
  =+  num=(slaw %ud i.t.pax)
  ?^  num  ``[%noun (scag u.num logs.u.eye)]
  ::  /name/debug: debug information
  ::
  ?.  ?=(%debug i.t.pax)  ~
  =-  ``[%noun -]
  =,  u.eye
  :*  node=(en-purl:html node)
      last=last-heard-block
      lent=(lent logs)
      time=poll-timer
  ==
::
++  peer
  |=  pax=path
  ^-  (quip move _+>)
  ?>  ?=([@ ~] pax)
  done:(put-snapshot-diff:(open:watcher i.pax) ost)
::
++  wake
  |=  [wir=wire ~]
  ^-  (quip move _+>)
  ?>  ?=([@ %poll ~] wir)
  done:poll-filter:(open:watcher i.wir)
::
++  sigh-tang
  |=  [wir=wire res=tang]
  ^-  (quip move _+>)
  ~&  ['something went wrong!' wir]
  ~_  res
  [~ +>.$]
::
++  sigh-json-rpc-response
  |=  [wir=wire res=response:rpc:jstd]
  ^-  (quip move _+>)
  ?>  ?=([@ *] wir)
  =<  done
  %-  sigh-json-rpc-response:(open:watcher i.wir)
  [t.wir res]
::
++  watcher
  |_  $:  =name
          =eye
          rewind-block=(unit @ud)
          new-logs=loglist
          moves=(list move)
      ==
  ::
  ::  +open: initialize core
  ::
  ++  open
    |=  nom=^name
    ^+  +>
    +>.$(name nom, eye (~(got by eyes) nom))
  ::
  ::  +init: set up eye and initialize core
  ::
  ++  init
    |=  [nom=^name =config]
    ^+  +>
    =.  name  nom
    =.  eye
      %*(. *^eye - config, last-heard-block from-block.config)
    get-latest-block
  ::
  ::  +|  outward
  ::
  ::  +wipe: delete eye
  ::
  ++  wipe
    =>  cancel-wait-poll
    =>  cancel-subscribers
    :-  (flop moves)
    ..watcher(eyes (~(del by eyes) name))
  ::
  ::  +done: store changes, update subscribers
  ::
  ++  done
    ^-  [(list move) _..watcher]
    =?  .  ?=(^ rewind-block)
      ::  if we're rewinding to a block, then we throw away any moves
      ::  and changes we were going to make.
      ::
      =:  moves     *(list move)
          new-logs  *loglist
        ==
      (restore-block u.rewind-block)
    ::  if we have any updates, send them
    ::
    =?  .  !=(~ new-logs)
      (fan-diff %logs new-logs)
    ::  produce moves, store updated state
    ::
    :-  (flop moves)
    ..watcher(eyes (~(put by eyes) name eye))
  ::
  ::  +put-move: store side-effect
  ::
  ++  put-move
    |=  =card
    %_(+> moves [[ost card] moves])
  ::
  ++  put-moves
    |=  moz=(list move)
    %_(+> moves (weld (flop moz) moves))
  ::
  ::  +put-rpc-request: store rpc request to ethereum node
  ::
  ++  put-rpc-request
    |=  [wir=wire id=(unit @t) req=request]
    ^+  +>
    %-  put-move
    ^-  card
    :*  %hiss
        [name wir]
        ~
        %json-rpc-response
        %hiss
        %+  json-request  node.eye
        (request-to-json id req)
    ==
  ::
  ::  +put-log: store change made by event
  ::
  ++  put-log
    |=  log=event-log
    %_  +>
      new-logs    (store-new-logs ~[log] new-logs)
      logs.eye    (store-new-logs ~[log] logs.eye)
      heard.eye   (~(put in heard.eye) (log-to-id log))
    ==
  ::
  ::  +|  subscriptions
  ::
  ++  put-diff
    |=  [for=bone dif=update]
    %_(+> moves [[for %diff %eth-watcher-update dif] moves])
  ::
  ++  put-snapshot-diff
    |=  for=bone
    (put-diff for %snap last-heard-block.eye heard.eye logs.eye)
  ::
  ++  get-subscribers
    ^-  (list bone)
    %+  murn  ~(tap by sup)
    |=  [b=bone s=ship p=path]
    ^-  (unit bone)
    ?>  ?=([@ *] p)
    ?:(=(name i.p) `b ~)
  ::
  ++  fan-diff
    |=  dif=update
    %-  put-moves
    %+  turn  get-subscribers
    |=  b=bone
    ^-  move
    [b %diff %eth-watcher-update dif]
  ::
  ++  cancel-subscribers
    %-  put-moves
    %+  turn  get-subscribers
    |=(b=bone [b %quit ~])
  ::
  ::  +|  catch-up-operations
  ::
  ::  +get-latest-block
  ::
  ::    Get latest block from eth node and compare to our own latest block.
  ::    Get intervening blocks in chunks until we're caught up, then set
  ::    up a filter going forward.
  ::
  ++  get-latest-block
    =>  cancel-wait-poll
    (put-rpc-request /catch-up/block-number `'block number' %eth-block-number ~)
  ::
  ::  +catch-up: get next chunk
  ::
  ++  catch-up
    |=  from-block=@ud
    ^+  +>
    ?:  (gte from-block latest-block.eye)
      new-filter
    =/  next-block  (min latest-block.eye (add from-block 5.760))  ::  ~d1
    ~?  debug=|
      [%catching-up from=from-block to=latest-block.eye]
    %-  put-rpc-request
    :+  /catch-up/step/(scot %ud from-block)/(scot %ud next-block)
      `'catch up'
    :*  %eth-get-logs
        `number+from-block
        `number+next-block
        contracts.eye
        topics.eye
    ==
  ::
  ::  +|  filter-operations
  ::
  ::  +new-filter: request a new polling filter
  ::
  ::    Listens from the last-heard block onward.
  ::
  ++  new-filter
    %-  put-rpc-request
    :+  /filter/new  `'new filter'
    ^-  request:rpc
    :*  %eth-new-filter
        `number+last-heard-block.eye
        ?~(to-block.eye ~ `number+u.to-block.eye)
        contracts.eye
        topics.eye
    ==
  ::
  ::  +read-filter: get all events the filter captures
  ::
  ++  read-filter
    %-  put-rpc-request
    :+  /filter/logs  `'filter logs'
    [%eth-get-filter-logs filter-id.eye]
  ::
  ::  +poll-filter: get all new events since last poll (or filter creation)
  ::
  ++  poll-filter
    ?:  =(0 filter-id.eye)
      ~&  %no-filter-bad-poll
      .
    %-  put-rpc-request
    :+  /filter/changes  `'poll filter'
    [%eth-get-filter-changes filter-id.eye]
  ::
  ::  +wait-poll: remind us to poll in four minutes
  ::
  ::    Four minutes because Ethereum RPC filters time out after five.
  ::    We don't check for an existing timer or clear an old one here,
  ::    sane flows shouldn't see this being called superfluously.
  ::
  ++  wait-poll
    =+  wen=(add now ~m4)
    %-  put-move(poll-timer.eye `wen)
    [%wait name^/poll wen]
  ::
  ::  +cancel-wait-poll: remove poll reminder
  ::
  ++  cancel-wait-poll
    ?~  poll-timer.eye  ..cancel-wait-poll
    %-  put-move(poll-timer.eye ~)
    [%rest u.poll-timer.eye]
  ::
  ::  +|  filter-results
  ::
  ::  +sigh-json-rpc-response: process rpc response
  ::
  ++  sigh-json-rpc-response
    |=  [wir=wire res=response:rpc:jstd]
    ^+  +>
    ~!  -.res
    ?:  ?=(%fail -.res)
      ?:  =(405 p.hit.res)
        ~&  'HTTP 405 error (expected if using infura)'
        +>.$
      ?.  =(5 (div p.hit.res 100))
        ~&  [%http-error hit.res]
        +>.$
      ?+  wir
        ~&  [%retrying-node ~] ::((soft tang) q.res)]
        wait-poll
          [%catch-up %step @ta @ta ~]
        ~&  %retrying-catch-up
        (catch-up (slav %ud `@ta`i.t.t.wir))
      ==
    ?+  wir  ~|([%weird-sigh-wire wir] !!)
        [%filter %new *]
      (take-new-filter res)
    ::
        [%filter *]
      (take-filter-results res)
    ::
        [%catch-up %block-number ~]
      (take-block-number res)
    ::
        [%catch-up %step @ta @ta ~]
      =/  from-block  (slav %ud `@ta`i.t.t.wir)
      =/  next-block  (slav %ud `@ta`i.t.t.t.wir)
      (take-catch-up-step res from-block next-block)
    ==
  ::
  ::  +take-new-filter: store filter-id and read it
  ::
  ++  take-new-filter
    |=  rep=response:rpc:jstd
    ^+  +>
    ~|  rep
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%filter-error--retrying message.rep]
      new-filter
    =-  read-filter(filter-id.eye -)
    (parse-eth-new-filter-res res.rep)
  ::
  ::  +take-filter-results: parse results into event-logs and process them
  ::
  ++  take-filter-results
    |=  rep=response:rpc:jstd
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?:  ?=(%error -.rep)
      ?.  ?|  =('filter not found' message.rep)  ::  geth
              =('Filter not found' message.rep)  ::  parity
          ==
        ~&  [%unhandled-filter-error +.rep]
        +>
      ~&  [%filter-timed-out--recreating block=last-heard-block.eye +.rep]
      ::  arguably should rewind 40 blocks on the off chance the chain reorganized
      ::  when we blinked.  this will also restart the filter.
      ::
      ::  (restore-block ?:((lth last-heard-block 40) 0 (sub.add last-heard-block 40)))
      ::
      ::  counter-argument: it's a royal pain to restore from a snapshot
      ::  every time you can't ping the node for 5 minutes.  this is likely
      ::  to destabilize the network.  better to manually restore if we
      ::  notice an anomaly.
      ::
      ::  third way: don't trust anything that doesn't have 40 confirmations
      ::
      new-filter
    ::  kick polling timer, only if it hasn't already been.
    =?  +>  |(?=(~ poll-timer.eye) (gth now u.poll-timer.eye))
      wait-poll
    (take-events rep)
  ::
  ::  +take-block-number: take block number and start catching up
  ::
  ++  take-block-number
    |=  rep=response:rpc:jstd
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%take-block-number-error--retrying message.rep]
      get-latest-block
    =.  latest-block.eye  (parse-eth-block-number res.rep)
    (catch-up last-heard-block.eye)
  ::
  ::  +take-catch-up-step: process chunk
  ::
  ++  take-catch-up-step
    |=  [rep=response:rpc:jstd from-block=@ud next-block=@ud]
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%catch-up-step-error--retrying message.rep]
      (catch-up from-block)
    =.  +>.$  (take-events rep)
    (catch-up next-block)
  ::
  ::  +take-events: process events
  ::
  ++  take-events
    |=  rep=response:rpc:jstd
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?<  ?=(%error -.rep)
    ?.  ?=(%a -.res.rep)
      ~&  [%events-not-array rep]
      !!
    =*  changes  p.res.rep
    ~?  &(debug=| (gth (lent changes) 0))
      :*  %processing-changes
          changes=(lent changes)
          block=last-heard-block.eye
          id=filter-id.eye
      ==
    |-  ^+  +>.^$
    ?~  changes  +>.^$
    =.  +>.^$
      (take-event-log (parse-event-log i.changes))
    $(changes t.changes)
  ::
  ::  +take-event-log: obtain changes from event-log
  ::
  ++  take-event-log
    |=  log=event-log
    ^+  +>
    ?~  mined.log
      ~&  %ignoring-unmined-event
      +>
    =*  place  u.mined.log
    ?:  (~(has in heard.eye) block-number.place log-index.place)
      ?.  removed.u.mined.log
        ~?  debug=|
          [%ignoring-duplicate-event tx=transaction-hash.u.mined.log]
        +>
      ::  block was reorganized away, so rewind to this block and
      ::  start syncing again.
      ::
      ~&  :*  'removed event!  Perhaps chain has reorganized?'
              tx-hash=transaction-hash.u.mined.log
              block-number=block-number.u.mined.log
              block-hash=block-hash.u.mined.log
          ==
      %=    +>
          rewind-block
        :-  ~
        ?~  rewind-block
          block-number.place
        (min block-number.place u.rewind-block)
      ==
    =.  last-heard-block.eye
      (max block-number.place last-heard-block.eye)
    ?:  ?&  (gte block-number.place from-block.eye)
            ?|  ?=(~ to-block.eye)
                (lte block-number.place u.to-block.eye)
            ==
        ==
      (put-log log)
    ~&  :*  %event-block-out-of-range
            got=block-number.place
            from=from-block.eye
            to=to-block.eye
        ==
    +>.$
  ::
  ::  +restore-block: rewind to block or earlier
  ::
  ++  restore-block
    |=  block=@ud
    ^+  +>
    =/  old-qeu  snaps.sap.eye
    ::  clear history
    ::
    =:  snaps.sap.eye       ~
        count.sap.eye       0
        latest-block.sap.eye  0
      ==
    ::  find a snapshot we can use, remove ones that are too new
    ::
    =^  snap=snapshot  +>.$
      ?:  |(=(~ old-qeu) (lth block last-heard-block:(need ~(top to old-qeu))))
        [%*(. *snapshot last-heard-block from-block.eye) +>.$]
      |-  ^-  [snapshot _+>.^$]
      =^  snap=snapshot  old-qeu
        ~(get to old-qeu)
      =:  count.sap.eye       +(count.sap.eye)
          latest-block.sap.eye  last-heard-block.snap
          snaps.sap.eye       (~(put to snaps.sap.eye) snap)
        ==
      ?:  |(=(~ old-qeu) (lth block last-heard-block:(need ~(top to old-qeu))))
        [snap +>.^$]
      $
    ~&  [%restoring-block block last-heard-block.snap]
    (restore-snap snap)
  ::
  ::  +restore-snap: revert state to snapshot
  ::
  ++  restore-snap
    |=  snap=snapshot
    ^+  +>
    ::  notify subscribers
    ::TODO  be more nuanced about what changed, maybe
    ::
    =.  +>.$  (fan-diff snap+snap)
    ::  restore state and kick new fetch cycle
    ::
    %=    get-latest-block
        last-heard-block.eye  last-heard-block.snap
        heard.eye         heard.snap
        logs.eye          logs.snap
    ==
  --
--
