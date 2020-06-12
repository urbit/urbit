::  gaze: azimuth statistics
::
::    general flow:
::    - receive events
::    - process events whose timestamp is known
::    - request timestamps for unknown block numbers (if not already running)
::    - receive timestamps, process events
::
/-  eth-watcher
/+  default-agent, verb
=,  ethereum
=,  azimuth
::
=>  |%
    +$  state-0
      $:  %0
          ::  qued: event logs waiting on block timestamp, oldest first
          ::  time: timstamps of block numbers
          ::  seen: events sorted by timestamp, newest first
          ::  days: stats by day, newest first
          ::
          running=(unit @ta)
          qued=loglist
          time=(map @ud @da)
          seen=(list [wen=@da wat=event])
          days=(list [day=@da sat=stats])
      ==
    ::
    +$  loglist  loglist:eth-watcher
    +$  event
      $%  [%azimuth who=ship dif=diff-point]
          [%invite by=ship of=ship gift=ship to=address]
      ==
    ::
    +$  stats
      $:  spawned=(list @p)
          activated=(list @p)
          transfer-p=(list @p)
          transferred=(list @p)
          configured=(list @p)
          breached=(list @p)
          request=(list @p)
          sponsor=(list @p)
          management-p=(list @p)
          voting-p=(list @p)
          spawn-p=(list @p)
          invites-senders=(list @p)
      ==
    ::
    +$  card  card:agent:gall
    ::
    ++  node-url  'http://eth-mainnet.urbit.org:8545'
    ++  refresh-rate  ~h1
    ++  timeout-time  ~h2
    --
::
=|  state-0
=*  state  -
::
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    [setup-cards:do this]
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  ?=(%noun mark)
    =/  =noun  !<(noun vase)
    |-  ^-  [cards=(list card) =_this]
    ?+  noun  ~|([dap.bowl %unknown-poke noun] !!)
        %reconnect
      :_  this
      :~  leave-eth-watcher:do
          watch-eth-watcher:do
      ==
    ::
        %reload
      :-  cards:$(noun %reconnect)
      this(qued ~, seen ~, days ~)
    ::
        %rewatch
      :_  this:$(noun %reset)
      :~  leave-eth-watcher:do
          clear-eth-watcher:do
          setup-eth-watcher:do
          await-eth-watcher:do
      ==
    ::
        %export
      [export:do this]
    ::
        %debug
      ~&  latest=(turn (scag 5 seen) head)
      ~&  oldest=(turn (slag (sub (max 5 (lent seen)) 5) seen) head)
      ~&  :-  'order is'
          =-  ?:(sane 'sane' 'insane')
          %+  roll  seen
          |=  [[this=@da *] last=@da sane=?]
          :-  this
          ?:  =(*@da last)  &
          (lte this last)
      ~&  time=~(wyt by time)
      ~&  qued=(lent qued)
      ~&  days=(lent days)
      [~ this]
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %kick
      ?.  =(/watcher wire)  [~ this]
      [[watch-eth-watcher:do]~ this]
    ::
        %fact
      ?+  wire  (on-agent:def wire sign)
          [%watcher ~]
        ?.  ?=(%eth-watcher-diff p.cage.sign)
          (on-agent:def wire sign)
        =^  cards  state
          %-  handle-eth-watcher-diff:do
          !<(diff:eth-watcher q.cage.sign)
        [cards this]
      ::
          [%timestamps @ ~]
        ?+  p.cage.sign  (on-agent:def wire sign)
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          =/  =tank  leaf+"{(trip dap.bowl)} thread failed; will retry"
          %-  (slog tank leaf+<term> tang)
          =^  cards  state
            request-timestamps:do
          [cards this]
        ::
            %thread-done
          =^  cards  state
            %-  save-timestamps:do
            !<((list [@ud @da]) q.cage.sign)
          [cards this]
        ==
      ==
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  +<.sign-arvo  ~|([dap.bowl %strange-arvo-sign +<.sign-arvo] !!)
        %wake
      ?:  =(/export wire)
        [[wait-export:do export:do] this]
      ?:  =(/watch wire)
        [[watch-eth-watcher:do]~ this]
      ~&  [dap.bowl %strange-wake wire]
      [~ this]
    ==
  ::
  ++  on-peek   on-peek:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  setup-cards
  ^-  (list card)
  :~  wait-export
      setup-eth-watcher
      ::  we punt on subscribing to the eth-watcher for a little while.
      ::  this way we get a %history diff containing all past events,
      ::  instead of so many individual %log diffs that we bail meme.
      ::  (to repro, replace this with `watch-eth-watcher`)
      ::
      await-eth-watcher
  ==
::
++  wait
  |=  [=wire =@dr]
  ^-  card
  [%pass wire %arvo %b %wait (add now.bowl dr)]
::
++  wait-export  (wait /export refresh-rate)
::
++  to-eth-watcher
  |=  [=wire =task:agent:gall]
  ^-  card
  [%pass wire %agent [our.bowl %eth-watcher] task]
::
++  setup-eth-watcher
  %+  to-eth-watcher  /setup
  :+  %poke   %eth-watcher-poke
  !>  ^-  poke:eth-watcher
  :+  %watch  /[dap.bowl]
  :*  node-url
      |
      refresh-rate
      timeout-time
      public:mainnet-contracts
      ~[azimuth delegated-sending]:mainnet-contracts
      ~
  ==
::
::  see also comment in +setup-cards
++  await-eth-watcher  (wait /watch ~m30)
::
++  watch-eth-watcher
  %+  to-eth-watcher  /watcher
  [%watch /logs/[dap.bowl]]
::
++  leave-eth-watcher
  %+  to-eth-watcher  /watcher
  [%leave ~]
::
++  clear-eth-watcher
  %+  to-eth-watcher  /clear
  :+  %poke  %eth-watcher-poke
  !>  ^-  poke:eth-watcher
  [%clear /logs/[dap.bowl]]
::
++  poke-spider
  |=  [=wire =cage]
  ^-  card
  [%pass wire %agent [our.bowl %spider] %poke cage]
::
++  watch-spider
  |=  [=wire =sub=path]
  ^-  card
  [%pass wire %agent [our.bowl %spider] %watch sub-path]
::
::  +handle-eth-watcher-diff: process new logs, clear state on rollback
::
::    processes logs for which we know the timestamp
::    adds timestamp-less logs to queue
::
++  handle-eth-watcher-diff
  |=  =diff:eth-watcher
  ^-  (quip card _state)
  =^  logs  state
    ^-  [loglist _state]
    ?-  -.diff
      %history  ~&  [%got-history (lent loglist.diff)]
                [loglist.diff state(qued ~, seen ~)]
      %log      ~&  %got-log
                [[event-log.diff ~] state]
      %disavow  ~&  %disavow-unimplemented
                [~ state]
    ==
  %-  process-logs
  %+  skip  logs
  |=  =event-log:rpc
  %-  is-lockup-block
  block-number:(need mined.event-log)
::
::  +is-lockup-block: whether the block contains lockup/ignorable transactions
::
::    this is the stupid dumb equivalent to actually identifying lockup
::    transactions procedurally, which is still in git history, but didn't
::    work quite right for unidentified reasons
::
++  is-lockup-block
  |=  num=@ud
  ^-  ?
  %+  roll
    ^-  (list [@ud @ud])
    :~  [7.050.978 7.051.038]
    ==
  |=  [[start=@ud end=@ud] in=_|]
  ?:  in  &
  &((gte num start) (lte num end))
::
::  +request-timestamps: request block timestamps for the logs as necessary
::
::    will come back as a thread result
::
++  request-timestamps
  ^-  (quip card _state)
  ?~  qued  [~ state]
  ?^  running  [~ state]
  =/  tid=@ta
    %+  scot  %ta
    :((cury cat 3) dap.bowl '_' (scot %uv eny.bowl))
  :_  state(running `tid)
  :~  (watch-spider /timestamps/[tid] /thread-result/[tid])
    ::
      %+  poke-spider  /timestamps/[tid]
      :-  %spider-start
      =-  !>([~ `tid %eth-get-timestamps -])
      !>  ^-  [@t (list @ud)]
      :-  node-url
      =-  ~(tap in -)
      %-  ~(gas in *(set @ud))
      ^-  (list @ud)
      %+  turn  qued
      |=  log=event-log:rpc
      block-number:(need mined.log)
  ==
::
::  +save-timestamps: store timestamps into state
::
++  save-timestamps
  |=  timestamps=(list [@ud @da])
  ^-  (quip card _state)
  =.  time     (~(gas by time) timestamps)
  =.  running   ~
  (process-logs ~)
::
::  +process-logs: handle new incoming logs
::
++  process-logs
  |=  new=loglist  ::  oldest first
  ^-  (quip card _state)
  =.  qued  (weld qued new)
  ?~  qued  [~ state]
  =-  %_  request-timestamps
        qued  (flop rest)  ::  oldest first
        seen  (weld logs seen)  ::  newest first
        days  (count-events (flop logs))  ::  oldest first
      ==
  %+  roll  `loglist`qued
  |=  [log=event-log:rpc [rest=loglist logs=(list [wen=@da wat=event])]]
  ::  to ensure logs are processed in sane order,
  ::  stop processing as soon as we skipped one
  ::
  ?^  rest  [[log rest] logs]
  =/  tim=(unit @da)
    %-  ~(get by time)
    block-number:(need mined.log)
  ?~  tim  [[log rest] logs]
  :-  rest
  =+  ven=(event-log-to-event log)
  ?~  ven  logs
  [[u.tim u.ven] logs]
::
::  +event-log-to-event: turn raw log into gaze noun
::
++  event-log-to-event
  |=  log=event-log:rpc
  ^-  (unit event)
  ?:  =(azimuth:mainnet-contracts address.log)
    =+  (event-log-to-point-diff log)
    ?~  -  ~
    `azimuth+u
  ?:  =(delegated-sending:mainnet-contracts address.log)
    ?.  .=  i.topics.log
        0x4763.8e3c.ddee.2204.81e4.c3f9.183d.639c.
          0efe.a7f0.5fcd.2df4.1888.5572.9f71.5419
      ~
    =/  [of=@ pool=@]
      ~|  t.topics.log
      %+  decode-topics:abi:ethereum  t.topics.log
      ~[%uint %uint]
    =/  [by=@ gift=@ to=@]
      ~|  data.log
      %+  decode-topics:abi:ethereum
        %+  rash  data.log
        =-  ;~(pfix (jest '0x') -)
        %+  stun  [3 3]
        (bass 16 (stun [64 64] hit))
      ~[%uint %uint %address]
    `invite+[by of gift to]
  ~
::
::  +count-events: add events to the daily stats
::
++  count-events
  |=  logs=_seen  ::  oldest first
  ^+  days
  =/  head=[day=@da sat=stats]
    ?^  days  i.days
    *[@da stats]
  =+  tail=?~(days ~ t.days)
  |-
  ::  when done, store updated head, but only if it's set
  ::
  ?~  logs
    ?:  =(*[@da stats] head)  tail
    [head tail]
  =*  log  i.logs
  ::  calculate day for current event, set head if unset
  ::
  =/  day=@da
    (sub wen.log (mod wen.log ~d1))
  =?  day.head  =(*@da day.head)  day
  ::  same day as head, so add to it
  ::
  ?:  =(day day.head)
    %_  $
      sat.head  (count-event wat.log sat.head)
      logs      t.logs
    ==
  ~|  [%weird-new-day old=day.head new=day]
  ?>  (gth day day.head)
  ::  newer day than head of days, so start new head
  ::
  %_  $
    tail  [head tail]
    head  [day *stats]
  ==
::
::  +count-event: add event to the stats, if it's relevant
::
++  count-event
  |=  [eve=event sat=stats]
  ^-  stats
  ?-  -.eve
    %invite  sat(invites-senders [by.eve invites-senders.sat])
  ::
      %azimuth
    ?+  -.dif.eve  sat
      %spawned           sat(spawned [who.dif.eve spawned.sat])
      %activated         sat(activated [who.eve activated.sat])
      %transfer-proxy    ?:  =(0x0 new.dif.eve)  sat
                         sat(transfer-p [who.eve transfer-p.sat])
      %owner             sat(transferred [who.eve transferred.sat])
      %keys              sat(configured [who.eve configured.sat])
      %continuity        sat(breached [who.eve breached.sat])
      %escape            ?~  new.dif.eve  sat
                         sat(request [who.eve request.sat])
      %sponsor           ?.  has.new.dif.eve  sat
                         sat(sponsor [who.eve sponsor.sat])
      %management-proxy  sat(management-p [who.eve management-p.sat])
      %voting-proxy      sat(voting-p [who.eve voting-p.sat])
      %spawn-proxy       sat(spawn-p [who.eve spawn-p.sat])
    ==
  ==
::
::
::  +export: periodically export data
::
++  export
  ^-  (list card)
  :~  (export-move %days (export-days days))
      (export-move %months (export-months days))
      (export-move %events export-raw)
  ==
::
::  +export-move: %info move to write exported .txt
::
++  export-move
  |=  [nom=@t dat=(list @t)]
  ^-  card
  =-  [%pass /export/[nom] %arvo %c %info -]
  %+  foal:space:userlib
    /(scot %p our.bowl)/home/(scot %da now.bowl)/gaze-exports/[nom]/txt
  [%txt !>(dat)]
::
::  +peek-x: accept gall scry
::
::    %/days/txt:   per day, digest stats
::    %/months/txt: per month, digest stats
::    %/raw/txt:    all observed events
::
++  peek-x  ::TODO
  |=  pax=path
  ^-  (unit (unit (pair mark *)))
  ?~  pax  ~
  ?:  =(%days i.pax)
    :^  ~  ~  %txt
    (export-days days)
  ?:  =(%months i.pax)
    :^  ~  ~  %txt
    (export-months days)
  ?:  =(%raw i.pax)
    ``txt+export-raw
  ~
::
::  +export-months: generate a csv of stats per month
::
++  export-months
  |=  =_days
  %-  export-days
  ^+  days
  %+  roll  (flop days)
  |=  [[day=@da sat=stats] mos=(list [mod=@da sat=stats])]
  ^+  mos
  =/  mod=@da
    %-  year
    =+  (yore day)
    -(d.t 1)
  ?~  mos  [mod sat]~
  ?:  !=(mod mod.i.mos)
    [[mod sat] mos]
  :_  t.mos
  :-  mod
  ::TODO  this is hideous. can we make a wet gate do this?
  :*  (weld spawned.sat spawned.sat.i.mos)
      (weld activated.sat activated.sat.i.mos)
      (weld transfer-p.sat transfer-p.sat.i.mos)
      (weld transferred.sat transferred.sat.i.mos)
      (weld configured.sat configured.sat.i.mos)
      (weld breached.sat breached.sat.i.mos)
      (weld request.sat request.sat.i.mos)
      (weld sponsor.sat sponsor.sat.i.mos)
      (weld management-p.sat management-p.sat.i.mos)
      (weld voting-p.sat voting-p.sat.i.mos)
      (weld spawn-p.sat spawn-p.sat.i.mos)
      (weld invites-senders.sat invites-senders.sat.i.mos)
  ==
::
::  +export-days: generate a csv of stats per day
::
++  export-days
  |=  =_days
  :-  %-  crip
      ;:  weld
        "date,"
        "spawned,"
        "activated,"
        "transfer proxy,"
        "transferred,"
        "transferred (unique),"
        "configured,"
        "configured (unique),"
        "escape request,"
        "sponsor change,"
        "invites,"
        "invites (unique senders)"
      ==
  |^  ^-  (list @t)
      %+  turn  days
      |=  [day=@da stats]
      %-  crip
      ;:  weld
        (scow %da day)            ","
        (count spawned)           ","
        (count activated)         ","
        (count transfer-p)        ","
        (unique transferred)      ","
        (unique configured)       ","
        (count request)           ","
        (count sponsor)           ","
        (unique invites-senders)
      ==
  ::
  ++  count
    |*  l=(list)
    (num (lent l))
  ::
  ++  unique
    |*  l=(list)
    ;:  weld
      (count l)
      ","
      (num ~(wyt in (~(gas in *(set)) l)))
    ==
  ::
  ++  num  (d-co:co 1)
  --
::
::  +export-raw: generate a csv of individual transactions
::
++  export-raw
  :-  %-  crip
      ;:  weld
        "date,"
        "point,"
        "event,"
        "field 1,field2,field3"
      ==
  |^  ^-  (list @t)
      %+  turn  seen
      :: (cork tail event-to-row crip)
      |=  [wen=@da =event]
      (crip "{(scow %da wen)},{(event-to-row event)}")
  ::
  ++  event-to-row
    |=  =event
    ?-  -.event
      %azimuth  (point-diff-to-row +.event)
      %invite   (invite-to-row +.event)
    ==
  ::
  ++  point-diff-to-row
    |=  [who=ship dif=diff-point]
    ^-  tape
    %+  weld  "{(pon who)},"
    ?-  -.dif
      %full               "full,"
      %owner              "owner,{(adr new.dif)}"
      %activated          "activated,"
      %spawned            "spawned,{(pon who.dif)}"
      %keys               "keys,{(num life.dif)}"
      %continuity         "breached,{(num new.dif)}"
      %sponsor            "sponsor,{(spo has.new.dif)},{(pon who.new.dif)}"
      %escape             "escape-req,{(req new.dif)}"
      %management-proxy   "management-p,{(adr new.dif)}"
      %voting-proxy       "voting-p,{(adr new.dif)}"
      %spawn-proxy        "spawn-p,{(adr new.dif)}"
      %transfer-proxy     "transfer-p,{(adr new.dif)}"
    ==
  ::
  ++  invite-to-row
    |=  [by=ship of=ship ship to=address]
    "{(pon by)},invite,{(pon of)},{(adr to)}"
  ::
  ++  num  (d-co:co 1)
  ++  pon  (cury scow %p)
  ++  adr  |=(a=@ ['0' 'x' ((x-co:co (mul 2 20)) a)])
  ++  spo  |=(h=? ?:(h "escaped to" "detached from"))
  ++  req  |=(r=(unit @p) ?~(r "canceled" (pon u.r)))
  --
--
