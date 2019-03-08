::  gaze: azimuth statistics
::
/+  *eth-watcher
::
=,  ethereum
=,  azimuth
::
|%
++  state
  $:  ::  qued: event logs waiting on block timestamp, oldest first
      ::  time: timstamps of block numbers
      ::  seen: events sorted by timestamp, newest first
      ::  days: stats by day, newest first
      ::
      qued=loglist
      time=(map @ud @da)
      seen=(list [wen=@da wat=event])
      days=(list [day=@da sat=stats])
  ==
::
++  event
  $%  [%azimuth who=ship dif=diff-point]
      ::TODO  [%invites *]
  ==
::
++  stats
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
  ==
::
::
++  move  (pair bone card)
++  card
  $%  [%poke wire [ship %eth-watcher] %eth-watcher-action action]
      [%peer wire [ship %eth-watcher] path]
      [%hiss wire (unit user:eyre) mark %hiss hiss:eyre]
      [%wait wire @da]
      [%info wire desk nori:clay]
  ==
--
::
|_  [bowl:gall state]
++  node-url  (need (de-purl:html 'http://eth-mainnet.urbit.org:8545'))
++  export-frequency  ~h1
::
++  prep
  |=  old=(unit state)
  ?~  old
    :_  ..prep
    [ost %wait /export (add now export-frequency)]~
  [~ ..prep(+<+ u.old)]
::
::  +poke-noun: do a thing
::
::    %kick-watcher:  reset, tell %eth-watcher to look for events for us
::    %regaze:        reset (but keep timestamps), subscribe to eth-watcher
::    %debug:         print debug info
::
++  poke-noun
  |=  a=?(%kick-watcher %regaze %debug)
  ^-  (quip move _+>)
  ?-  a
      %kick-watcher
    :_  +>.$(qued ~, seen ~, days ~, time ~)
    :~
      :-  ost
      :*  %poke
          /look
          [our %eth-watcher]
          %eth-watcher-action
        ::
          ^-  action
          :+  %watch  dap
          :*  node-url
              public:contracts
              ~
              ~[azimuth:contracts]
              ~
          ==
      ==
    ==
  ::
      %regaze
    :_  +>.$(qued ~, seen ~, days ~)
    :~
      :-  ost
      :*  %peer
          /look
          [our %eth-watcher]
          /[dap]
      ==
    ==
  ::
      %debug
    ~&  latest=(turn (scag 5 seen) head)
    ~&  oldest=(turn (slag (sub (lent seen) 5) seen) head)
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
    [~ +>.$]
  ==
::
::  +diff-eth-watcher-update: process new logs, clear state on rollback
::
++  diff-eth-watcher-update
  |=  [=wire =^update]
  ^-  (quip move _+>)
  =^  logs  +>.$
    ?-  -.update
      %snap  ~&  [%got-snap (lent logs.snapshot.update)]
             [logs.snapshot.update +>.$(qued ~, seen ~)]
      %logs  ~&  [%got-logs (lent loglist.update)]
             [loglist.update +>.$]
    ==
  ?~  logs  [~ +>.$]
  =-  =^  moz  +>.$  (queue-logs mistime)  ::  oldest first
      =.  +>.$  (process-logs havtime)  ::  oldest first
      [moz +>.$]
  ::  sort based on timstamp known, throw out lockup logs
  ::
  %+  roll  `loglist`logs
  |=  [log=event-log:rpc havtime=loglist mistime=loglist]
  ^+  [havtime mistime]
  =+  bon=block-number:(need mined.log)
  ?:  (is-lockup-block bon)  [havtime mistime]
  ?:  (~(has by time) bon)
    [[log havtime] mistime]
  [havtime [log mistime]]
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
::  +queue-logs: hold on to new logs, requesting timestamps for them
::
++  queue-logs
  |=  logs=loglist  ::  oldest first
  ^-  (quip move _+>)
  ?~  logs  [~ +>]
  :-  [(request-timestamps logs) ~]
  +>(qued (weld qued logs))
::
::  +request-timestamps: request block timestamps for the logs as necessary
::
++  request-timestamps
  |=  logs=loglist
  ^-  move
  =-  [ost %hiss /timestamps ~ %json-rpc-response %hiss -]
  ^-  hiss:eyre
  %+  json-request:rpc  node-url
  :-  %a
  ^-  (list json)
  %+  turn
    ^-  (list @ud)
    =-  ~(tap in -)
    %-  ~(gas in *(set @ud))
    ^-  (list @ud)
    %+  turn  logs
    |=  log=event-log:rpc
    block-number:(need mined.log)
  |=  num=@ud
  ^-  json
  ~!  *request:rpc
  %+  request-to-json:rpc
    `(scot %ud num)
  [%eth-get-block-by-number num |]
::
::  +sigh-json-rpc-response: get block details, extract timestamps
::
++  sigh-json-rpc-response
  |=  [=wire =response:rpc:jstd]
  ^-  (quip move _+>)
  ?>  ?=([%timestamps ~] wire)
  ?:  ?=(?(%error %fail) -.response)
    ~?  ?=(%error -.response)  [%rpc-error +.response]
    ~?  ?=(%fail -.response)   [%httr-fail hit.response]
    ~&  %retrying-timestamps
    [[(request-timestamps qued) ~] +>]
  ?>  ?=(%batch -.response)
  =-  [~ (process-logs(time -, qued ~) qued)]
  %-  ~(gas by time)
  =/  max=@ud
    (roll ~(tap in ~(key by time)) max)
  ::  for every result, get the block number and timestamp
  ::
  %+  turn  bas.response
  |=  res=response:rpc:jstd
  ^-  (pair @ud @da)
  ~|  res
  ?>  ?=(%result -.res)
  ~|  id.res
  :-  (slav %ud id.res)
  %-  from-unix:chrono:userlib
  %-  parse-hex-result:rpc
  ?>  ?=(%o -.res.res)
  (~(got by p.res.res) 'timestamp')
::
::  +process logs that are in the queue
::
++  process-logs
  |=  logs=loglist  ::  oldest first
  ^+  +>
  ?~  logs  +>
  =-  %_  +>.$
        qued  (flop rest)  ::  oldest first
        seen  (weld logs seen)  ::  newest first
        days  (count-events (flop logs))  ::  oldest first
      ==
  %+  roll  `loglist`logs
  |=  [log=event-log:rpc rest=loglist logs=(list [wen=@da wat=event])]
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
  ?:  =(azimuth:contracts address.log)
    =+  (event-log-to-point-diff log)
    ?~  -  ~
    `azimuth+u
  ::TODO  delegated sending support
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
  ?>  ?=(%azimuth -.eve)
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
::
::
::  +wake-export: periodically export data
::
++  wake-export
  |=  [=wire ~]
  ^-  (quip move _+>)
  :_  +>
  :~  [ost %wait /export (add now export-frequency)]
      (export-move %days (export-days days))
      (export-move %months (export-months days))
      (export-move %events export-raw)
  ==
::
::  +export-move: %info move to write exported .txt
::
++  export-move
  |=  [nom=@t dat=(list @t)]
  ^-  move
  :^  ost  %info  /export/[nom]
  %+  foal:space:userlib
    /(scot %p our)/home/(scot %da now)/gaze-exports/[nom]/txt
  [%txt !>(dat)]
::
::  +peek-x: accept gall scry
::
::    %/days/txt:   per day, digest stats
::    %/months/txt: per month, digest stats
::    %/raw/txt:    all observed events
::
++  peek-x
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
        "sponsor change"
      ==
  |^  ^-  (list @t)
      %+  turn  days
      |=  [day=@da stats]
      %-  crip
      ;:  weld
        (scow %da day)        ","
        (count spawned)       ","
        (count activated)     ","
        (count transfer-p)    ","
        (unique transferred)  ","
        (unique configured)   ","
        (count request)       ","
        (count sponsor)
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
        "field 1"
      ==
  |^  ^-  (list @t)
      %+  turn  seen
      |=  [wen=@da wat=event]
      %-  crip
      ;:  weld
        (scow %da wen)  ","
        (pon who.wat)   ","
        (point-diff-to-row dif.wat)
      ==
  ::
  ++  point-diff-to-row
    |=  dif=diff-point
    ?-  -.dif
      %full               "full,"
      %owner              "owner,{(adr new.dif)}"
      %activated          "activated,"
      %spawned            "spawned,{(pon who.dif)}"
      %keys               "keys,{(num life.dif)}"
      %continuity         "breached,{(num new.dif)}"
      %sponsor            "sponsor,{(spo has.new.dif)} {(pon who.new.dif)}"
      %escape             "escape-req,{(req new.dif)}"
      %management-proxy   "management-p,{(adr new.dif)}"
      %voting-proxy       "voting-p,{(adr new.dif)}"
      %spawn-proxy        "spawn-p,{(adr new.dif)}"
      %transfer-proxy     "transfer-p,{(adr new.dif)}"
    ==
  ::
  ++  num  (d-co:co 1)
  ++  pon  (cury scow %p)
  ++  adr  ['0' 'x' (x-co:co 20)]
  ++  spo  |=(h=? ?:(h "escaped to" "detached from"))
  ++  req  |=(r=(unit @p) ?~(r "canceled" (pon u.r)))
  --
--
