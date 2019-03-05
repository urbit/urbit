::  gaze: azimuth analytics
::
::TODO  daily stats, but also sane longer-period stats
::      probably convert block numbers to day, then store difs by day
::      every day, week, and month, append entry to csv file with stat counters
::  not on a timer, but whenever we see a block belogns to the next day/week etc
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
      ::  lock: when ships got locked up
      ::
      qued=loglist
      time=(map @ud @da)
      seen=(list [wen=@da wat=event])
      days=(list [day=@da sat=stats])
      lock=(map @p @da)
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
  $%  [%hiss wire (unit user:eyre) mark %hiss hiss:eyre]
      ::  [%wait wire @da]
      ::  [%rest @da]
      [%poke wire [ship %eth-watcher] %eth-watcher-action action]
      [%peer wire [ship %eth-watcher] path]
  ==
--
::
|_  [bowl:gall state]
++  node-url  (need (de-purl:html 'http://eth-mainnet.urbit.org:8545'))
::
++  prep
  |=  old=(unit *) ::state)
  :: ?~  old
    [~ ..prep]
  :: [~ ..prep(+<+ u.old)]
::
++  poke-noun
  |=  a=?(%kick-watcher %regaze %debug)
  ^-  (quip move _+>)
  ?-  a
      %kick-watcher
    :_  +>.$
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
    :_  +>.$(qued ~, seen ~)
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
    ~&  latest=(turn (scag 10 seen) head)
    ~&  oldest=(turn (slag (sub (lent seen) 10) seen) head)
    ~&  :-  'order is'
        =-  ?:(sane 'sane' 'insane')
        %+  roll  seen
        |=  [[this=@da *] last=@da sane=?]
        :-  this
        ?:  =(*@da last)  &
        (lte this last)
    ~&  time=~(wyt by time)
    ~&  qued=(lent qued)
    ~&  days=(scag 5 days)
    [~ +>.$]
  ==
::
++  peek-x
  |=  pax=path
  ^-  (unit (unit [mark *]))
  ?~  pax  ~
  ?.  =(%days i.pax)  ~
  :^  ~  ~  %txt
  export
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
  =-  =^  moz  +>.$  (queue-logs mistime)
      =.  +>.$  (process-logs havtime)
      [moz +>.$]
  ^-  [havtime=loglist mistime=loglist]
  %+  skid  `loglist`logs
  |=  log=event-log:rpc
  %-  ~(has by time)
  block-number:(need mined.log)
::
::  +queue-logs: hold on to new logs, requesting timestamps for them
::
++  queue-logs
  |=  logs=loglist
  ^-  (quip move _+>)
  ?~  logs  [~ +>]
  :-  [(request-timestamps logs) ~]
  +>(qued (weld qued (flop logs)))
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
  ::  for every result, get the block number and timestamp
  ::
  ~&  [%got-times (lent bas.response)]
  ~&  [%still-in-queue (lent qued)]
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
  |=  logs=loglist
  ^+  +>
  =-  ~&  [%processed (lent -)]
      %_  +>.$
        seen  (weld (flop -) seen)
        days  (count-events (flop -))
      ==
  ~&  [%processing (lent logs)]
  %+  roll  logs
  |=  [log=event-log:rpc logs=(list [wen=@da wat=event])]
  =/  tim=@da
    %-  ~(got by time)
    block-number:(need mined.log)
  =+  ven=(event-log-to-event log)
  ?~  ven  logs
  [[tim u.ven] logs]
::
++  event-log-to-event
  |=  log=event-log:rpc
  ^-  (unit event)
  ?:  =(azimuth:contracts address.log)
    =+  (event-log-to-point-diff log)
    ?~  -  ~
    ::  ignore initial events for locked up stars
    ::
    ::TODO  do this filtering earlier, so we don't ask for unnecessary blocks
    =;  ignore=?  ?:(ignore ~ `azimuth+u)
    ::TODO  check against lock map
    |
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
::  +find-lockups: search the seen event log for lockup events
::
::    lockup events are identified by a transfer to either the linear or
::    conditional star release contract. a timestamp of the lockup transfer
::    is saved so that we can discard all events prior to it.
::
++  find-lockups
  ~  ::TODO
::
::  +export: generate a csv of stats per day
::
++  export
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
--
