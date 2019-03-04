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
      ::TODO  do we actually gain anything by oldest first?
      ::      it's baked into the logic now...
      ::  time: timstamps of block numbers
      ::  seen: events sorted by timestamp
      ::
      qued=loglist
      time=(map @ud @da)
      seen=(list [wen=@da wat=event])
  ==
::
++  event
  $%  [%azimuth who=ship dif=diff-point]
      ::TODO  [%invites *]
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
  |=  old=(unit state)
  ?~  old
    [~ ..prep]
  [~ ..prep(+<+ u.old)]
::
++  poke-noun
  |=  a=?(%kick-watcher %regaze %simple %debug)
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
      %simple
    =/  loz=loglist
      .^(loglist %gx /(scot %p our)/eth-watcher/(scot %da now)/[dap]/noun)
    ::  lockup: ships that went into lockup
    ::
    =/  lockup=(set ship)
      %-  ~(gas in *(set ship))
      %+  murn  loz
      |=  log=event-log:rpc
      ^-  (unit ship)
      =+  dif=(event-log-to-point-diff log)
      ?~  dif  ~
      =*  diz  q.u.dif
      ?.  ?=(%owner -.diz)  ~
      ?:  =(linear-star-release:contracts new.diz)  `p.u.dif
      ?:  =(conditional-star-release:contracts new.diz)  `p.u.dif
      ~
    ::
    =+  activated=(filter loz activated:azimuth-events lockup)
    =+  spawned=(filter loz spawned:azimuth-events lockup)
    =+  transfer=(filter loz owner-changed:azimuth-events lockup)
    =+  transfer-u=(~(gas in *(set ship)) transfer)
    =+  rekeyed=(filter loz changed-keys:azimuth-events lockup)
    =+  rekeyed-u=(~(gas in *(set ship)) rekeyed)
    ~&  ;:  weld
          "Since launch, there have been: "
          "- {<(lent activated)>} points activated "
          "(out of {<(lent spawned)>} spawned), "
          "- {<(sub (sub (lent transfer) (lent spawned)) (lent activated))>} point transfers "
          "({<(sub ~(wyt in transfer-u) (lent spawned))>} unique), "
          "- {<(lent rekeyed)>} key configurations "
          "({<~(wyt in rekeyed-u)>} unique)."
        ==
    [~ +>.$]
  ::
      %debug
    ~&  latest=(turn (scag 10 seen) head)
    ~&  oldest=(turn (slag (sub (lent seen) 10) seen) head)
    ~&  time=~(wyt by time)
    ~&  qued=(lent qued)
    [~ +>.$]
  ==
::
::  +filter: find ships that were the subject of some :event
::
++  filter
  |=  [logs=loglist event=@ux exclude=(set ship)]
  %+  murn  logs
  |=  log=event-log:rpc
  ^-  (unit ship)
  ?.  =(event i.topics.log)  ~
  =+  dif=(event-log-to-point-diff log)
  ?~  dif  ~
  =/  who
    ?:  ?=(%spawned -.q.u.dif)  who.q.u.dif
    p.u.dif
  ?:  (~(has in exclude) who)  ~
  `who
::
::  +locked: set of galaxies whose stars got locked up
::
++  locked
  %-  ~(gas in *(set ship))
  :~  ~sev
      ~wes
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
  =-  =^  moz  +>.$  (queue-logs mistime)
      [moz (process-logs havtime)]
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
  =-  +>.$(qued (flop rest), seen (weld (flop logs) seen))
  %+  roll  logs
  |=  [log=event-log:rpc rest=loglist logs=(list [@da event])]
  =/  tim=(unit @da)
    %-  ~(get by time)
    block-number:(need mined.log)
  ?~  tim  [[log rest] logs]
  :-  rest
  =+  ven=(event-log-to-event log)
  ?~  ven  logs
  [[u.tim u.ven] logs]
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
    =*  who=@p  p.u
    =*  dif=diff-point  q.u  ::TODO  want to use below, but mint-cove
    ::  ignore spawning of locked up stars
    ::
    ?|  ?&  (~(has in locked) who)
            ?=(%spawned -.q.u)
        ==
      ::
        ::  ignore spawn-transfer events of locked up stars
        ::
        ?&  (~(has in locked) (^sein:title who))
          ::
            ?|  ?=(%activated -.q.u)
              ::
                ?&  ?=(%owner -.q.u)
                  ::
                    ?|  =(new.q.u linear-star-release:contracts)
                        =(new.q.u conditional-star-release:contracts)
                    ==
                ==
            ==
        ==
    ==
  ::TODO  delegated sending support
  ~
::
::  +export: generate a csv with per-period
::
++  export
  ~
--
