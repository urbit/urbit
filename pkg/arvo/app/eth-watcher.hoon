::  eth-watcher: ethereum event log collector
::
/-  *eth-watcher, spider
/+  ethereum, default-agent, verb, dbug
=,  ethereum-types
=,  jael
::
=>  |%
    +$  card  card:agent:gall
    +$  app-state
      $:  %6
          dogs=(map path watchdog)
      ==
    ::
    +$  context  [=path dog=watchdog]
    +$  watchdog
      $:  config
          running=(unit [since=@da =tid:spider])
          =number:block
          =pending-logs
          =history
          blocks=(list block)
      ==
    ::
    ::  history: newest block first, oldest event first
    +$  history  (list loglist)
    --
::
::  Helpers
::
=>  |%
    ++  wait
      |=  [=path now=@da time=@dr]
      ^-  card
      [%pass [%timer path] %arvo %b %wait (add now time)]
    ::
    ++  wait-shortcut
      |=  [=path now=@da]
      ^-  card
      [%pass [%timer path] %arvo %b %wait now]
    ::
    ++  poke-spider
      |=  [=path our=@p =cage]
      ^-  card
      [%pass [%running path] %agent [our %spider] %poke cage]
    ::
    ++  watch-spider
      |=  [=path our=@p =sub=path]
      ^-  card
      [%pass [%running path] %agent [our %spider] %watch sub-path]
    ::
    ++  leave-spider
      |=  [=path our=@p]
      ^-  card
      [%pass [%running path] %agent [our %spider] %leave ~]
    --
::
::  Main
::
%-  agent:dbug
^-  agent:gall
=|  state=app-state
%+  verb  |
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    bec   byk.bowl(r da+now.bowl)
::
++  on-init
  ^-  (quip card _this)
  [~ this]
::
++  on-save   !>(state)
++  on-load
  |=  old=vase
  |^
  =+  !<(old-state=app-states old)
  =?  old-state  ?=(%0 -.old-state)
    %-  (slog leaf+"upgrading eth-watcher from %0" ~)
    ^-  app-state-1
    %=    old-state
        -  %1
        dogs
      %-  ~(run by dogs.old-state)
      |=  dog=watchdog-0
      %=  dog
        ->  [~m5 ->.dog]
      ==
    ==
  ::
  =^  cards-1=(list card)  old-state
    ?.  ?=(%1 -.old-state)
      `old-state
    %-  (slog leaf+"upgrading eth-watcher from %1" ~)
    :_  old-state(- %2)
    %+  turn  ~(tap by dogs.old-state)
    |=  [=path dog=watchdog-1]
    (wait-shortcut path now.bowl)
  ::
  =?  old-state  ?=(%2 -.old-state)
    %-  (slog leaf+"upgrading eth-watcher from %2" ~)
    ^-  app-state-3
    %=    old-state
        -  %3
        dogs
      %-  ~(run by dogs.old-state)
      |=  dog=watchdog-1
      %=  dog
        ->  [| ->.dog]
      ==
    ==
  ::
  =?  old-state  ?=(%3 -.old-state)
    %-  (slog leaf+"upgrading eth-watcher from %3" ~)
    ^-  app-state-4
    %=    old-state
        -  %4
        dogs
      %-  ~(run by dogs.old-state)
      |=  dog=watchdog-3
      ^-  watchdog-4
      %=  dog
          -
        ^-  config-4
        =,  -.dog
        [url eager refresh-rate (mul refresh-rate 6) from contracts topics]
      ::
          running
        ?~  running.dog  ~
        `[now.bowl u.running.dog]
      ==
    ==
  ::
  =?  old-state  ?=(%4 -.old-state)
    %-  (slog leaf+"upgrading eth-watcher from %4" ~)
    ^-  app-state-5
    %=    old-state
        -  %5
        dogs
      %-  ~(run by dogs.old-state)
      |=  dog=watchdog-4
      ^-  watchdog-5
      %=  dog
          -
        ^-  config-5
        =,  -.dog
        [url eager refresh-rate timeout-time from contracts ~ topics]
      ::
          pending-logs-4
        %-  ~(run by pending-logs-4.dog)
        |=  =loglist-4
        %+  turn  loglist-4
        |=  =event-log-4
        event-log-4(mined ?~(mined.event-log-4 ~ `mined.event-log-4))
      ::
          history-4
        %+  turn  history-4.dog
        |=  =loglist-4
        %+  turn  loglist-4
        |=  =event-log-4
        event-log-4(mined ?~(mined.event-log-4 ~ `mined.event-log-4))
      ==
    ==
  ::
  =?  old-state  ?=(%5 -.old-state)
    ^-  app-state
    %=    old-state
        -  %6
        dogs
      %-  ~(run by dogs.old-state)
      |=  dog=watchdog-5
      ^-  watchdog
      %=  dog
          -
        ^-  config
        =,  -.dog
        [url eager refresh-rate timeout-time from ~ contracts batchers topics]
      ::
          running
        ?~  running.dog  ~
        `[now.bowl tid.u.running.dog]
      ==
    ==
  ::
  [cards-1 this(state ?>(?=(%6 -.old-state) old-state))]
  ::
  +$  app-states
    $%(app-state-0 app-state-1 app-state-2 app-state-3 app-state-4 app-state-5 app-state)
  ::
  +$  app-state-5
    $:  %5
        dogs=(map path watchdog-5)
    ==
  ::
  +$  watchdog-5
    $:  config-5
        running=(unit [since=@da =tid:spider])
        =number:block
        =pending-logs
        =history
        blocks=(list block)
    ==
  ::
  +$  config-5
    $:  url=@ta
        eager=?
        refresh-rate=@dr
        timeout-time=@dr
        from=number:block
        contracts=(list address:ethereum)
        batchers=(list address:ethereum)
        =topics
    ==
  ::
  +$  app-state-4
    $:  %4
        dogs=(map path watchdog-4)
    ==
  ::
  +$  watchdog-4
    $:  config-4
        running=(unit [since=@da =tid:spider])
        =number:block
        =pending-logs-4
        =history-4
        blocks=(list block)
    ==
  ::
  +$  config-4
    $:  url=@ta
        eager=?
        refresh-rate=@dr
        timeout-time=@dr
        from=number:block
        contracts=(list address:ethereum)
        =topics
    ==
  +$  pending-logs-4  (map number:block loglist-4)
  +$  history-4       (list loglist-4)
  +$  loglist-4       (list event-log-4)
  +$  event-log-4
    $:  $=  mined  %-  unit
        $:  log-index=@ud
            transaction-index=@ud
            transaction-hash=@ux
            block-number=@ud
            block-hash=@ux
            removed=?
        ==
      ::
        address=@ux
        data=@t
        topics=(lest @ux)
    ==
  ::
  +$  app-state-3
    $:  %3
        dogs=(map path watchdog-3)
    ==
  ::
  +$  watchdog-3
    $:  config-3
        running=(unit =tid:spider)
        =number:block
        =pending-logs-4
        =history-4
        blocks=(list block)
    ==
  ::
  +$  config-3
    $:  url=@ta
        eager=?
        refresh-rate=@dr
        from=number:block
        contracts=(list address:ethereum)
        =topics
    ==
  ::
  +$  app-state-2
    $:  %2
        dogs=(map path watchdog-1)
    ==
  ::
  +$  app-state-1
    $:  %1
        dogs=(map path watchdog-1)
    ==
  ::
  +$  watchdog-1
    $:  config-1
        running=(unit =tid:spider)
        =number:block
        =pending-logs-4
        =history-4
        blocks=(list block)
    ==
  ::
  +$  config-1
    $:  url=@ta
        refresh-rate=@dr
        from=number:block
        contracts=(list address:ethereum)
        =topics
    ==
  ::
  +$  app-state-0
    $:  %0
        dogs=(map path watchdog-0)
    ==
  ::
  +$  watchdog-0
    $:  config-0
        running=(unit =tid:spider)
        =number:block
        =pending-logs-4
        =history-4
        blocks=(list block)
    ==
  ::
  +$  config-0
    $:  url=@ta
        from=number:block
        contracts=(list address:ethereum)
        =topics
    ==
  --
::
++  on-poke
  |=  [=mark =vase]
  ?>  (team:title [our src]:bowl)
  ?:  ?=(%noun mark)
    ~&  state
    `this
  ?.  ?=(%eth-watcher-poke mark)
    (on-poke:def mark vase)
  ::
  =+  !<(=poke vase)
  ?-  -.poke
      %watch
    ::  fully restart the watchdog if it doesn't exist yet,
    ::  or if result-altering parts of the config changed.
    =/  restart=?
      ?|  !(~(has by dogs.state) path.poke)
          ?!  .=  ->+>+:(~(got by dogs.state) path.poke)
                   +>+>.config.poke
      ==
    ::
    =/  already  (~(has by dogs.state) path.poke)
    ~?  &(already restart)
      [dap.bowl 'overwriting existing watchdog on' path.poke]
    =/  wait-cards=(list card)
      ?:  already
        ~
      [(wait-shortcut path.poke now.bowl) ~]
    ::
    =/  restart-cards=(list card)
      =/  dog  (~(get by dogs.state) path.poke)
      ?.  ?&  restart
              ?=(^ dog)
              ?=(^ running.u.dog)
          ==
        ~
      =/  =cage  [%spider-stop !>([tid.u.running.u.dog &])]
      :_  ~
      `card`[%pass [%starting path.poke] %agent [our.bowl %spider] %poke cage]
    =/  new-dog
      =/  dog=watchdog
        ?:  restart  *watchdog
        (~(got by dogs.state) path.poke)
      =+  pending=(sort ~(tap in ~(key by pending-logs.dog)) lth)
      =?  pending-logs.dog
          ?:  restart  |
          ?~  pending  |
          (gte i.pending from.config.poke)
        ?>  ?=(^ pending)
        ::  if there are pending logs newer than what we poke with,
        ::  we need to clear those too avoid processing duplicates
        ::
        ~&  %dropping-unreleased-logs^[from+i.pending n+(lent pending)]
        ~
      %_  dog
        -       config.poke
        number  from.config.poke
      ==
    =.  dogs.state  (~(put by dogs.state) path.poke new-dog)
    [(weld wait-cards restart-cards) this]
  ::
      %clear
    =.  dogs.state  (~(del by dogs.state) path.poke)
    [~ this]
  ==
::
::  +on-watch: subscribe & get initial subscription data
::
::    /logs/some-path:
::
++  on-watch
  |=  =path
  ^-  (quip card agent:gall)
  ?.  ?=([%logs ^] path)
    ~|  [%invalid-subscription-path path]
    !!
  :_  this  :_  ~
  :*  %give  %fact  ~  %eth-watcher-diff  !>
      :-  %history
      ^-  loglist
      %-  zing
      %-  flop
      =<  history
      (~(gut by dogs.state) t.path *watchdog)
  ==
::
++  on-leave  on-leave:def
::
::  +on-peek: get diagnostics data
::
::    /block/some-path: get next block number to check for /some-path
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  ~
      [%x %block ^]
    ?.  (~(has by dogs.state) t.t.path)  ~
    :+  ~  ~
    :-  %atom
    !>(number:(~(got by dogs.state) t.t.path))
  ::
      [%x %dogs ~]
    ``noun+!>(~(key by dogs.state))
  ::
      [%x %dogs %configs ~]
    ``noun+!>((~(run by dogs.state) |=(=watchdog -.watchdog)))
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  |^
  ^-  (quip card agent:gall)
  ?.  ?=([%running *] wire)
    (on-agent:def wire sign)
  ?-    -.sign
      %poke-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"eth-watcher couldn't start thread" u.p.sign)
    :_  (clear-running t.wire)  :_  ~
    (leave-spider t.wire our.bowl)
  ::
      %watch-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"eth-watcher couldn't start listening to thread" u.p.sign)
    ::  TODO: kill thread that may have started, although it may not
    ::  have started yet since we get this response before the
    ::  %start-spider poke is processed
    ::
    [~ (clear-running t.wire)]
  ::
      %kick  [~ (clear-running t.wire)]
      %fact
    =*  path  t.wire
    =/  dog  (~(get by dogs.state) path)
    ?~  dog
      [~ this]
    ?+    p.cage.sign  (on-agent:def wire sign)
        %thread-fail
      =+  !<([=term =tang] q.cage.sign)
      %-  (slog leaf+"eth-watcher failed; will retry" leaf+<term> tang)
      [~ this(dogs.state (~(put by dogs.state) path u.dog(running ~)))]
    ::
        %thread-done
      ::  if empty, that means we cancelled this thread
      ::
      ?:  =(*vase q.cage.sign)
        `this
      =+  !<([vows=disavows pup=watchpup] q.cage.sign)
      =.  u.dog
        %_  u.dog
          -             -.pup
          number        number.pup
          blocks        blocks.pup
          pending-logs  pending-logs.pup
        ==
      =^  cards-1  u.dog  (disavow path u.dog vows)
      =^  cards-2  u.dog  (release-logs path u.dog)
      =.  dogs.state  (~(put by dogs.state) path u.dog(running ~))
      [(weld cards-1 cards-2) this]
    ==
  ==
  ::
  ++  clear-running
    |=  =path
    =/  dog  (~(get by dogs.state) path)
    ?~  dog
      this
    this(dogs.state (~(put by dogs.state) path u.dog(running ~)))
  ::
  ++  disavow
    |=  [=path dog=watchdog vows=disavows]
    ^-  (quip card watchdog)
    =/  history-ids=(list [id:block loglist])
      %+  murn  history.dog
      |=  logs=loglist
      ^-  (unit [id:block loglist])
      ?~  logs
         ~
      `[[block-hash block-number]:(need mined.i.logs) logs]
    =/  actual-vows=disavows
      %+  skim  vows
      |=  =id:block
      (lien history-ids |=([=history=id:block *] =(id history-id)))
    =/  actual-history=history
      %+  murn  history-ids
      |=  [=id:block logs=loglist]
      ^-  (unit loglist)
      ?:  (lien actual-vows |=(=vow=id:block =(id vow-id)))
        ~
      `logs
    :_  dog(history actual-history)
    %+  turn  actual-vows
    |=  =id:block
    [%give %fact [%logs path]~ %eth-watcher-diff !>([%disavow id])]
  ::
  ++  release-logs
    |=  [=path dog=watchdog]
    ^-  (quip card watchdog)
    ?:  (lth number.dog 30)
      `dog
    =/  numbers=(list number:block)  ~(tap in ~(key by pending-logs.dog))
    =.  numbers  (sort numbers lth)
    =^  logs=(list event-log:rpc:ethereum)  dog
      |-  ^-  (quip event-log:rpc:ethereum watchdog)
      ?~  numbers
        `dog
      =^  rel-logs-1  dog
        =/  =loglist  (~(get ja pending-logs.dog) i.numbers)
        =.  pending-logs.dog  (~(del by pending-logs.dog) i.numbers)
        ?~  loglist
          `dog
        =.  history.dog  [loglist history.dog]
        [loglist dog]
      =^  rel-logs-2  dog  $(numbers t.numbers)
      [(weld rel-logs-1 rel-logs-2) dog]
    :_  dog
    ?~  logs
      ~
    ^-  (list card:agent:gall)
    [%give %fact [%logs path]~ %eth-watcher-diff !>([%logs logs])]~
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card agent:gall)
  ?+    +<.sign-arvo  ~|([%strange-sign-arvo -.sign-arvo] !!)
      %wake
    ?.  ?=([%timer *] wire)  ~&  weird-wire=wire  [~ this]
    =*  path  t.wire
    ?.  (~(has by dogs.state) path)
      [~ this]
    =/  dog=watchdog
      (~(got by dogs.state) path)
    ?^  error.sign-arvo
      ::  failed, try again.  maybe should tell user if fails more than
      ::  5 times.
      ::
      %-  (slog leaf+"eth-watcher failed; will retry" ~)
      [[(wait path now.bowl refresh-rate.dog)]~ this]
    ::  maybe kill a timed-out update thread, maybe start a new one
    ::
    =^  stop-cards=(list card)  dog
      ::  if still running beyond timeout time, kill it
      ::
      ?.  ?&  ?=(^ running.dog)
            ::
              %+  gth  now.bowl
              (add since.u.running.dog timeout-time.dog)
          ==
        `dog
      ::
      %-  (slog leaf+"eth-watcher {(spud path)} timed out; will restart" ~)
      =/  =cage  [%spider-stop !>([tid.u.running.dog |])]
      :_  dog(running ~)
      :~  (leave-spider path our.bowl)
          [%pass [%starting path] %agent [our.bowl %spider] %poke cage]
      ==
    ::
    =^  start-cards=(list card)  dog
      ::  if not (or no longer) running, start a new thread
      ::
      ?^  running.dog
        `dog
      :: if reached the to-block, don't start a new thread
      ::
      ?:  ?&  ?=(^ to.dog)
              (gte number.dog u.to.dog)
          ==
        `dog
      ::
      =/  new-tid=@ta
        (cat 3 'eth-watcher--' (scot %uv eny.bowl))
      :_  dog(running `[now.bowl new-tid])
      =/  args
        :*  ~  `new-tid  bec  %eth-watcher
            !>([~ `watchpup`[- number pending-logs blocks]:dog])
        ==
      :~  (watch-spider path our.bowl /thread-result/[new-tid])
          (poke-spider path our.bowl %spider-start !>(args))
      ==
    ::
    :-  [(wait path now.bowl refresh-rate.dog) (weld stop-cards start-cards)]
    this(dogs.state (~(put by dogs.state) path dog))
  ==
::
++  on-fail   on-fail:def
--
