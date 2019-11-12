::  eth-watcher: ethereum event log collector
::
/-  *eth-watcher, spider
/+  run-imp, default-agent, verb
=,  ethereum-types
=,  able:jael
::
|%
+$  card  card:agent:mall
+$  app-state
  $:  %0
      dogs=(map path watchdog)
  ==
::
+$  context  [=path dog=watchdog]
+$  watchdog
  $:  config
      =number:block
      =pending-logs
      =history
      blocks=(list block)
  ==
::
::  history: newest block first, oldest event first
+$  history       (list loglist)
--
::
::  Helpers
::
|%
++  refresh-rate  ~m5
++  wait
  |=  now=@da
  ^-  card
  [%pass /timer %arvo %b %wait (add now refresh-rate)]
--
::
::
::  Main
::
^-  agent:mall
=|  state=app-state
%+  verb  &
|_  =bowl:mall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  ::  start update timer loop
  [[(wait now.bowl) ~] this]
::
++  on-save   !>(state)
++  on-load
  |=  old=vase
  ~&  >  %old-ew
  =+  !<(old-state=app-state old)

  `this(state old-state)
::
++  on-poke
  |=  [=mark =vase]
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
    ::  or if the new config changes more than just the url.
    ~&  >  %ouch
    =/  restart=?
      ?|  !(~(has by dogs.state) path.poke)
          ?!  .=  ->:(~(got by dogs.state) path.poke)
                  +.config.poke
      ==
    ~?  &((~(has by dogs.state) path.poke) restart)
      [dap.bowl 'overwriting existing watchdog on' path.poke]
    =/  new-dog
      =/  dog=watchdog
        ?:  restart  *watchdog
        (~(got by dogs.state) path.poke)
      %_  dog
        -       config.poke
        number  from.config.poke
      ==
    =.  dogs.state  (~(put by dogs.state) path.poke new-dog)
    :_  this  :_  ~
    [%pass /shortcut %arvo %b %wait now.bowl]
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
  ?.  ?=([%logs ^] path)
    (on-watch:def path)
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
  ?.  ?=([%x %block ^] path)  ~
  ?.  (~(has by dogs.state) t.t.path)  ~
  :+  ~  ~
  :-  %atom
  !>(number:(~(got by dogs.state) t.t.path))
::
++  on-agent
  |=  [=wire =sign:agent:mall]
  |^
  ^-  (quip card agent:mall)
  ?+    wire  (on-agent:def wire sign)
      [%run *]
    ?+    -.sign  (on-agent:def wire sign)
        %fact
      =*  path  t.wire
      =/  dog  (~(get by dogs.state) path)
      ?~  dog
        [~ this]
      ?+    p.cage.sign  (on-agent:def wire sign)
          %imp-fail
        =+  !<([=term =tang] q.cage.sign)
        %-  (slog leaf+"eth-watcher failed; will retry" leaf+<term> tang)
        `this
      ::
          %imp-done
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
        [(weld cards-1 cards-2) this]
      ==
    ==
  ==
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
    [%give %fact `path %eth-watcher-diff !>([%disavow id])]
  ::
  ++  release-logs
    |=  [=path dog=watchdog]
    ^-  (quip card watchdog)
    ?:  (lth number.dog 30)
      `dog
    =/  rel-number  (sub number.dog 30)
    =/  numbers  ~(tap in ~(key by pending-logs.dog))
    |-  ^-  (quip card watchdog)
    ?~  numbers
      `dog
    ?:  (gth i.numbers rel-number)
      $(numbers t.numbers)
    =^  cards-1  dog
      =/  =loglist  (~(get ja pending-logs.dog) i.numbers)
      =.  pending-logs.dog  (~(del by pending-logs.dog) i.numbers)
      ?~  loglist
        `dog
      =.  history.dog  [loglist history.dog]
      :_  dog
      %+  turn  loglist
      |=  =event-log:rpc:ethereum
      ^-  card
      [%give %fact `path %eth-watcher-diff !>([%log event-log])]
    =^  cards-2  dog  $(numbers t.numbers)
    [(weld cards-1 cards-2) dog]
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card agent:mall)
  ?+  +<.sign-arvo  ~|([%strange-sign-arvo -.sign-arvo] !!)
      %wake
    =;  rest
      ?.  =(/timer wire)
        rest
      [[(wait now.bowl) -.rest] +.rest]
    ?^  error.sign-arvo
      ::  failed, try again.  maybe should tell user if fails more than
      ::  5 times.
      ::
      [[(wait now.bowl) ~] this]
    ::  start all updates in parallel
    ::
    =/  dogs=(list [=path dog=watchdog])  ~(tap by dogs.state)
    =|  cards=(list card)
    =/  this-agent=agent:mall  this
    ^-  (quip card agent:mall)
    |-  ^-  (quip card agent:mall)
    =*  loop  $
    ?~  dogs
      [cards this-agent]
    =,  i.dogs
    ::  XX should not start another one if the previous is still going
    =^  new-cards  this-agent

      =/  args
        ``[%eth-watcher !>(`watchpup`[- number pending-logs blocks]:dog)]
      (run-imp our.bowl [%run path] args this-agent)
    loop(dogs t.dogs, cards (weld cards new-cards))
  ==
::
++  on-fail   on-fail:def
--
