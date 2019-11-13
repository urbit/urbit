::  eth-watcher: ethereum event log collector
::
/-  *eth-watcher, spider
/+  default-agent, verb
=,  ethereum-types
=,  able:jael
::
=>  |%
    ++  refresh-rate  ~m5
    --
::
=>  |%
    +$  card  card:agent:mall
    +$  app-state
      $:  %0
          dogs=(map path watchdog)
      ==
    ::
    +$  context  [=path dog=watchdog]
    +$  watchdog
      $:  config
          running=(unit (unit =iid:spider))
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
=>  |%
    ++  wait
      |=  now=@da
      ^-  card
      [%pass /timer %arvo %b %wait (add now refresh-rate)]
    ::
    ++  wait-shortcut
      |=  now=@da
      ^-  card
      [%pass /shortcut %arvo %b %wait now]
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
    --
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
    =/  restart-cards
      =/  dog  (~(get by dogs.state) path.poke)
      ?.  ?&  restart
              ?=(^ dog)
              ?=([~ ~ *] running.u.dog)
          ==
        ~
      =/  =cage  [%spider-stop !>([u.u.running.u.dog &])]
      [%pass [%starting path] %agent [our.bowl %spider] %poke cage]
    =/  new-dog
      =/  dog=watchdog
        ?:  restart  *watchdog
        (~(got by dogs.state) path.poke)
      %_  dog
        -       config.poke
        number  from.config.poke
      ==
    =.  dogs.state  (~(put by dogs.state) path.poke new-dog)
    [[(wait-shortcut now.bowl) ~] this]
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
    ~|  [%invalid-subscription-path path]
    !!
  !!
  ::  ;<  ~  bind:m
  ::    %+  send-effect-on-bone:stdio  ost.bowl
  ::    :+  %diff  %eth-watcher-diff
  ::    :-  %history
  ::    ^-  loglist
  ::    %-  zing
  ::    %-  flop
  ::    =<  history
  ::    (~(gut by dogs.state) t.path *watchdog)
  ::  (pure:m state)
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
      [%starting *]
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"eth-watcher failed to get iid" u.p.sign)
      [~ (clear-running t.wire)]
    ::
        %kick
      =*  path  t.wire
      =/  dog  (~(get by dogs.state) path)
      ?~  dog
        [~ this]
      ?~  running.u.dog
        [~ this]
      ?^  u.running.u.dog
        [~ this]
      [~ this(dogs.state (~(put by dogs.state) path u.dog(running ~)))] 
    ::
        %fact
      =*  path  t.wire
      ?>  ?=(%iid p.cage.sign)
      =+  !<(=new=iid:spider q.cage.sign)
      =/  dog  (~(get by dogs.state) path)
      ::  watchdog already cancelled
      ::
      ?~  dog
        [~ this]
      ::  not looking for imp
      ::
      ?~  running.u.dog
        [~ this]
      ::  already running imp
      ::
      ?^  u.running.u.dog
        [~ this]
      =>  .(running.u.dog ``new-iid)
      =/  args
        :^  ~  `new-iid  %eth-watcher
        !>(`watchpup`[- number pending-logs blocks]:u.dog)
      :_  this(dogs.state (~(put by dogs.state) path u.dog))
      :~  (watch-spider path our.bowl /imp-result/[new-iid])
          (poke-spider path our.bowl %spider-start !>(args))
      ==
    ==
  ::
      [%running *]
    ?-    -.sign
        %poke-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"eth-watcher couldn't start imp" u.p.sign)
      [~ (clear-running t.wire)]
    ::
        %watch-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"eth-watcher couldn't start listen to imp" u.p.sign)
      [~ (clear-running t.wire)]
    ::
        %kick  [~ (clear-running t.wire)]
        %fact
      =*  path  t.wire
      =/  dog  (~(get by dogs.state) path)
      ?~  dog
        [~ this]
      ?+    p.cage.sign  (on-agent:def wire sign)
          %imp-fail
        =+  !<([=term =tang] q.cage.sign)
        %-  (slog leaf+"eth-watcher failed; will retry" leaf+<term> tang)
        [~ this(dogs.state (~(put by dogs.state) path u.dog(running ~)))]
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
        =.  dogs.state  (~(put by dogs.state) path u.dog(running ~))
        `this
        ::  [(weld cards-1 cards-2) this]
      ==
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
    ^-  (quip card agent:mall)
    =-  [(flop -<) ->]
    |-  ^-  (quip card agent:mall)
    =*  loop  $
    ?~  dogs
      [cards this]
    =,  i.dogs
    ?^  running.dog.i.dogs
      ?~  u.running.dog.i.dogs
        %-  (slog leaf+"eth-watcher delayed getting iid" ~)
        loop(dogs t.dogs)
      ::  if still running, kill it and restart
      ::
      =/  =cage  [%spider-stop !>([u.u.running.dog |])]
      =.  cards
        :_  cards
        [%pass [%starting path] %agent [our.bowl %spider] %poke cage]
      loop(i.dogs i.dogs(running.dog ~))
    ::
    =>  .(running.dog.i.dogs [~ ~])
    =.  cards
      :_  cards
      [%pass [%starting path] %agent [our.bowl %spider] %watch /next-iid]
    =.  dogs.state  (~(put by dogs.state) path dog)
    loop(dogs t.dogs)
  ==
::
++  on-fail   on-fail:def
--
