/+  *server
|%
+$  move  [bone card]
+$  card
  $%  [%poke wire dock pear]
      [%wait wire @da]
      [%rest wire @da]
      [%info wire toro:clay]
      [%connect wire binding:eyre term]
      [%http-response =http-event:http]
  ==
::
+$  pear
  $%  [%noun cord]
      [%helm-hi cord]
  ==
::
+$  state
  $:  scanning=_|                        :: are we currently scanning?
      filter=(set ship)
      configured=(set ship)              :: set of ships to scan
      outstanding=(map ship @da)         :: ships we've |hi'd with no reply
      log=(map ship (list [@da @da]))    :: log of past |hi's sent/recv by ship
      timers=(map wire @da)              :: map of delay timers
      moves=(list move)                  :: list of outgoing moves
  ==
::
--
|_  [bol=bowl:gall sty=state]
::
++  this  .
::
++  abet
  ^-  (quip move _this)
  [moves.sty this(moves.sty ~)]
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  :-  [[ost.bol %connect / [~ /'~radar'] %radar]]~
  ?~  old
    %=    this
    ::
        scanning.sty  %.y
    ::
        configured.sty  ~
    ==
  this(sty u.old)
::
++  get-point-json
  |=  her=ship
  ^-  json
  ?.  (~(has in configured.sty) her)
   [%s 'not being tracked']
  =/  lastseen/(unit @da)
    (~(get by outstanding.sty) her)
  =/  log/(list [@da @da])
    (fall (~(get by log.sty) her) ~)
  =/  failed/(list {p/@t q/json})
    ?~  lastseen
      ~
    [%'last-seen' (time:enjs:format u.lastseen)]~
  %-  pairs:enjs:format
  :_  failed
  :-  %responded
  :-  %a
  %+  turn  log
  |=  [sent=@da recv=@da]
  ^-  json
  %-  pairs:enjs:format
  :~  [%sent (time:enjs:format sent)]
      [%recv (time:enjs:format recv)]
  ==
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?+  a
    [~ this]
  ::
      %running
    ~&  scanning.sty
    [~ this]
  ::
      %print
    ~&  sty
    [~ this]
  ::
      %start
    ?:  scanning.sty
      ~&  'already running'
      [~ this]
    =.  this  this(scanning.sty &)
    ::
    =.  this  (scan ~(tap in configured.sty))
    abet
  ::
      %stop
    :_  this(scanning.sty |, timers.sty ~)
    %-  ~(rep by timers.sty)
    |=  [[wir=wire d=@da] mov=(list move)]
    ^-  (list move)
    [[ost.bol %rest wir d] mov]
  ::
      %flush
    [~ this(log.sty ~)]
  ==
::

++  ping
  |=  her=ship
  ^+  this
  %=  this
  ::
      moves.sty
    :_  moves.sty
    :*  ost.bol
        %poke
        /ping/(scot %p her)/(scot %da now.bol)
        [her %hall]  %noun  'fdsjfk'
    ==
  ::
      outstanding.sty
    (~(put by outstanding.sty) her now.bol)
  ==
::
++  scan
  |=  ships=(list ship)
  ^+  this
  |-
  ?~  ships
    this
  =.  this  (ping i.ships)
  $(ships t.ships)
::
++  coup
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?>  ?=([%ping @t @t ~] wir)
  ::
  =/  who=@p   (slav %p i.t.wir)
  =/  wen=@da  (slav %da i.t.t.wir)
  =/  out=(unit @da)  (~(get by outstanding.sty) who)
  ?~  out
    [~ this]    :: no longer tracking ship
  ?>  =(wen u.out)
  ::
  =/  old-log=(unit (list [@da @da]))
    (~(get by log.sty) who)
  ::
  =/  new-log=(list [@da @da])
    ?~  old-log  [[wen now.bol] ~]
    [[wen now.bol] u.old-log]
  ::
  =.  outstanding.sty  (~(del by outstanding.sty) who)

  ::
  ?.  scanning.sty
    [~ this]
  ::
  =/  timeout=@da  (add now.bol ~m10)
  =/  wait-wire=wire  /wait/[i.t.wir]/(scot %da timeout)
  :-  [ost.bol %wait wait-wire timeout]~
  %=  this
  ::
      log.sty
    (~(put by log.sty) who new-log)
  ::
      timers.sty
    (~(put by timers.sty) wait-wire timeout)
  ::
  ==
::
:: Routing
:: /~radar GET -> list of ships being tracked
:: /~radar/{shipname} GET -> uptime info of ship
:: /~radar/{shipname} POST -> start tracking ship (requires authorization)
:: /~radar/{shipname} DELETE -> stop tracking ship (requires authorization)
++  poke-handle-http-request
  =<
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  =/  method  method.request.inbound-request
  =/  authorized  authenticated.inbound-request
  =/  request-line  (parse-request-line url.request.inbound-request)
  =/  back-path  (flop site.request-line)
  =/  name=@t
    ?~  back-path
      ''
    i.back-path
  =/  her/(unit ship)
    (slaw %p name)
  ?+  method  unsupported
    ::
      %'GET'
    ?~  her
      index
    (get u.her)
    ::
      %'POST'
    ?~  her
      unsupported
    ?.  authorized
      unauthorized
    (post u.her)
    ::
      %'DELETE'
    ?~  her
      unsupported
    ?.  authorized
      unauthorized
    (delete u.her)
  ==
  |%
  ++  no-content
    ^-  json
    (frond:enjs:format [%success [%s 'true']])
  ::
  ++  get
    |=  her=ship
    ^-  (quip move _this)
    :_  this
    :_  ~
    (json-response (get-point-json her))
  ::
  ++  json-response
    |=  jon=json
    ^-  move
    :*
    ost.bol
    %http-response
    %-  json-response:app
    (json-to-octs jon)
    ==
  ::
  ++  unauthorized
    ^-  (quip move _this)
    :_  this
    [ost.bol %http-response [%start [401 ~] ~ %.y]]~
  ::
  ++  unsupported
    ^-  (quip move _this)
    :_  this
    [ost.bol %http-response [%start [400 ~] ~ %.y]]~
  ::
  ++  post
      |=  her=ship
      ^-  (quip move _this)
      =.  this  (ping her)
      =<  abet
      %=  this
        ::
          configured.sty
        (~(put in configured.sty) her)
        ::
          moves.sty
        [(json-response no-content) moves.sty]
      ==
  ::
  ++  index
    ^-  (quip move _this)
    :_  this
    :_  ~
    %-  json-response
    %-  frond:enjs:format
    :-  %ships
    :-  %a
    %+  turn  ~(tap in configured.sty)
    |=  her=ship
    :-  %s
    (scot %p her)
  ::
  ++  delete
    |=  her=ship
    ^-  (quip move _this)
    :-  [(json-response no-content) ~]
    %=  this
     configured.sty   (~(del in configured.sty) her)
     outstanding.sty  (~(del in outstanding.sty) her)
    ==
  --
::
++  wake
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?+    wir
    [~ this]
  ::

      [%wait @t @t ~]
    =/  who=@p   (slav %p i.t.wir)
    =/  wen=@da  (slav %da i.t.t.wir)
    =.  timers.sty  (~(del by timers.sty) wir)
    ?.  (~(has in configured.sty) who)
      [~ this]
    ?:  scanning.sty
      abet:(ping who)
    [~ this]
  ==
--
