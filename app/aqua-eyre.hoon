::  Pass-through Eyre driver
::
/-  aquarium
=,  aquarium
=>  $~  |%
    +$  move  (pair bone card)
    +$  card
      $%  [%poke wire dock %aqua-events (list aqua-event)]
          [%peer wire dock path]
          [%pull wire dock ~]
          [%hiss wire p=(unit user:eyre) q=mark r=(cask hiss:eyre)]
      ==
    ::
    +$  state
      $:  %0
          subscribed=_|
          piers=(map ship http-requests=(set @ud))
      ==
    --
=,  gall
=|  moves=(list move)
|_  $:  bowl
        state
    ==
++  this  .
++  apex  %_(this moves ~)
++  abet  [(flop moves) this]
++  emit-moves
  |=  ms=(list move)
  %_(this moves (weld ms moves))
::
++  emit-aqua-events
  |=  aes=(list aqua-event)
  %-  emit-moves
  [%poke /aqua-events [our %aqua] %aqua-events aes]~
::
++  poke-aqua-vane-control
  |=  command=?(%subscribe %unsubscribe)
  :_  this(subscribed =(command %subscribe)
  (aqua-vane-control-handler subscribed)
::
++  diff-aqua-effects
  |=  [way=wire afs=aqua-effects]
  ^-  (quip move _this)
  =.  this  apex  =<  abet
  |-  ^+  this
  ?~  ufs.afs
    this
  =.  this
    ?+  -.q.i.ufs.afs  this
      %sleep    abet-pe:handle-sleep:(pe who.afs)
      %restore  abet-pe:handle-restore:(pe who.afs)
      %thus     abet-pe:(handle-thus:(pe who.afs) i.ufs.afs)
    --
  $(ufs.afs t.ufs.afs)
::
::  Received inbound HTTP response
::
++  sigh-httr
  |=  [way=wire res=httr:eyre]
  ^-  (quip move _this)
  =.  this  apex-aqua  =<  abet-aqua
  ?>  ?=([@ *] way)
  =/  who  (,@p (slav %p i.way))
  ~&  [%received-httr who]
  abet-pe:(take-sigh-httr:(pe who) t.way res)
::
::  Received inbound HTTP response error
::
++  sigh-tang
  |=  [way=wire tan=tang]
  ^-  (quip move _this)
  =.  this  apex-aqua  =<  abet-aqua
  ?>  ?=([@ *] way)
  =/  who  (,@p (slav %p i.way))
  ~&  [%received-httr who]
  abet-pe:(take-sigh-tang:(pe who) t.way tan)
::
++  pe
  |=  who=ship
  =+  (fall (~(get by piers) who) *pier)
  =*  pier-data  -
  |%
  ++  abet-pe
    ^+  this
    =.  piers  (~(put by piers) who pier-data)
    this
  ::
  ++  handle-sleep
    ^+  ..abet-pe
    ..abet-pe(pier-data *pier)
  ::
  ++  handle-restore
    ^+  ..abet-pe
    %-  emit-aqua-events
    [%event who [//http/0v1n.2m9vh %born ~]]~
  ::
  ++  handle-thus
    |=  [way=wire %thus num=@ud req=(unit hiss:eyre)]
    ^+  ..abet-pe
    ?~  req
      ?.  (~(has in http-requests) num)
        ..abet-pe
      ::  Eyre doesn't support cancelling HTTP requests from userspace,
      ::  so we remove it from our state so we won't pass along the
      ::  response.
      ::
      ~&  [who=who %aqua-eyre-cant-cancel-thus num=num]
      =.  http-requests  (~(del in http-requests) num)
      ..abet-pe
    ~&  [who=who %aqua-eyre-requesting u.req]
    =.  http-requests  (~(put in http-requests) num)
    %-  emit-moves  :_  ~
    :*  ost.hid
        %hiss
        /(scot %p who)/(scot %ud num)
        ~
        %httr
        [%hiss u.req]
    ==
  ::
  ::  Pass HTTP response back to virtual ship
  ::
  ++  take-sigh-httr
    |=  [way=wire res=httr:eyre]
    ^+  ..abet-pe
    ?>  ?=([@ ~] way)
    =/  num  (slav %ud i.way)
    ?.  (~(has in http-requests) num)
      ~&  [who=who %ignoring-httr num=num]
      ..abet-pe
    =.  http-requests  (~(del in http-requests) num)
    (emit-aqua-events [%event who [//http/0v1n.2m9vh %they num res]~)
  ::
  ::  Got error in HTTP response
  ::
  ++  take-sigh-tang
    |=  [way=wire tan=tang]
    ^+  ..abet-pe
    ?>  ?=([@ ~] way)
    =/  num  (slav %ud i.way)
    ?.  (~(has in http-requests) num)
      ~&  [who=who %ignoring-httr num=num]
      ..abet-pe
    =.  http-requests  (~(del in http-requests) num)
    %-  (slog tan)
    ..abet-pe
--
