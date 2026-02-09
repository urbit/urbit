!:
::  http-client
::
|=  our=ship
=,  iris
::
::
::  internal data structures
::
=>  =~
::
::  internal data structures that won't go in zuse
::
|%
+$  move
  ::
  $:  ::  duct: request identifier
      ::
      =duct
      ::
      ::
      card=(wind note gift)
  ==
::  +note: private request from iris to another vane
::
+$  note
  $%  ::  %d: to dill
      ::
      $:  %d
          ::
          ::
      $%  [%flog =flog:dill]
  ==  ==  ==
--
::  more structures
::
|%
+$  axle
  $:  ::  date: date at which iris state was updated to this data structure
      ::
      date=%~2025.7.17
      ::
      ::
      =state
  ==
::  +state:client: state relating to open outbound HTTP connections
::
+$  state
  $:  ::  next-id: monotonically increasing id number for the next connection
      ::
      next-id=@ud
      ::  connection-by-id: open connections to the
      ::
      connection-by-id=(map @ud [=duct =in-progress-http-request])
      ::  connection-by-duct: used for cancellation
      ::
      connection-by-duct=(map duct @ud)
      ::  outbound-duct: the duct to send outbound requests on
      ::
      outbound-duct=duct
  ==
::  +in-progress-http-request: state around an outbound http
::
+$  in-progress-http-request
  $:  ::  remaining-redirects: http limit of number of redirects before error
      ::
      remaining-redirects=@ud
      ::  remaining-retries: number of times to retry the request
      ::
      remaining-retries=@ud
      ::  response-header: the response headers from the %start packet
      ::
      ::    We send the response headers with each %http-progress, so we must
      ::    save them.
      ::
      response-header=(unit response-header:http)
      ::  chunks: a list of partial results returned from unix
      ::
      ::    This list of octs must be flopped before it is composed as the
      ::    final response, as we want to be able to quickly insert.
      ::
      chunks=(list octs)
      ::  bytes-read: the sum of the size of the :chunks
      ::
      bytes-read=@ud
      ::  expected-size: the expected content-length of the http request
      ::
      expected-size=(unit @ud)
      ::  request: the original request, needed for handling redirects
      ::
      request=request:http
  ==
--
::
|%
::  +combine-octs: combine multiple octs into one
::
++  combine-octs
  |=  a=(list octs)
  ^-  octs
  :-  %+  roll  a
      |=  [=octs sum=@ud]
      (add sum p.octs)
  (can 3 a)
::  +per-client-event: per-event client core
::
++  per-client-event
  |=  [[eny=@ =duct now=@da rof=roof] =state]
  |%
  ::  +request: makes an external web request
  ::
  ++  request
    |=  [=request:http =outbound-config]
    ^-  [(list move) ^state]
    ::  if there's already a request on this duct, abort
    ::
    ?:  (~(has by connection-by-duct.state) duct)
      ~&  %cant-send-second-http-client-request-on-same-duct
      [~ state]
    ::  get the next id for this request
    ::
    =^  id  next-id.state  [next-id.state +(next-id.state)]
    ::  add a new open session
    ::
    =.  connection-by-id.state
      %+  ~(put by connection-by-id.state)  id
      =,  outbound-config
      [duct [redirects retries ~ ~ 0 ~ request]]
    ::  keep track of the duct for cancellation
    ::
    =.  connection-by-duct.state
      (~(put by connection-by-duct.state) duct id)
    :: if we don't have a duct yet just ignore the request, %born will
    :: cancel it soon. this is not ideal to say the least.
    ::
    ?~  outbound-duct.state  [~ state]
    ::  start the download
    ::
    ::  the original eyre keeps track of the duct on %born and then sends a
    ::  %give on that duct. this seems like a weird inversion of
    ::  responsibility, where we should instead be doing a pass to unix. the
    ::  reason we need to manually build ids is because we aren't using the
    ::  built in duct system.
    ::
    ::  email discussions make it sound like fixing that might be hard, so
    ::  maybe i should just live with the way it is now?
    ::
    :-  [outbound-duct.state %give %request id request]~
    state
  ::  +cancel: client cancels an outstanding request
  ::
  ++  cancel
    ^-  [(list move) ^state]
    ::
    ?~  cancel-id=(~(get by connection-by-duct.state) duct)
      ~&  %iris-invalid-cancel
      [~ state]
    ::
    :-  ?~  outbound-duct.state  ~
      [outbound-duct.state %give %cancel-request u.cancel-id]~
    (cleanup-connection u.cancel-id)
  ::  +receive: receives a response to an http-request we made
  ::
  ::    TODO: Right now, we are not following redirect and not handling retries
  ::    correctly. We need to do this.
  ::
  ++  receive
    |=  [id=@ud =http-event:http]
    ^-  [(list move) ^state]
    ::  ensure that this is a valid receive
    ::
    ?~  connection=(~(get by connection-by-id.state) id)
      ~&  [%eyre-unknown-receive id]
      [~ state]
    ::
    ?-    -.http-event
        %start
      ::  TODO: Handle redirects and retries here, before we start dispatching
      ::  back to the application.
      ::
      ::  record data from the http response that only comes from %start
      ::
      =.  connection-by-id.state
        %+  ~(jab by connection-by-id.state)  id
        |=  [duct=^duct =in-progress-http-request]
        ::
        =.  expected-size.in-progress-http-request
          ?~  str=(get-header:http 'content-length' headers.response-header.http-event)
            ~
          ::
          (rush u.str dum:ag)
        ::
        =.  response-header.in-progress-http-request
          `response-header:http-event
        ::
        [duct in-progress-http-request]
      =*  status-code=@ud  status-code.response-header.http-event
      ?:  ?|  =(307 status-code)
              =(303 status-code)
              =(301 status-code)
          ==
          %:        handle-redirect
                  id
                http-event
              remaining-redirects.in-progress-http-request.u.connection
            request.in-progress-http-request.u.connection
          ==
      ?:  complete.http-event
        (send-finished id data.http-event)
      ::
      (record-and-send-progress id data.http-event)
    ::
        %continue
      ?:  complete.http-event
        (send-finished id data.http-event)
      ::
      (record-and-send-progress id data.http-event)
    ::
        %cancel
      ::  we have received a cancel from outside; pass it on to our requester
      ::
      :_  (cleanup-connection id)
      ^-  (list move)
      :_  ~
      :*  duct.u.connection
        %give
        %http-response
        %cancel
        ~
      ==
    ==
  ::  +handle-redirect: transparently handle redirects if applicable
  ::
  ++  handle-redirect
    |=  [id=@ud =http-event:http remaining-redirects=@ud =request:http]
    ?>  ?=(%start -.http-event)
    ?:  =(0 remaining-redirects)
      ?:  complete.http-event
        (send-finished id data.http-event)
      (record-and-send-progress id data.http-event)
    ?~  loc=(get-header:http 'location' headers.response-header.http-event)
      ?:  complete.http-event
        (send-finished id data.http-event)
      (record-and-send-progress id data.http-event)
    =.  connection-by-id.state
      %+  ~(jab by connection-by-id.state)  id
      |=  [duct=^duct =in-progress-http-request]
      :-  duct
      %=  in-progress-http-request
          remaining-redirects
        (dec remaining-redirects.in-progress-http-request)
      ==
    :_  state
    [outbound-duct.state %give %request id request(url u.loc)]~
  ::  +record-and-send-progress: save incoming data and send progress report
  ::
  ++  record-and-send-progress
    |=  [id=@ud data=(unit octs)]
    ^-  [(list move) ^state]
    ::
    =.  connection-by-id.state
      %+  ~(jab by connection-by-id.state)  id
      |=  [duct=^duct =in-progress-http-request]
      ::  record the data chunk and size, if it exists
      ::
      =?    chunks.in-progress-http-request
          ?=(^ data)
        [u.data chunks.in-progress-http-request]
      =?    bytes-read.in-progress-http-request
          ?=(^ data)
        (add bytes-read.in-progress-http-request p.u.data)
      ::
      [duct in-progress-http-request]
    ::
    =/  connection  (~(got by connection-by-id.state) id)
    :_  state
    ^-  (list move)
    :_  ~
    :*  duct.connection
        %give
        %http-response
        %progress
        (need response-header.in-progress-http-request.connection)
        bytes-read.in-progress-http-request.connection
        expected-size.in-progress-http-request.connection
        data
    ==
  ::  +send-finished: sends the %finished, cleans up the session state
  ::
  ++  send-finished
    |=  [id=@ud data=(unit octs)]
    ^-  [(list move) ^state]
    ::
    =/  connection  (~(got by connection-by-id.state) id)
    ::  reassemble the octs that we've received into their final form
    ::
    =/  data=octs
      %-  combine-octs
      %-  flop
      ::
      ?~  data
        chunks.in-progress-http-request.connection
      [u.data chunks.in-progress-http-request.connection]
    ::
    =/  response-header=response-header:http
      (need response-header.in-progress-http-request.connection)
    ::
    =/  mime=@t
      ?~  mime-type=(get-header:http 'content-type' headers.response-header)
        'application/octet-stream'
      u.mime-type
    ::
    :_  (cleanup-connection id)
    :~  :*  duct.connection
            %give
            %http-response
            %finished
            response-header
            ?:(=(0 p.data) ~ `[mime data])
    ==  ==
  ::
  ++  cleanup-connection
    |=  id=@ud
    ^-  ^state
    ?~  con=(~(get by connection-by-id.state) id)
      state
    %_    state
      connection-by-id    (~(del by connection-by-id.state) id)
      connection-by-duct  (~(del by connection-by-duct.state) duct.u.con)
    ==
  --
--
::  end the =~
::
.  ==
::  begin with a default +axle as a blank slate
::
=|  ax=axle
::  a vane is activated with current date, entropy, and a namespace function
::
|=  [now=@da eny=@uvJ rof=roof]
::  allow jets to be registered within this core
::
~%  %http-client  ..part  ~
|%
++  call
  |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
  ~>  %spin.['call/iris']
  ^-  [(list move) _iris-gate]
  ::
  =/  task=task  ((harden task) wrapped-task)
  ::
  ::  XX handle error notifications
  ::
  ?^  dud
    =/  moves=(list move)
      [[duct %slip %d %flog %crud [-.task tang.u.dud]] ~]
    [moves iris-gate]
  ::  %trim: in response to memory pressure
  ::
  ?:  ?=(%trim -.task)
    [~ iris-gate]
  ::  %vega: notifies us of a completed kernel upgrade
  ::
  ?:  ?=(%vega -.task)
    [~ iris-gate]
  ::
  =/  event-args  [[eny duct now rof] state.ax]
  =/  client  (per-client-event event-args)
  ?-    -.task
  ::
      %born
    ::  create a cancel for each outstanding connection
    ::
    ::    TODO: We should gracefully retry on restart instead of just sending a
    ::    cancel.
    ::    TODO  we might not want to do that though!
    ::
    =/  moves=(list move)
      %+  turn  ~(tap by connection-by-duct.state.ax)
      |=  [=^duct @ud]
      ^-  move
      [duct %give %http-response %cancel ~]
    ::  reset all connection state on born
    ::
    =:  next-id.state.ax             0
        connection-by-id.state.ax    ~
        connection-by-duct.state.ax  ~
        outbound-duct.state.ax       duct
    ==
    ::
    [moves iris-gate]
  ::
      %request
    =^  moves  state.ax  (request:client +.task)
    [moves iris-gate]
  ::
      %cancel-request
    =^  moves  state.ax  cancel:client
    [moves iris-gate]
  ::
      %receive
    =^  moves  state.ax  (receive:client +.task)
    [moves iris-gate]
  ==
::  http-client issues no requests to other vanes
::
++  take
  |=  [=wire =duct dud=(unit goof) sign=*]
  ^-  [(list move) _iris-gate]
  ~>  %spin.['take/iris']
  ?<  ?=(^ dud)
  !!
::
++  iris-gate  ..$
::  +load: migrate old state to new state (called on vane reload)
::
++  load
  =>  |%
      +$  axle-any
        $%  [date=%~2019.2.8 state=state-0]
            [date=%~2025.7.17 =state]
        ==
      ::
      +$  state-0
        $:  next-id=@ud
            connection-by-id=(map @ud [=duct in-progress-http-request=in-progress-http-request-0])
            connection-by-duct=(map duct @ud)
            outbound-duct=duct
        ==
      +$  in-progress-http-request-0
        $:  remaining-redirects=@ud
            remaining-retries=@ud
            response-header=(unit response-header:http)
            chunks=(list octs)
            bytes-read=@ud
            expected-size=(unit @ud)
        ==
      --
  |=  old=axle-any
  ^+  iris-gate
  ~>  %spin.['load/iris']
  ?-    -.old
      %~2019.2.8
    %=  $
      date.old  %~2025.7.17
    ::
      connection-by-id.state.old
    %-  ~(run by connection-by-id.state.old)
    |=  [d=duct r=in-progress-http-request-0]
    ^-  [duct in-progress-http-request]
    :-  d
    ::  set remaining redirects to 0 because we don't have the original request.
    ::  it's safe to bunt the .request because it only gets used if
    ::  .remaining-redirects is non-zero.
    ::
    :-  remaining-redirects=0
    +.r(expected-size [expected-size.r *request:http])
    ==
      %~2025.7.17
    iris-gate(ax old)
  ==
::  +stay: produce current state
::
++  stay  `axle`ax
::  +scry: request a path in the urbit namespace
::
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  ~>  %spin.['scry/iris']
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ?.  ?=(%& -.why)  ~
  =*  his  p.why
  ?:  &(?=(%x ren) =(tyl //whey) =([~ ~] lyc))
    =/  maz=(list mass)
      :~  nex+&+next-id.state.ax
          outbound+&+outbound-duct.state.ax
          by-id+&+connection-by-id.state.ax
          by-duct+&+connection-by-duct.state.ax
          axle+&+ax
      ==
    ``mass+!>(maz)
  [~ ~]
--
