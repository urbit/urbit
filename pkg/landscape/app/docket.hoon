/-  *docket
/+  *server, agentio, default-agent, dbug, verb, hood-kiln=kiln
|%
+$  card  card:agent:gall
+$  state-0
  $:  dockets=(map desk docket)
      charges=(map desk charge)
  ==
::
--
^-  agent:gall
%-  agent:dbug
%+  verb  &
=|  state-0
=*  state  -
=<
|_  =bowl:gall
+*  this  .
    io    ~(. agentio bowl)
    pass  pass:io
    def   ~(. (default-agent this %|) bowl)
    cc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  =^  cards  state  :: TODO: break into own desk, remove special casing
    (add-docket:cc %grid)
  :_  this
  :*  (~(watch-our pass /kiln) %hood /kiln/vats)
      (~(connect pass /eyre) [~ /] %docket)
      (~(connect pass /eyre) [~ /apps] %docket)
      cards
  ==
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  :_  this(state !<(state-0 vase))
  ?:  (~(has by wex.bowl) /kiln our.bowl %hood)  ~
  (~(watch-our pass /kiln) %hood /kiln/vats)^~
::
++  on-save  !>(state)
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
    ?+  mark  (on-poke:def:cc mark vase)
        %noun
      =+  ;;([%kick =desk] q.vase)
      (add-docket:cc desk)
    ::
        %handle-http-request
      =+  !<([id=@ta req=inbound-request:eyre] vase)
      :_  state
      %+  give-simple-payload:app  id
      (handle-http-request:cc req)
    ==
  [cards this]
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title [our src]:bowl)
  ?+  path  (on-watch:def path)
      [%http-response *]  [~ this]
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  [~ ~]
      [%x %dockets ~]
    :-  ~  :-  ~
    :-  %json
    !>  ^-  json
    %-  pairs:enjs:format
    %+  turn  ~(tap by dockets)
    |=  [=desk =docket]
    ^-  [cord json]
    :-  desk
    %-  pairs:enjs:format
    :~  title+s+title.docket
        color+s+(scot %ux color.docket)
        url+s+url.docket
        base+s+(spat base.docket)
    ==
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  =^  cards  state
    ?+  wire  ~|(bad-docket-take+wire !!)
      ~  `state
      [%kiln ~]  take-kiln
      [%spider @ ~]  (take-spider i.t.wire)
    ==
  [cards this]
  ::
  ++  take-spider
    |=  =desk
    ^-  (quip card _state)
    ?-  -.sign
        %poke-ack
      ?~  p.sign
        `state
      %-  (slog leaf+"glob: couldn't start thread; will retry" u.p.sign)
      `state
        %watch-ack
      ?~  p.sign
        `state
      %-  (slog leaf+"glob: couldn't listen to thread; will retry" u.p.sign)
      `state
    ::
        %kick
      `state
    ::
        %fact
      ?+    p.cage.sign  `state
          %thread-fail
        =+  !<([=term =tang] q.cage.sign)
        %-  (slog leaf+"glob: thread failed; will retry" leaf+<term> tang)
        `state
      ::
          %thread-done
        =+  !<(=glob q.cage.sign)
        =/  =docket
          (~(got by dockets) desk)
        =.  charges
          (~(put by charges) desk glob docket)
        `state
      ==
    ==
  ::
  ++  take-kiln
    ^-  (quip card _state)
    ?+  -.sign   (on-agent:def:cc wire sign)
      %kick  [(~(watch-our pass /kiln) %hood /kiln/vats)^~ state]
    ::
        %fact
      ~&  p.cage.sign
      ?.  ?=(%kiln-vats-diff p.cage.sign)  `state
      =+  !<(=diff:hood-kiln q.cage.sign)
      ~&  -.diff
      ?.  ?=(%merge -.diff)  `state
      (add-docket:cc desk.diff)
    ==
  --
::
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  =^  cards  state
    ?+  wire  (on-arvo:def wire sign-arvo)
        [%eyre ~]
      ?>  ?=([%eyre %bound *] sign-arvo)
      ?:  accepted.sign-arvo   `state 
      ~&  [dap.bowl %failed-to-bind path.binding.sign-arvo]
      `state
    ==
  [cards this]
::
++  on-fail  on-fail:def
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  `this
--
|_  =bowl:gall
+*  io    ~(. agentio bowl)
    pass  pass:io
++  def  ~(. (default-agent state %|) bowl)
::
++  add-docket
  |=  =desk
  ^-  (quip card _state)
  =/  =path
    ?:  =(%grid desk) :: XX remove
      /(scot %p our.bowl)/landscape/(scot %da now.bowl)/grid/docket
    /(scot %p our.bowl)/[desk]/(scot %da now.bowl)/desk/docket
  =+  .^(exists=? %cu path)
  ?.  exists  :: no docket
    ~&  no-docket-for-desk+desk
    `state
  =+  .^(=docket %cx path)
  =.  dockets
    (~(put by dockets) desk docket)
  =*  spi-pass  ~(. pass /spider/[desk])
  =/  tid=@t
    (cat 3 'docket-' (scot %uv (sham (mix eny.bowl desk))))
  :_  state
  :~  (watch-our:spi-pass %spider /thread-result/[tid])
      ::
      %+  poke-our:spi-pass  %spider
      :-  %spider-start
      !>([~ `tid byk.bowl(r da+now.bowl) %glob !>([~ url.docket])])
  ==
::
++  handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  %+  require-authorization-simple:app  inbound-request
  =*  req       request.inbound-request
  =*  headers   header-list.req
  =/  req-line  (parse-request-line url.req)
  ?.  =(method.req %'GET')  not-found:gen
  ?:  &(=(ext.req-line `%js) ?=([%session ~] site.req-line))
    %.  %-  as-octs:mimes:html
        (rap 3 'window.ship = "' (rsh 3 (scot %p our.bowl)) '";' ~)
    %*  .  js-response:gen
      cache  %.n
    ==
  ?.  ?=([%apps @ *] site.req-line)
    (redirect:gen '/apps/grid')
  =/  cha=(unit charge) 
    (~(get by charges) i.t.site.req-line)
  ?~  cha
    not-found:gen
  ::
  =/  suffix=^path
    (weld (slag 2 `^path`site.req-line) (drop ext.req-line))
  =/  data=mime
    (~(gut by glob.u.cha) suffix (~(got by glob.u.cha) /index/html))
  =/  mime-type=@t  (rsh 3 (crip <p.data>))
  =/  headers
    :~  content-type+mime-type 
        max-1-wk:gen 
        'service-worker-allowed'^'/'
    ==
  [[200 headers] `q.data]
--
