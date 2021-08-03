/-  *docket
/+  *server, agentio, default-agent, dbug, verb
|%
+$  card  card:agent:gall
+$  state-0
  $:  dockets=(map warrant docket)
      charges=(map warrant charge)
  ==
::
+$  cache
  (map path warrant)
::
+$  inflated-state
  [state-0 cache]
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
  =^  grid-cards  state
    (add-docket:cc q.byk.bowl %grid)
  =^  landscape-cards  state
    (add-docket:cc q.byk.bowl %landscape)
  =^  btc-cards  state
    (add-docket:cc q.byk.bowl %btc)
  :_  this
  :*  (~(watch-our pass /kiln) %hood /kiln/desks)
      (~(connect pass /eyre) [~ /] %docket)
      (~(connect pass /eyre) [~ /apps] %docket)
      :(welp grid-cards landscape-cards btc-cards)
  ==
::
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  `this(state !<(state-0 vase))
::
++  on-save  !>(state)
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
    ?+  mark  (on-poke:def:cc mark vase)
        %noun
      =+  ;;([%kick desk=@t name=@t] q.vase)
      (add-docket:cc desk name)
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
    |=  [[=desk name=cord] =docket]
    ^-  [cord json]
    :-  (rap 3 desk '/' name ~)
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
      [%spider @ @ ~]  (take-spider [i i.t]:t.wire)
    ==
  [cards this]
  ::
  ++  take-spider
    |=  [=desk name=cord]
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
          (~(got by dockets) desk name)
        =.  charges
          (~(put by charges) [desk name] glob docket)
        `state
      ==
    ==
  ::
  ++  take-kiln
    |^  ^-  (quip card _state)
    ?+  -.sign   (on-agent:def:cc wire sign)
      %kick  [(~(watch-our pass /kiln) %hood /kiln/desks)^~ state]
    ::
        %fact
      =+  ;;(=desk q.q.cage.sign)
      =/  desk-dockets=(set [=^desk name=cord])
        %-  ~(gas in *(set [=^desk name=cord]))
        %+  murn
          .^((list path) %ct (scry:io desk /))
        |=  =path
        ?.  ?=([@ %docket ~] path)  ~
        `[desk i.path]
      =^  add-cards  state
        (add desk-dockets)
      =^  rm-cards  state
        (remove desk-dockets)
      :_  state
      (welp add-cards rm-cards)
    ==
    ::
    ++  add
      |=  updates=(set [=desk name=cord])
      ^-  (quip card _state)
      =/  dockets=(list [=desk name=^cord])  ~(tap in updates)
      =|  cards=(list card)
      |-  
      ?~  dockets  [cards state]
      =^  crds  state
        (add-docket:cc i.dockets)
      $(cards (welp crds cards), dockets t.dockets)
    ::
    ++  remove
      |=  updates=(set [=desk name=cord])
      ^-  (quip card _state)
      `state
    --
  --
::
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  =^  cards  state
    ?+  wire  (on-arvo:def wire sign-arvo)
        [%delay @ @ ~]
      ?>  ?=([%behn %wake *] sign-arvo)
      (add-docket:cc [i i.t]:t.wire)
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
++  delay-add
  |=  [=desk name=cord]
  ^-  (quip card _state)
  :_  state
  (~(wait pass /delay/[desk]/[name]) (add now.bowl ~s1))^~
::
++  add-docket
  |=  [=desk name=cord]
  ^-  (quip card _state)
  =/  =path
      /(scot %p our.bowl)/[desk]/(scot %da now.bowl)/[name]/docket
  =+  .^(exists=? %cu path)
  ?.  exists  :: no docket
    ~&  no-docket-for-desk+desk
    `state
  =+  .^(=docket %cx path)
  =.  dockets
    (~(put by dockets) [desk name] docket)
  =*  spi-pass  ~(. pass /spider/[desk]/[name])
  =/  tid=@t
    (cat 3 'docket-' (scot %uv (sham :(mix eny.bowl desk name))))
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
  ?.  ?=([%apps @ @ *] site.req-line)
    (redirect:gen '/apps/landscape/grid')
  =/  cha=(unit charge) 
    (~(get by charges) [i i.t]:t.site.req-line)
  ?~  cha
    not-found:gen
  ::
  =/  suffix=^path
    (weld (slag 3 `^path`site.req-line) (drop ext.req-line))
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
