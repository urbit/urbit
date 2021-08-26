/-  *docket, hood, treaty
/+  *server, agentio, default-agent, dbug, verb
|%
+$  card  card:agent:gall
+$  state-0
  $:  ::  local
      charges=(map desk charge)
  ==
::  $cache: impermanent state
+$  cache
  by-base=(map term desk)
::
+$  inflated-state
  [state-0 cache]
::  +lac: toggle verbosity
++  lac  &
::
++  ver
  |%
  ++  poke  1
  ++  scry  1
  ++  peer  1
  --
::
--
^-  agent:gall
%-  agent:dbug
%+  verb  &
=|  inflated-state
=*  state  -
=<
|_  =bowl:gall
+*  this  .
    io    ~(. agentio bowl)
    pass  pass:io
    def   ~(. (default-agent this %|) bowl)
    cc    ~(. +> bowl)
    ch    ch:cc
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  (~(watch-our pass /kiln) %hood /kiln/vats)
      (~(connect pass /eyre) [~ /] %docket)
      (~(wait pass /init) (add 1 now.bowl))
      (~(connect pass /eyre) [~ /apps] %docket)
  ==
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =+  !<(old=state-0 vase)
  =*  cha  ~(. ch q.byk.bowl)
  |^  
  =.  -.state  old
  =.  +.state  inflate-cache
  `this
  ::  
  ++  inflate-cache
    ^-  cache
    %-  ~(gas by *(map term desk))
    %+  murn  ~(tap by charges)
    |=  [=desk =charge]
    ?.  ?=(%glob -.href.docket.charge)  ~
    `:_(desk base.href.docket.charge)
  --
::
++  on-save  !>(-.state)
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^  
  =^  cards  state
    ?+  mark  (on-poke:def:cc mark vase)
      %docket-install    (install !<([ship desk] vase))
      %docket-uninstall  (uninstall !<(desk vase))
    ::
        %noun
      =+  ;;([%kick =desk] q.vase)
      :_(state ~(fetch-glob ch desk))
    ::
        %handle-http-request
      =+  !<([id=@ta req=inbound-request:eyre] vase)
      :_  state
      %+  give-simple-payload:app  id
      (handle-http-request:cc req)
    ==
  [cards this]
  ::
  ++  install
    |=  [=ship =desk]
    ^-  (quip card _state)
    =+  .^(=treaty:treaty %gx (scry:io %treaty /treaty/(scot %p ship)/[desk]/noun))
    ?<  ~|(%bad-install-desk (~(has by charges) desk)) 
    =.  charges
      (~(put by charges) desk docket.treaty %install ~)
    =*  cha   ~(. ch desk)
    :_  state
    ~[add-fact:cha (install:cha ship desk)]
  ::
  ++  uninstall
    |=  =desk
    ^-  (quip card _state)
    ~|  %no-charge-install
    =/  =charge  (~(got by charges) desk)
    =.  charges  (~(del by charges) desk)
    =?  by-base  ?=(%glob -.href.docket.charge)
      (~(del by by-base) base.href.docket)
    =*  cha  ~(. ch desk)
    :_  state
    ~[del-fact:cha uninstall:cha]
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  =^  cards  state
    ?+  path  (on-watch:def path)
        [%http-response *]  
      ?>  (team:title [our src]:bowl)
      `state
    ::
        [%charges ~]
      ?>  (team:title [our src]:bowl)
      `state
    ==
  [cards this]
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  [~ ~]
    [%x %ver %poke ~]  ``noun+!>(poke:ver)
    [%x %ver %peer ~]  ``noun+!>(peer:ver)
    [%x %ver %scry ~]  ``noun+!>(scry:ver)
    [%x %our ~]  ``json+!>(s+(scot %p our.bowl))
    ::
      [%x %dbug %state ~]
    =-  ``noun+!>(-)
    %_  state
        charges
      %-  ~(run by charges)
      |=  =charge
      =?  chad.charge  ?=(%glob -.chad.charge)
        [%glob *glob]
      charge
    ==
    ::
      [%x %charges ~]
    :-  ~  :-  ~
    :-  %charge-update
    !>  ^-  charge-update
    :-  %initial
    %-  ~(gas by *(map desk charge))
    %+  turn  ~(tap by charges)
    |=  [=desk =charge]
    [desk (get-light-charge charge)]
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  =^  cards  state
    ?+  wire  ~&(bad-docket-take+wire `state)
      ~  `state
      [%kiln ~]  take-kiln
      [%charge @ *]  (take-charge i.t.wire t.t.wire)
    ==
  [cards this]
  ::
  ++  take-kiln
    ^-  (quip card _state)
    ?+  -.sign   (on-agent:def:cc wire sign)
      %kick  [(~(watch-our pass /kiln) %hood /kiln/vats)^~ state]
    ::
        %fact
      ?.  ?=(%kiln-vats-diff p.cage.sign)  `state
      =+  !<(=diff:hood q.cage.sign)
      =*  cha  ~(. ch desk.diff)
      ?+  -.diff  `state
      ::
          %merge
        =*  cha  ~(. ch desk.diff)
        ?.  docket-exists:cha  `state
        =/  =docket  docket:cha
        ?:  ?=(%site -.href.docket)
          :_  state(charges (~(put by charges) desk.diff [docket [%site ~]]))
          ~[add-fact:cha]
        =.  charges  (~(put by charges) desk.diff [docket %install ~])
        =.  by-base  (~(put by by-base) base.href.docket desk.diff)
        :_  state
        [add-fact:cha fetch-glob:cha]
      ::
          %suspend
        ?.  (~(has by charges) desk.diff)  `state
        =.  charges  (new-chad:cha %suspend ~)
        :_(state ~[add-fact:cha])
      ::
          %revive
        ?.  (~(has by charges) desk.diff)  `state
        =/  =charge  (~(got by charges) desk.diff)
        ?.  ?=(%glob -.href.docket.charge)
          =.  charges  (new-chad:cha %site ~)
          :_(state ~[add-fact:cha])
        =.  charges  (new-chad:cha %install ~)
        :_(state [add-fact fetch-glob]:cha)
      ==
    ==
  ++  take-charge
    |=  [=desk =^wire]
    ^-  (quip card _state)
    ~|  %took-for-nonexistent-charge
    ?>  (~(has by charges) desk)
    =*  cha  ~(. ch desk)
    ?+  wire  ~|(%bad-charge-wire !!)
    ::
        [%install ~]
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign  
        `state
      =.  charges   (new-chad:cha hung+'Failed install')
      ((slog leaf+"Failed installing %{(trip desk)}" u.p.sign) `state)
    ::
        [%uninstall ~] 
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign  `state
      ((slog leaf+"Failed to uninstall %{(trip desk)}" u.p.sign) `state)
    ::
        [%glob ~]
      ?-  -.sign
        %kick   `state
      ::
          ?(%poke-ack %watch-ack)
        ?~  p.sign  `state
        =/  act=tape  ?:(?=(%poke-ack -.sign) "start" "listen")
        =.  charges  (new-chad:cha hung+'glob-failed')
        :-  ~[add-fact:cha]
        ((slog leaf+"docket: couldn't {act} thread; will retry" u.p.sign) state)
      ::
          %fact
        ?+    p.cage.sign  `state
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          =.  charges  (new-chad:cha hung+'glob-failed')
          :-  ~[add-fact:cha]
          ((slog leaf+"docket: thread failed; will retry" leaf+<term> tang) state)
        ::
            %thread-done
          =+  !<(=glob q.cage.sign)
          =/  =charge  (~(got by charges) desk)
          ?>  ?=(%glob -.href.docket.charge)
          =.  charges  (new-chad:cha glob+glob)
          =.  by-base  (~(put by by-base) base.href.docket.charge desk)
          :_(state ~[add-fact:cha])
        ==
      ==
    ==
  --
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  =^  cards  state
    ?+  wire  (on-arvo:def wire sign)
        [%init ~]  
      =*  cha  ~(. ch q.byk.bowl)
      =.  charges  (~(put by charges) q.byk.bowl [docket:cha %install ~])
      [fetch-glob:cha state]
    ::
        [%eyre ~]
      ?>  ?=([%eyre %bound *] sign)
      ?:  accepted.sign   `state 
      ~&  [dap.bowl %failed-to-bind path.binding.sign]
      `state
    ==
  [cards this]
::
++  on-fail  on-fail:def
++  on-leave  on-leave:def
--
|_  =bowl:gall
++  io    ~(. agentio bowl)
++  pass  pass:io
++  def  ~(. (default-agent state %|) bowl)
::
++  inline-js-response
  |=  js=cord
  ^-  simple-payload:http
  %.  (as-octs:mimes:html js)
  %*  .  js-response:gen
    cache  %.n
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
    %-  inline-js-response 
    (rap 3 'window.ship = "' (rsh 3 (scot %p our.bowl)) '";' ~)
  ?.  ?=([%apps @ *] site.req-line)
    (redirect:gen '/apps/grid/')
  =/  des=(unit desk)
    (~(get by by-base) i.t.site.req-line)
  ?~  des  not-found:gen
  =/  cha=(unit charge) 
    (~(get by charges) u.des)
  ?~  cha  not-found:gen
  ?.  ?=(%glob -.chad.u.cha)  not-found:gen
  =*  glob  glob.chad.u.cha
  =/  suffix=^path
    (weld (slag 2 `^path`site.req-line) (drop ext.req-line))
  ?:  =(suffix /desk/js) 
    %-  inline-js-response
    (rap 3 'window.desk = "' u.des '";' ~)

  =/  data=mime
    (~(gut by glob) suffix (~(got by glob) /index/html))
  =/  mime-type=@t  (rsh 3 (crip <p.data>))
  =/  headers
    :~  content-type+mime-type 
        max-1-wk:gen 
        'service-worker-allowed'^'/'
    ==
  [[200 headers] `q.data]
::
++  get-light-charge
  |=  =charge
  ?.  ?=(%glob -.chad.charge)  charge
  charge(glob.chad *glob)
::  +ch: Charge engine
++  ch
  |_  =desk
  ++  pass  |=(slug=term ~(. ^pass /charge/[desk]/[slug]))
  ++  add-fact
    =/  =charge  (~(got by charges) desk)
    (fact:io charge-update+!>([%add-charge desk (get-light-charge charge)]) /charges ~)
  ++  del-fact  (fact:io charge-update+!>([%del-charge desk]) /charges ~)
  ++  install
    |=  [=ship remote=^desk]
    (poke-our:(pass %install) %hood kiln-install+!>([desk ship remote]))
  ++  uninstall
    (poke-our:(pass %uninstall) %hood kiln-uninstall+!>(desk))
  ++  new-chad  |=(c=chad (~(jab by charges) desk |=(charge +<(chad c))))
  ++  fetch-glob
    =/  =charge  (~(got by charges) desk)
    =/  tid=@t  (cat 3 'docket-' (scot %uv (sham (mix eny.bowl desk))))
    ?>  ?=(%glob -.href.docket.charge)
    ?>  ?=(%http -.glob-location.href.docket.charge)
    =*  url  url.glob-location.href.docket.charge
    =/  =cage  spider-start+!>([~ `tid byk.bowl(r da+now.bowl) %glob !>(`url)])
    :~  (watch-our:(pass %glob) %spider /thread-result/[tid])
        (poke-our:(pass %glob) %spider cage)
    ==
  ++  docket-exists  .^(? %cu (scry:io desk /desk/docket))
  ++  docket  .^(^docket %cx (scry:io desk /desk/docket))
  --
--

