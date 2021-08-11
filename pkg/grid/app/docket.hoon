/-  *docket
/+  *server, agentio, default-agent, dbug, verb, hood-kiln=kiln
|%
+$  card  card:agent:gall
+$  state-0
  $:  dockets=(map desk docket)
      charges=(map desk charge)
      treaties=(map [=ship =desk] treaty)
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
    dc-abed  dc-abed:dock-core:cc
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  (~(watch-our pass /kiln) %hood /kiln/vats)
      (~(connect pass /eyre) [~ /] %docket)
      (~(connect pass /eyre) [~ /apps] %docket)
      (poke-self:pass init+!>(~))
  ==
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =+  !<(old=state-0 vase)
  |^  
  =.  -.state  old
  =.  +.state  inflate-cache
  `this
  ::  
  ++  inflate-cache
    ^-  cache
    %-  ~(gas by *(map term desk))
    %+  turn  ~(tap by charges)
    |=  [=desk =charge]
    :_(desk base.docket.charge)
  --
::
++  on-save  !>(-.state)
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
    ?+  mark  (on-poke:def:cc mark vase)
      %docket-uninstall  dc-abet:dc-uninstall:(dc-abed !<(desk vase)) 
    ::
        %init   ::  must delay poke until after commit
      =^  g-cards  state  :: TODO: break into own desk, remove special casing
          dc-abet:dc-install:(dc-abed %grid)
      =^  l-cards  state
        dc-abet:dc-install:(dc-abed %landscape)
      [(weld g-cards l-cards) state]
    ::
        %noun
      =+  ;;([%kick =desk] q.vase)
      dc-abet:dc-install:(dc-abed desk)
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
  =^  cards  state
    ?+  path  (on-watch:def path)
        [%http-response *]  
      ?>  (team:title [our src]:bowl)
      `state
    ::
        [%treaty @ @ ~]
      =*  desk  i.t.t.path
      =/  =ship  (slav %p i.t.path)
      ?>  |(&(!=(our.bowl ship) (team:title [src our]:bowl)) =(our.bowl ship))
      sy-abet:sy-peer:(sy-abed:sync-core:cc ship desk)
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
    [%x %our ~]  ``json+!>(s+(scot %p our.bowl))
    ::
      [%x %dockets ~]
    :-  ~  :-  ~
    :-  %docket-update
    !>  ^-  update
    [%initial dockets]
    ::
      [%x %charges ~]
    :-  ~  :-  ~
    :-  %docket-update
    !>  ^-  update
    :-  %initial
    %-  ~(gas by *(map desk docket))
    %+  turn  ~(tap by charges)
    |=  [=desk =charge]
    [desk docket.charge]
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
      [%docket @ *]  dc-abet:(dc-take-agent:(dc-abed i.t.wire) t.t.wire sign)
    ::
        [%treaty @ @ ~]
      =*  desk  i.t.t.wire
      =/  =ship  (slav %p i.t.wire)
      sy-abet:(sy-take:(sy-abed:sync-core:cc ship desk) sign)
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
      =+  !<(=diff:hood-kiln q.cage.sign)
      ?.  &(?=(%merge -.diff) !(~(has by dockets) desk.diff))  `state
      dc-abet:dc-install:(dc-abed desk.diff)
    ==
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  =^  cards  state
    ?+  wire  (on-arvo:def wire sign-arvo)
    ::
        [%docket @ *]  
      dc-abet:(dc-take-arvo:(dc-abed i.t.wire) t.t.wire sign-arvo)
    ::
        [%eyre ~]
      ?>  ?=([%eyre %bound *] sign-arvo)
      ?:  accepted.sign-arvo   `state 
      ~&  [dap.bowl %failed-to-bind path.binding.sign-arvo]
      `state
    ==
  [cards this]
::
++  on-fail  on-fail:def
++  on-leave  on-leave:def
--
|_  =bowl:gall
+*  io    ~(. agentio bowl)
    pass  pass:io
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
  =/  suffix=^path
    (weld (slag 2 `^path`site.req-line) (drop ext.req-line))
  ?:  =(suffix /desk/js) 
    %-  inline-js-response
    (rap 3 'window.desk = "' u.des '";' ~)
  =/  data=mime
    (~(gut by glob.u.cha) suffix (~(got by glob.u.cha) /index/html))
  =/  mime-type=@t  (rsh 3 (crip <p.data>))
  =/  headers
    :~  content-type+mime-type 
        max-1-wk:gen 
        'service-worker-allowed'^'/'
    ==
  [[200 headers] `q.data]
::  +dock-core: Local docket engine
++  dock-core
  |_  $:  cards=(list card)
          =desk  
          docket=(unit docket)
          charge=(unit charge)
      ==
  ++  dc-core  .
  ++  dc-abed  
    |=  d=^desk
    dc-core(desk d, docket (~(get by dockets) d), charge (~(get by charges) d))
  ++  dc-abet  
    =/  old-charge  (~(get by charges) desk)
    =?  by-base  &(=(old-charge charge) ?=(^ old-charge))
      ?~  charge  (~(del by by-base) base.docket.u.old-charge)
      (~(put by by-base) base.docket.u.charge desk)
    =:  charges  ?~(charge (~(del by charges) desk) (~(put by charges) desk u.charge))
        dockets  ?~(docket (~(del by dockets) desk) (~(put by dockets) desk u.docket))
      ==
    [(flop cards) state]
  ::
  ++  dc-emit  |=(=card dc-core(cards [card cards]))
  ++  dc-emil  |=(crds=(list card) dc-core(cards (welp (flop crds) cards)))
  ++  dc-pass  |=(=path ~(. pass (welp /docket/[desk] path)))
  ::
  ::  +|  %entrypoints
  ++  dc-install
    =.  dc-core  (dc-log "installing {<desk>}")
    =/  =path
      ?:  =(%grid desk) :: XX remove
        /(scot %p our.bowl)/landscape/(scot %da now.bowl)/grid/docket
      /(scot %p our.bowl)/[desk]/(scot %da now.bowl)/desk/docket
    =+  .^(exists=? %cu path)
    ?.  exists  :: no docket
      ~&  no-docket-for-desk+desk
      dc-core
    (dc-new .^(dock=^docket %cx path))
  ::
  ++  dc-uninstall
    =>  dc-gone
    uninstall:dc-kiln
  ::
  ++  dc-take-agent
    |=  [=wire =sign:agent:gall]
    |^
    ?+  wire   ~|(%bad-docket-take !!)
      [%spider *]  (take-spider t.wire)
      [%kiln *]    dc-core
    ==
    ++  take-spider
      |=  wire=^wire
      ?-  -.sign
        %kick   dc-core
          ?(%poke-ack %watch-ack)
        ?~  p.sign  dc-core
        =/  act=tape  ?:(?=(%poke-ack -.sign) "start" "listen")
        (dc-slog leaf+"docket: couldn't {act} thread; will retry" u.p.sign)
      ::
          %fact
        ?+    p.cage.sign  dc-core
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          (dc-slog leaf+"docket: thread failed; will retry" leaf+<term> tang)
        ::
            %thread-done
          =+  !<(=glob q.cage.sign)
          =.  charge  `[glob dc-docket]
          dc-give-charge
        ==
      ==
    --
  ++  dc-take-arvo
    |=  [=wire =sign-arvo]
    ?>  ?=([?(%clay %behn) %writ *] sign-arvo)
    ?:  =(~ docket)  dc-core  :: uninstalled
    =.  dc-core  dc-warp-docket
    ?~(p.sign-arvo dc-gone (dc-update u.p.sign-arvo))
  ::
  ::  +|  %transitions
  ::
  ::  +dc-gone: Uninstall
  ++  dc-gone  
    =:  docket  ~
        charge  ~
      ==
    dc-give-charge
  ::  
  ::  +dc-update: Handle new docket from clay
  ++  dc-update
    |=  =rant:clay
    =*  cage  r.rant
    ?.  ?=(%docket p.cage)  ~|(%bad-rant-mark !!)
    (dc-new !<(dock=^docket q.cage))
  ::
  ::  +dc-new: Handle new docket
  ++  dc-new
    |=  dock=^docket
    =.  dc-core  (dc-log "new docket for {<desk>}")
    =.  docket  `dock
    =?  dc-core  |(?=(~ charge) !=(glob.docket.u.charge glob.dock))
      dc-start-thread  :: only refetch if changed
    dc-warp-docket
  ::  +|  %card
  ++  dc-warp-docket
    (dc-emit (warp-our:(dc-pass /warp) desk `[%next %x da+now.bowl /desk/docket]))
  ++  dc-start-thread
    =/  tid=@t  (cat 3 'docket-' (scot %uv (sham (mix eny.bowl desk))))
    =*  glob-url   glob:dc-docket
    =/  =cage  spider-start+!>([~ `tid byk.bowl(r da+now.bowl) %glob !>(`glob-url)])
    =*  pass  (dc-pass /spider)
    =.  dc-core  (dc-emit (watch-our:pass %spider /thread-result/[tid]))
    (dc-emit (poke-our:pass %spider cage))
  ::
  ++  dc-give-charge
    =;  =update
      (dc-emit (fact:io docket-update+!>(update) /charges ~))
    ?~(charge [%del-dock desk] [%add-dock desk docket.u.charge])
  ::
  ++  dc-kiln
    |%
    ++  pass  (dc-pass /kiln)
    ++  uninstall  (dc-emit (poke-our:pass %hood kiln-uninstall+!>(desk)))
    ++  install  |=(=ship (dc-emit (poke-our:pass %hood kiln-install+!>([ship desk]))))
    --
  ::
  ::  +|  %constants/utils
  ++  dc-log   |=(=tape ?:(lac dc-core ((slog leaf+"docket: {tape}" ~) dc-core)))
  ++  dc-slog  |=(=tang ((slog tang) dc-core))
  ++  dc-docket  (need docket)
  ++  dc-charge  (need charge)
  --
::  +sync-core: Treaty engine
++  sync-core
  |_  $:  =ship  =desk
          cards=(list card)
      ==
  ++  sy-core  .
  ++  sy-abed  |=([s=^ship d=^desk] sy-core(ship s, desk d))
  ++  sy-abet  [(flop cards) state]
  ++  sy-emit  |=(=card sy-core(cards [card cards]))
  ++  sy-emil  |=(crds=(list card) sy-core(cards (welp (flop crds) cards)))
  ++  sy-path  /treaty/(scot %p ship)/[desk]
  ++  sy-pass  ~(. pass sy-path)
  ::
  ::  %|  entrypoints
  ++  sy-peer
    ^+  sy-core
    ?:  =(our.bowl ship)  sy-local-treaty
    ?>  (team:title [our src]:bowl)
    ?.  (~(has by treaties) [ship desk])
      sy-watch-foreign
    (sy-emit (fact:io treaty+!>((~(got by treaties) [ship desk])) sy-path ~))
  ::
  ++  sy-take
    |=  =sign:agent:gall
    ^+  sy-core
    ?+  -.sign  sy-core
      %kick   sy-watch-foreign
    ::
        %watch-ack   
      ?~  p.sign  sy-core
      %-  (slog leaf+"docket-sync: couldn't subscribe to foreign" u.p.sign)
      sy-core
    ::
        %fact
      ?.  ?=(%treaty p.cage.sign)  sy-core
      =+  !<(=treaty q.cage.sign)
      =.  treaties  (~(put by treaties) [ship desk] treaty)
      (sy-give treaty)
    ==
  ::  +|  %cards
  ++  sy-watch-foreign  (sy-emit (watch:sy-pass [ship %docket] sy-path))
  ++  sy-give-gone  (sy-emit (kick:io sy-path ~))
  ++  sy-give  |=(=treaty (sy-emit (fact:io treaty+!>(treaty) sy-path ~)))
  ++  sy-local-treaty
    =/  =docket  (~(got by dockets) desk)
    =+  .^(=cass:clay %cw (scry:io desk /desk/docket))
    =+  .^(hash=@uv %cz (scry:io desk ~))
    (sy-give our.bowl desk da+da.cass hash docket)
  --
--
