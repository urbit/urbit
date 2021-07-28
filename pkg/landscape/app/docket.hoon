/-  *docket
/+  agentio, default-agent, dbug, verb
|%
+$  card  card:agent:gall
+$  state
  $:  dockets=(map [=desk name=cord] docket)
      charges=(map [=desk name=cord] charge)
  ==
--
^-  agent:gall
%-  agent:dbug
%+  verb  &
=|  state
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
  :_  this
  :-  (~(watch-our pass /kiln) %hood /kiln/desks)
  (welp grid-cards landscape-cards)
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  `this(state !<(^state vase))
::
++  on-save  !>(state)
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
    ?+  mark  !!
        %noun
      =+  ;;([%kick desk=@t name=@t] q.vase)
      (add-docket:cc desk name)
    ==
  [cards this]
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  `this
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  [~ ~]
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
        :_  state  :_  ~
        %+  poke-our:pass  %file-server
        file-server-action+!>([%serve-glob base.docket glob %&])
      ==
    ==
  ::
  ++  take-kiln
    ^-  (quip card _state)
    ?+  -.sign   !!
      %watch-ack  `state
      %kick  [(~(watch-our pass /kiln) %hood /kiln/desks)^~ state]
    ::
        %fact
      =+  ;;(=desk q.q.cage.sign)
      =+  .^(files=(list path) %ct (scry:io desk /))
      =|  cards=(list card)
      |-  ^-  (quip card _state)
      ?~  files  [cards state]
      ?.  ?=([@ %docket ~] i.files)  [cards state]
      =^  crds  state
        (add-docket:cc desk i.i.files)
      $(cards (welp cards crds), files t.files)
    ==
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  `this
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
    (cat 3 'docket-' (scot %uv (sham eny.bowl)))
  :_  state
  :~  (watch-our:spi-pass %spider /thread-result/[tid])
      ::
      %+  poke-our:spi-pass  %spider
      :-  %spider-start
      !>([~ `tid byk.bowl %glob !>([~ url.docket])])
  ==
--
