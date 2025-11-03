::  sub
::
/+  *server, default-agent, verb, dbug
=,  format
::
|%
::
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 ~]
+$  action
  $%  [?(%sub %sup %bye %pok %sap) who=@p whe=term]
      [?(%flu %fla %hi) who=@p]
      [%the dat=@]
      :: [%http who=@p whe=term]
      [%hola who=@p whe=term]
      [%keen secret=? =ship =path]
      [%give ~]
  ==
--
%+  verb  |
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    :: ?.  ?=(%handle-http-request mark)
    ::   =+  !<([id=@ta req=inbound-request:eyre] vase)

    ?.  ?=(%noun mark)  ~&  bowl  (on-poke:def mark vase)
    ~&  on-poke+mark^vase
    =+  !<(=action vase)
    :_  this
    ?-    -.action
        %give  ~&(%sending [%give %fact [/http]~ json+!>(n+45)]~)
        %hola  [%pass /pok %agent [who.action whe.action] %poke *cage]~
        :: %http  [%pass /sub %agent [who.action whe.action] %poke /subs]
    ::
        %sub  :~  [%pass /sub %agent [who.action whe.action] %watch /subs]
                  ::[%pass /poko %arvo %b %wait now.bowl]
                  ::[%pass /sub %agent [who.action whe.action] %leave ~]
              ==
        %sup  [%pass /sup %agent [who.action whe.action] %watch /sups]~
        %sap  [%pass /sup %agent [who.action whe.action] %watch /sabs]~
        %bye  :~  [%pass /sub %agent [who.action whe.action] %leave ~]
                  ::[%pass /sup %agent [who.action whe.action] %leave ~]
              ==
      ::
        %flu  [%pass /flu %agent [who.action %pub] %watch /flus]~
        %fla  [%pass /flu %agent [who.action %pub] %leave ~]~
      ::
        %the
      =/  tid         `@ta`(cat 3 'thread_' (scot %uv (sham eny.bowl)))
      =/  ta-now      `@ta`(scot %da now.bowl)
      ~&  byk.bowl
      =/  start-args  [~ `tid byk.bowl %hilo-1 !>(dat.action)]
      =/  =cage       spider-start+!>(start-args)
      =/  spider      [our.bowl %spider]
      =/  =wire       /thread-result/[tid]
      ~&  tid+tid
      :~  [%pass /the/[ta-now] %agent spider %watch wire]
          [%pass /the/[ta-now] %agent spider %poke cage]
          [%pass /the/diff/[ta-now] %agent spider %watch /the/[tid]/diff]
      ==
      ::
        %pok
      [%pass /sub %agent [who.action whe.action] %poke %noun !>([%null ~])]~
      ::
        %hi
      =/  tid         `@ta`(cat 3 'thread_' (scot %uv (sham eny.bowl)))
      =/  ta-now      `@ta`(scot %da now.bowl)
      =/  =beak       byk.bowl(r da+now.bowl)
      =/  start-args  [~ `tid beak %hi !>([~ who.action])]
      =/  =cage       spider-start+!>(start-args)
      =/  spider      [our.bowl %spider]
      =/  =wire       /thread-result/[tid]
      ~&  tid+tid
      :~  [%pass /hi/[ta-now] %agent spider %watch wire]
          [%pass /hi/[ta-now] %agent spider %poke cage]
      ==
      ::
        %keen
      [%pass /my/keen %keen secret.action ship.action path.action]~
    ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  :_  this
  ~&  >>  path/path
  ?+  path  (on-watch:def path)
    [%subs ~]  ~&(subs+src.bowl ~)
    [%http ~]  ~&(subs+src.bowl ~)
  ==
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  ~&  leave+[src.bowl path]
  `this
::
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ~&  wire-on-agent/wire

  ?+  wire  (on-agent:def wire sign)
    [%pok ~]  (get-data sign)
    [%sub ~]  (get-data sign)
    [%sup ~]  (get-data sign)
    [%flu ~]  (get-data sign)
    :: [%sew *]  (get-hilo sign)
    [%the *]  (get-hilo sign)
    [%pok *]  ~&  sign+-.sign  [~ this]
    [%hi *]  ~&  hi+-.sign  [~ this]
  ==
  ::
  ++  get-data
    |=  =sign:agent:gall
    ^-  (quip card _this)
    ?+  -.sign  [~ this]
        %watch-ack
      ~&  >  %subscribed
      ?~  p.sign
        [~ this]
      =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listening to pub"
      ::  XX resusbcribe to create a loop
      %-  (slog tank u.p.sign)
      [~ this]
    ::
        %kick
      ~&  wire+"they kicked us!"
      `this
      :: :_  this
      :: [%pass /sub %agent [who.action whe.action] %watch /subs]~
      :: !!
     ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %atom
        ~&  %fact-received
        :: ?:  &  ~&  >>>  %about-to-crash  !!
        [~ this]
      ==
    ==
  ::
  ++  get-hilo
    |=  =sign:agent:gall
    ^-  (quip card _this)
    ?+    -.sign  [~ this]
        %poke-ack
      ?~  p.sign
        %-  (slog leaf+"Thread started successfully" ~)
        [~ this]
      %-  (slog leaf+"Thread failed to start" u.p.sign)
      [~ this]
      ::
        %fact
      ?+    p.cage.sign  (on-agent:def wire sign)
          %thread-fail
        =/  err  !<  (pair term tang)  q.cage.sign
        %-  (slog leaf+"Thread failed: {(trip p.err)}" q.err)
        `this
      ::
          %thread-done
        =/  res  (trip !<(term q.cage.sign))
        %-  (slog leaf+"Result: {res}" ~)
        `this
      ::
           %diff
         =/  msg  !<  tape  q.cage.sign
         %-  (slog leaf+msg ~)
         `this
      ==
    ==
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ~&  wire-on-arvo/wire^sign-arvo
  ?:  ?=([%poko ~] wire)
    (on-poke %noun !>([%sub ~fyr %pub]))
  ?.  ?=([%ames %sage *] sign-arvo)  `this
  ?~   q.sage.sign-arvo
    ~&  no-item/sage.sign-arvo
    `this
  =/  =path
    /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.q.sage.sign-arvo]
  =+  .^  =dais:clay  %cb
    path
  ==
  :_  this
  [%pass /flog %arvo %d %flog %text (noah ;;(vale.dais q.q.sage.sign-arvo))]~
++  on-fail   on-fail:def
--
