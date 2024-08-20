::  glob [landscape]:
::
::  prompts content delivery and Gall state storage for Landscape JS blob
::
/-  glob, *resource
/+  default-agent, verb, dbug
|%
++  landscape-hash  0v3.sdoer.mnnfi.opjrg.npmcj.utr8l
++  btc-wallet-hash  0v758lj.uf0s5.0nh3m.gunn6.942gj
+$  state-0  [%0 hash=@uv glob=(unit (each glob:glob tid=@ta))]
+$  state-1  [%1 =globs:glob]
+$  all-states
  $%  state-0
      state-1
  ==
+$  card  card:agent:gall
--
|%
++  wait-timeout
  |=  [=path now=@da]
  ^-  card
  [%pass [%timer path] %arvo %b %wait (add now ~m30)]
::
++  wait-start
  |=  [now=@da =path]
  ^-  card
  [%pass [%start path] %arvo %b %wait now]
::
++  poke-file-server
  |=  [our=@p hash=@uv =cage]
  ^-  card
  [%pass /serving/(scot %uv hash) %agent [our %file-server] %poke cage]
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
::
++  leave-spider
  |=  [=path our=@p]
  ^-  card
  [%pass [%running path] %agent [our %spider] %leave ~]
--
=|  state=state-1
=.  globs.state
  (~(put by globs.state) /'~landscape'/js/bundle landscape-hash ~)
=.  globs.state
  (~(put by globs.state) /'~btc'/js/bundle btc-wallet-hash ~)
::
^-  agent:gall
%+  verb  |
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
    bec   byk.bowl(r da+now.bowl)
++  on-init
  ^-  (quip card _this)
  ::  delay through timer to make sure %spider has started
  :_  this
  %+  turn  ~(tap by ~(key by globs.state))
  |=(=path (wait-start now.bowl path))
::
++  on-save   !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  =+  !<(old=all-states old-state)
  =|  cards=(list card)
  =/  upgrading=?  %.n
  |-
  ?-  -.old
      %1
    =/  [cards-1=(list card) =globs:glob]
      %-  ~(rep by globs.old)
      |=  $:  [=serve=path =glob-details:glob]
              cards=(list card)
              globs=_globs.state
          ==
      ^-  [(list card) globs:glob]
      =/  new-glob-details  (~(get by globs) serve-path)
      ?~  new-glob-details
        [cards globs]
      ?~  glob.glob-details
        :_  globs
        [(wait-start now.bowl serve-path) cards]
      ?:  ?=(%& -.u.glob.glob-details)
        ?:  =(hash.u.new-glob-details hash.glob-details)
          [cards (~(put by globs) serve-path glob-details)]
        :_  globs
        [(wait-start now.bowl serve-path) cards]
      ?:  upgrading
        :_  globs
        [(wait-start now.bowl serve-path) cards]
      =/  args  [tid.p.u.glob.glob-details &]
      =/  spider-wire  [(scot %uv hash.glob-details) serve-path]
      :_  globs
      :*  (leave-spider spider-wire our.bowl)
          (poke-spider spider-wire our.bowl %spider-stop !>(args))
          (wait-start now.bowl serve-path)
          cards
      ==
    :-  (weld cards cards-1)
    this(globs.state globs)
  ::
      %0
    =/  globs
      (~(put by globs.state) /'~landscape'/js/bundle [hash.old glob.old])
    %=  $
        old  [%1 globs]
    ::
        cards
      ?~  glob.old  ~
      ?:  =(%& -.u.glob.old)  ~
      ?>  ?=(%| -.u.glob.old)
      =/  args  [tid.p.u.glob.old &]
      :~  (leave-spider /(scot %uv hash.old) our.bowl)
          (poke-spider /(scot %uv hash.old) our.bowl %spider-stop !>(args))
      ==
    ::
        upgrading  %.y
    ==
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      %glob-make
    =+  !<(dir=path vase)
    :_  this
    =/  home=path  /(scot %p our.bowl)/home/(scot %da now.bowl)
    =+  .^(paths=(list path) %ct (weld home dir))
    =+  .^(=js=tube:clay %cc (weld home /js/mime))
    =+  .^(=map=tube:clay %cc (weld home /map/mime))
    =/  =glob:glob
      %-  ~(gas by *glob:glob)
      %+  turn  paths
      |=  pax=path
      ^-  [path mime]
      =+  .^(file=@t %cx (weld home pax))
      =/  mar  (snag 0 (flop pax))
      :-  (slag (lent dir) pax)
      ?+  mar  ~|(unsupported-glob-type+mar !!)
        %js   !<(mime (js-tube !>(file)))
        %map  !<(mime (map-tube !>(file)))
      ==
    =/  =path  /(cat 3 'glob-' (scot %uv (sham glob)))/glob
    ~&  globbed+`(set ^path)`~(key by glob)
    [%pass /make %agent [our.bowl %hood] %poke %drum-put !>([path (jam glob)])]~
  ::
      %noun
    ?:  =(%kick -.q.vase)
      =+  !<([%kick =path] vase)
      =/  glob-details  (~(get by globs.state) path)
      ?~  glob-details
        ~&  no-such-glob+path
        `this
      =/  new-state
        state(globs (~(put by globs.state) path *@uv glob.u.glob-details))
      (on-load !>(new-state))
    (on-poke:def mark vase)
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    [%x %btc-wallet ~]  ``noun+!>(btc-wallet-hash)
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?:  ?=([%serving @ ~] wire)
    (on-agent:def wire sign)
  ?:  ?=([%make ~] wire)
    (on-agent:def wire sign)
  ?.  ?=([%running @ *] wire)
    %-  (slog leaf+"glob: strange on-agent! {<wire -.sign>}" ~)
    (on-agent:def wire sign)
  ::
  =/  produced-hash  (slav %uv i.t.wire)
  =*  serve-path     t.t.wire
  =/  glob-details   (~(get by globs.state) serve-path)
  ?~  glob-details
    [~ this]
  ?.  =(hash.u.glob-details produced-hash)
    [~ this]
  ?-    -.sign
      %poke-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"glob: couldn't start thread; will retry" u.p.sign)
    :_  this(globs.state (~(put by globs.state) serve-path produced-hash ~))
    [(leave-spider t.wire our.bowl)]~
  ::
      %watch-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"glob: couldn't listen to thread; will retry" u.p.sign)
    [~ this(globs.state (~(put by globs.state) serve-path produced-hash ~))]
  ::
      %kick
    ?.  ?=([~ %| *] glob.u.glob-details)
      `this
    [~ this(globs.state (~(put by globs.state) serve-path produced-hash ~))]
  ::
      %fact
    ?+    p.cage.sign  (on-agent:def wire sign)
        %thread-fail
      =+  !<([=term =tang] q.cage.sign)
      %-  (slog leaf+"glob: thread failed; will retry" leaf+<term> tang)
      :-  ~
      this(globs.state (~(put by globs.state) serve-path produced-hash ~))
    ::
        %thread-done
      =+  !<(=glob:glob q.cage.sign)
      ?.  =(hash.u.glob-details (sham glob))
        %:  mean
          leaf+"glob: hash doesn't match!"
          >expected=hash.u.glob-details<
          >got=(sham glob)<
          ~
        ==
      =.  globs.state
        (~(put by globs.state) serve-path produced-hash `[%& glob])
      :_  this  :_  ~
      %:  poke-file-server
          our.bowl
          produced-hash
          %file-server-action
          !>([%serve-glob serve-path glob %&])
      ==
    ==
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?:  ?=([%start *] wire)
    =*  serve-path  t.wire
    =/  glob-details  (~(get by globs.state) serve-path)
    ?~  glob-details
      [~ this]
    =/  new-tid=@ta  (cat 3 'glob--' (scot %uv (sham eny.bowl serve-path)))
    =/  args  [~ `new-tid bec %glob !>([~ hash.u.glob-details])]
    =/  action=cage  [%file-server-action !>([%unserve-dir serve-path])]
    =/  spider-wire  [(scot %uv hash.u.glob-details) serve-path]
    =.  globs.state
      (~(put by globs.state) serve-path hash.u.glob-details `[%| new-tid])
    :_  this
    :~  (poke-file-server our.bowl hash.u.glob-details action)
        (wait-timeout [new-tid serve-path] now.bowl)
        (watch-spider spider-wire our.bowl /thread-result/[new-tid])
        (poke-spider spider-wire our.bowl %spider-start !>(args))
    ==
  ::
  ?.  ?=([%timer @ *] wire)
    %-  (slog leaf+"glob: strange on-arvo wire: {<wire [- +<]:sign-arvo>}" ~)
    `this
  ?.  ?=(%wake +<.sign-arvo)
    %-  (slog leaf+"glob: strange on-arvo sign: {<wire [- +<]:sign-arvo>}" ~)
    `this
  =*  serve-path  t.wire
  =/  glob-details  (~(get by globs.state) serve-path)
  ?~  glob-details
    `this
  ?:  ?=([~ %& *] glob.u.glob-details)
    `this
  ?.  ?|  ?=(~ glob.u.glob-details)
          =(i.t.wire tid.p.u.glob.u.glob-details)
      ==
    `this
  ?^  error.sign-arvo
    %-  (slog leaf+"glob: timer handling failed; will retry" ~)
    [[(wait-timeout t.wire now.bowl)]~ this]
  %-  (slog leaf+"glob: timed out; retrying" ~)
  =/  new-details  u.glob-details(hash *@uv)
  =/  new-state    state(globs (~(put by globs.state) serve-path new-details))
  (on-load !>(new-state))
::
++  on-fail   on-fail:def
--
