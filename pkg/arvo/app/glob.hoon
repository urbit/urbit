::  glob [landscape]:
::
::  prompts content delivery and Gall state storage for Landscape JS blob
::
/-  glob
/+  default-agent, verb, dbug
|%
++  hash  0v3.o81b7.9dkd7.6ubrn.ebhmi.dtree
+$  state-0  [%0 hash=@uv glob=(unit (each glob:glob tid=@ta))]
+$  all-states
  $%  state-0
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
  |=  now=@da
  ^-  card
  [%pass /start %arvo %b %wait now]
::
++  poke-file-server
  |=  [our=@p =cage]
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
=|  state=state-0
=.  hash.state  hash
=/  serve-path=path  /'~landscape'/js/bundle
^-  agent:gall
%+  verb  |
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
++  on-init
  ^-  (quip card _this)
  ::  delay through timer to make sure %spider has started
  [[(wait-start now.bowl) ~] this]
::
++  on-save   !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  =+  !<(old=all-states old-state)
  ?>  ?=(%0 -.old)
  ?~  glob.old
    on-init
  ?:  ?=(%& -.u.glob.old)
    ?:  =(hash.old hash.state)
      `this(state old)
    on-init
  =/  cancel-cards
    =/  args  [tid.p.u.glob.old &]
    :~  (leave-spider /(scot %uv hash.old) our.bowl)
        (poke-spider /(scot %uv hash.old) our.bowl %spider-stop !>(args))
    ==
  =^  init-cards  this  on-init
  [(weld cancel-cards init-cards) this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      %glob-make
    :_  this
    =/  home=path  /(scot %p our.bowl)/home/(scot %da now.bowl)
    =+  .^(=js=tube:clay %cc (weld home /js/mime))
    =+  .^(=map=tube:clay %cc (weld home /map/mime))
    =+  .^(arch %cy (weld home /app/landscape/js/bundle))
    =/  bundle-hash=@t
      %-  need
      ^-  (unit @t)
      %-  ~(rep by dir)
      |=  [[file=@t ~] out=(unit @t)]
      ?^  out  out
      ?.  ?&  =((end [3 6] file) 'index.')
              !=('sj.' (end [3 3] (swp 3 file)))
          ==
        out
      ``@t`(rsh [3 6] file)
    =/  js-name
      (cat 3 'index.' bundle-hash)
    =/  map-name
      (cat 3 js-name '.js')
    =+  .^(js=@t %cx :(weld home /app/landscape/js/bundle /[js-name]/js))
    =+  .^(map=@t %cx :(weld home /app/landscape/js/bundle /[map-name]/map))
    =+  .^(sw=@t %cx :(weld home /app/landscape/js/bundle /serviceworker/js))
    =+  !<(=js=mime (js-tube !>(js)))
    =+  !<(=sw=mime (js-tube !>(sw)))
    =+  !<(=map=mime (map-tube !>(map)))
    =/  =glob:glob
      %-  ~(gas by *glob:glob)
      :~  /[js-name]/js^js-mime
          /[map-name]/map^map-mime
          /serviceworker/js^sw-mime
      ==
    =/  =path  /(cat 3 'glob-' (scot %uv (sham glob)))/glob
    [%pass /make %agent [our.bowl %hood] %poke %drum-put !>([path (jam glob)])]~
  ::
      %noun
    ?:  =(%kick q.vase)
      (on-load !>(state(hash *@uv)))
    (on-poke:def mark vase)
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?:  ?=([%serving @ ~] wire)
    (on-agent:def wire sign)
  ?:  ?=([%make ~] wire)
    (on-agent:def wire sign)
  ?.  ?=([%running @ ~] wire)
    %-  (slog leaf+"glob: strange on-agent! {<wire -.sign>}" ~)
    (on-agent:def wire sign)
  ?-    -.sign
      %poke-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"glob: couldn't start thread; will retry" u.p.sign)
    :_  this(glob.state ~)  :_  ~
    (leave-spider t.wire our.bowl)
  ::
      %watch-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"glob: couldn't listen to thread; will retry" u.p.sign)
    [~ this(glob.state ~)]
  ::
      %kick
    =?  glob.state  ?=([~ %| *] glob.state)
      ~
    `this
  ::
      %fact
    =/  produced-hash  (slav %uv i.t.wire)
    ?.  =(hash.state produced-hash)
      [~ this]
    ?+    p.cage.sign  (on-agent:def wire sign)
        %thread-fail
      =+  !<([=term =tang] q.cage.sign)
      %-  (slog leaf+"glob: thread failed; will retry" leaf+<term> tang)
      [~ this(glob.state ~)]
    ::
        %thread-done
      =+  !<(=glob:glob q.cage.sign)
      ?.  =(hash.state (sham glob))
        %:  mean
          leaf+"glob: hash doesn't match!"
          >expected=hash.state<
          >got=(sham glob)<
          ~
        ==
      :_  this(glob.state `[%& glob])  :_  ~
      %+  poke-file-server  our.bowl
      [%file-server-action !>([%serve-glob serve-path glob %&])]
    ==
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?:  ?=([%start ~] wire)
    =/  new-tid=@ta  (cat 3 'glob--' (scot %uv eny.bowl))
    =/  args  [~ `new-tid %glob !>([~ hash.state])]
    =/  action  !>([%unserve-dir serve-path])
    :_  this(glob.state `[%| new-tid])
    :~  (poke-file-server our.bowl %file-server-action action)
        (wait-timeout /[new-tid] now.bowl)
        (watch-spider /(scot %uv hash.state) our.bowl /thread-result/[new-tid])
        (poke-spider /(scot %uv hash.state) our.bowl %spider-start !>(args))
    ==
  ?.  ?=([%timer @ ~] wire)
    %-  (slog leaf+"glob: strange on-arvo wire: {<wire [- +<]:sign-arvo>}" ~)
    `this
  ?.  ?=(%wake +<.sign-arvo)
    %-  (slog leaf+"glob: strange on-arvo sign: {<wire [- +<]:sign-arvo>}" ~)
    `this
  ?:  ?=([~ %& *] glob.state)
    `this
  ?.  ?|  ?=(~ glob.state)
          =(i.t.wire tid.p.u.glob.state)
      ==
    `this
  ?^  error.sign-arvo
    %-  (slog leaf+"glob: timer handling failed; will retry" ~)
    [[(wait-timeout t.wire now.bowl)]~ this]
  %-  (slog leaf+"glob: timed out; retrying" ~)
  (on-load !>(state(hash *@uv)))
::
++  on-fail   on-fail:def
--
