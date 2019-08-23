/-  *lyre
/+  *server
::
/=  index
  /^  $-(json manx)
  /:  /===/app/lyre/index  /!noun/
::
/=  js
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/lyre/js/index  /js/
      /~  ~
  ==
::
/=  css
  /^  octs
  /;  as-octs:mimes:html
  /|  /:  /===/app/lyre/css/index  /css/
      /~  ~
  ==
::
|%
+$  move  [bone card]
+$  card
  $%  [%connect wire binding:eyre term]
      [%disconnect wire binding:eyre]
      [%http-response =http-event:http]
      [%diff diff]
      [%build wire ? schematic:ford]
      [%warp wire ship riff:clay]
  ==
::
+$  diff
  $%  [%json json]
  ==
::
+$  session
  $:  pax=path
  ==
+$  renderer  ~
::
+$  state
  $:  ses=(list session)
      cur=@u
      ren=(map path (unit renderer))
      req-handle=(map wire bone)
  ==
::
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  ~&  prep+act.bol
  ?~  old
    :_  this
    [ost.bol %connect / [~ /'~lyre'] %lyre]~
::  [~ this(+<+ initialize-state)]           ::  flush state
  [~ this(+<+ ;;(state u.old))]  ::  keep state
::
++  initialize-state
  ^-  state
  =|  sty=state
  sty(ses [/]~)
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?+  a
    ~&  poked+a
    [~ this]
  ::
      %print-state
    ~&  +<+.this
    [~ this]
  ::
      %build
    (get-session-json /diff ost.bol)
  ::
  ==
::
::  ford helper functions
::
++  render
  |=  [pax=path ren=(unit renderer)]
  ^-  schematic:ford
  =/  ren-path
    ?~  ren
      /hoon/[(snag 0 (flop pax))]/lyre/lib
    !!
  :*  %core
      [our.bol q.byk.bol]
      ren-path
  ==
::
++  bake-file
  |=  pax=path
  ^-  schematic:ford
  :*  %bake
      (snag 0 (flop pax))
      *coin
      [our.bol q.byk.bol]
      (slag 1 (flop (slag 3 pax)))
  ==
::
++  render-dir
  ^-  schematic:ford
  :*  %core
      [our.bol q.byk.bol]
      /hoon/clay-dir/lyre/lib
  ==
::
++  call-gate
  |=  [gat=schematic:ford sam=schematic:ford]
  ^-  schematic:ford
  [%call gat sam]
::
++  cast-mark
  |=  [mar=term sch=schematic:ford]
  ^-  schematic:ford
  [%cast [our.bol q.byk.bol] mar sch]
::
++  build-dom-json
  |=  [pax=path ren=(unit renderer)]
  ^-  schematic:ford
  =/  ark=arch  .^(arch %cy pax)
  ?:  &(?=(~ fil.ark) ?=(~ dir.ark))
    %+  cast-mark  %json
    [%$ %lyre-dom !>([%text 'Empty path'])]
  ::
  ?:  ?=(~ fil.ark)
    %+  cast-mark  %json
    %+  cast-mark  %lyre-dom
    %+  call-gate
      render-dir
    [%$ %noun !>(ark)]
  ::
  %+  cast-mark  %json
  %+  cast-mark  %lyre-dom
  (call-gate (render pax ren) (bake-file pax))
::
::  main action entry-point
::
++  poke-lyre-action
  |=  act=action
  ^-  (quip move _this)
  ?-  -.act
      %new-session
    =/  new-pax  ?~  pax.act  /  u.pax.act
    =.  ses  (snoc ses new-pax)
    =.  cur  (dec (lent ses))
    (get-session-json /diff ost.bol)
  ::
      %delete-session
    =.  ses  (oust [id.act 1] ses)
    =.  cur  ?:((gte cur id.act) (dec cur) cur)
    (get-session-json /diff ost.bol)
  ::
      %switch-session
    ?>  (lth id.act (lent ses))
    =.  cur  id.act
    (get-session-json /diff ost.bol)
  ::
      %set-path
    =.  ses  ;:(welp (scag cur ses) [pax.act]~ (slag +(cur) ses))
    (get-session-json /diff ost.bol)
  ==
::
++  session-json
  |=  bod=json
  ^-  json
  %-  pairs:enjs:format
  :~  :+  %sessions   %a
      %+  turn  ses
      |=  s=session
      ^-  json
      :-  %a
      %+  turn  pax.s
      |=  seg=@t
      [%s seg]
  ::
      current+(numb:enjs:format cur)
  ::
      body+bod
  ==
::
++  get-session-json
  |=  [wir=wire bon=bone]
  ^-  (quip move _this)
  =/  cur-ses=path    (snag cur ses)
  =/  build-wir=path  (welp wir cur-ses)
  =/  file-path=path  (welp our-beak cur-ses)
  :_  this(req-handle (~(put by req-handle) build-wir bon))
  [ost.bol %build build-wir %.n (build-dom-json file-path ~)]~
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  (get-session-json /diff ost.bol)
::
++  tang-dom-json
  |=  tan=tang
  ^-  json
  %+  frond:enjs:format
    %text
  %-  wall:enjs:format
  %-  zing
  %+  turn  tan
  |=  a=tank
  (wash [0 80] a)
::
++  made
  |=  [wir=wire wen=@da mad=made-result:ford]
  ^-  (quip move _this)
  =/  jon=json
    ?:  ?=(%incomplete -.mad)
      (tang-dom-json tang.mad)
    =/  bul=build-result:ford  build-result.mad
    ?:  ?=(%error -.bul)
      (tang-dom-json message.bul)
    ?+  +<.bul
      ~
      %cast  ;;(json q.q.cage.bul)
    ==
  ::
  ?+  wir
    [~ this]
  ::
      [%diff *]
    :_  this(req-handle (~(del by req-handle) wir))
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [b=bone *]
    ^-  move
    [b %diff %json (session-json jon)]
  ::
      [%http *]
    =/  b=bone  (~(got by req-handle) wir)
    :_  this(req-handle (~(del by req-handle) wir))
    [b %http-response (manx-response:app (index (session-json jon)))]~
  ==
::
++  bound
  |=  [wir=wire suc=? bin=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  =/  request-line  (parse-request-line url.request.inbound-request)
  ?+  request-line
    :_  this
    [ost.bol %http-response not-found:app]~
  ::  styling
  ::
      [[[~ %css] [%'~lyre' %index ~]] ~]
    :_  this
    [ost.bol %http-response (css-response:app css)]~
  ::  scripting
  ::
      [[[~ %js] [%'~lyre' %index ~]] ~]
    :_  this
    [ost.bol %http-response (js-response:app js)]~
  ::  home page; redirect to recent
  ::
      [[~ [%'~lyre' ~]] ~]
    (get-session-json /http ost.bol)
::    =/  jon=json  (session-json ~)
::    :_  this
::    [ost.bol %http-response (manx-response:app (index jon))]~
  ==
::
--

