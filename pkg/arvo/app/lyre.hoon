/-  *lyre, inbox-store
/+  *server
::
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
      [%kill wire ~]
      [%warp wire ship riff:clay]
      [%peer wire dock path]
      [%pull wire dock ~]
  ==
::
+$  diff
  $%  [%json json]
  ==
::
+$  view
  $:  dep=dependencies
      arg=arguments
      req-handle=(map wire bone) 
      out=output
  ==
::
+$  dependencies
  $:  clay=(list [beam care:clay])
      gall=(list [app=@tas sub=path])
      raw=(unit json)
      ren=renderer
  ==
::
+$  arguments
  $:  clay=(map [beam care:clay] (each cage tang))
      gall=(map [@tas path] (each cage tang))
      raw=(unit json)
  ==
::
+$  renderer  @tas
::
+$  output
  $~  [%pending ~]
  $%  [%complete json]
      [%error json]
      [%pending ~]
  ==
::
+$  state
  $:  ses=(map @tas view)
      cur=@tas
      req-handle=(map wire bone)
  ==
::
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
    =/  res=[mov=(list move) thy=_this]  initialize
    :_  thy.res
    [[ost.bol %connect / [~ /'~lyre'] %lyre] mov.res]
  initialize
::
++  initialize
  ^-  (quip move _this)
  =/  dep=dependencies  [~ ~ ~ %home]
  =/  initial-view=view  [dep [~ ~ ~] ~ *output]
  =.  cur  %home
  =.  ses  (my [%home initial-view] ~)
  finish:send-error-or-build-page:(init:per-view cur)
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?+  a
    ~&  poked+;;(json a)
    [~ this]
  ::
      %print-state
    ~&  cur+cur
    ::
    =+  %+  turn  ~(tap by ses)
    |=  [nom=@tas vew=view]
    ~&  dependencies+dep.vew
    ~&  %arguments
    ~&  %clay
    =+  %-  ~(rut by clay.arg.vew) 
      |=  [[bem=beam car=care:clay] res=(each cage tang)]
      ~&  [bem car]
      ?:  ?=(%.y -.res)
        ~&  q.q.p.res
        ~
      %-  (slog p.res)
      ~
    ~&  %gall
    =+  %-  ~(rut by gall.arg.vew) 
      |=  [met=[@tas path] res=(each cage tang)]
      ~&  met
      ?:  ?=(%.y -.res)
        ~&  q.q.p.res
        ~
      %-  (slog p.res)
      ~
    ~&  raw=raw.arg.vew
    ~&  out+out.vew
    ~
    ::
    [~ this]
  ::
      %flush-state
    initialize
  ::
      %print-bowl
    ~&  bol
    [~ this]
  ::
      %chat
    =/  dep=dependencies
      :*  ~  ::[[[byk.bol /hoon/hello/gen] %x] [[byk.bol /] %y]]
          ~[[%inbox /full/all/chat1]]
          ~
          %chat
      ==
    ::
    finish:(set-deps:(init:per-view cur) dep)
  ::
  ==
::
++  writ
  |=  [wir=wire rot=riot:clay]
  ^-  (quip move _this)
  ?<  ?=(~ wir)
  =/  nom=@tas  i.wir
  finish:(update-clay:(init:per-view nom) wir rot)
::
++  diff
  |=  [wir=wire mar=@tas dat=*]
  ^-  (quip move _this)
  ?<  ?=(~ wir)
  =/  nom=@tas  i.wir
  finish:(validate-gall:(init:per-view nom) wir mar dat)
::
++  made
  |=  [wir=wire wen=@da mad=made-result:ford]
  ^-  (quip move _this)
  =/  result=(each cage tang)
    ?:  ?=(%incomplete -.mad)
      [%.n tang.mad]
    =/  bul=build-result:ford  build-result.mad
    ?:  ?=(%error -.bul)
      %-  (slog message.bul)
      [%.n message.bul]
    ?+  +<.bul
      ~|(we-shouldnt-be-here+wir !!)
      %cast  [%.y cage.bul]
      %vale  [%.y cage.bul]
    ==
  ?+  wir
    ~|(we-shouldnt-be-here+wir !!)
  ::
      [%validate @tas *]
    =/  nom=@tas  i.t.wir
    finish:(return-validated:(init:per-view nom) wir result)
  ::
    [%render @tas @tas ~]
    =/  nom=@tas  i.t.wir
    finish:(update-output:(init:per-view nom) wir result)
  ==
::
++  per-view
  |_  $:  nom=@tas
          vew=view
          mow=(list move)
      ==
  ++  this  .
  ::
  ++  finish
    [(flop mow) ^this(ses (~(put by ses) nom vew))]
  ::
  ++  return  [(flop mow) vew]
  ::
  ++  emit
    |=  mov=move
    ^+  this
    this(mow [mov mow])
  ::
  ++  emil
    |=  mos=(list move)
    ^+  this
    this(mow (welp mos mow))
  ::
  ++  init
    |=  nom=@tas
    ^+  this
    %=  this
      nom  nom
    ::
        vew
      ~|  db-init-nonexistent-view+nom
      (~(got by ses) nom)
    ==
  ::
  ++  set-deps
    |=  dep=dependencies
    ^+  this
::    ?:  =(dep dep.vew)
::      this
    =.  this  stop-clay
    =.  this  pull-gall
    =.  dep.vew  dep
    =.  this  read-clay
    =.  this  subscribe-gall
    =.  out.vew  [%pending ~]
    send-output
  ::
  ++  read-clay
    ^+  this
    =^  mol   req-handle.vew
      %+  roll  clay.dep.vew
      |=  [[bem=beam car=care:clay] [mov=(list move) req=_req-handle.vew]]
      =/  wir=wire  (welp /[nom]/[car] (en-beam:format bem))
      :-  [[ost.bol %warp wir p.bem q.bem `[%sing car r.bem (flop s.bem)]] mov]
      (~(put by req) wir ost.bol)
    (emil mol)
  ::
  ++  stop-clay
    ^+  this
    =^  mol   req-handle.vew
      %+  roll  clay.dep.vew
      |=  [[bem=beam car=care:clay] [mov=(list move) req=_req-handle.vew]]
      =/  wir=wire  (welp /[nom]/[car] (en-beam:format bem))
      =/  bon=bone  (~(got by req) wir)
      :-  [[bon %warp wir p.bem q.bem ~] mov]
      (~(del by req) wir ost.bol)
    (emil mol)
  ::
  ++  update-clay
    |=  [wir=wire rot=riot:clay]
    ^+  this
    ?>  ?=([@tas @tas *] wir)
    =/  car=care:clay  ;;(care:clay i.t.wir)
    =/  bem=beam       (need (de-beam:format t.t.wir))
    =/  res=(each cage tang)
      ?~  rot
        [%.n [leaf+"no such file: {(spud t.t.wir)}"]~]
      [%.y r.u.rot]
    =.  clay.arg.vew    (~(put by clay.arg.vew) [bem car] res)
    =.  req-handle.vew  (~(put by req-handle.vew) wir ost.bol)
    =.  this
      %-  emit
      :*  ost.bol  %warp  wir
          p.bem  q.bem
          `[%next car [%da now.bol] (flop s.bem)]
      ==
    send-error-or-build-page
  ::
  ++  subscribe-gall
    ^+  this
    =^  mol   req-handle.vew
      %+  roll  gall.dep.vew
      |=  [[app=@tas sub=path] [mov=(list move) req=_req-handle.vew]]
      =/  wir=wire  (welp /[nom]/[app] sub)
      :-  [[ost.bol %peer wir [our.bol app] sub] mov]
      (~(put by req) wir ost.bol)
    (emil mol)
  ::
  ++  pull-gall
    ^+  this
    =^  mol   req-handle.vew
      %+  roll  gall.dep.vew
      |=  [[app=@tas sub=path] [mov=(list move) req=_req-handle.vew]]
      =/  wir=wire  (welp /[nom]/[app] sub)
      =/  bon=bone  (~(got by req) wir)
      :-  [[bon %pull sub [our.bol app] ~] mov]
      (~(del by req) wir ost.bol)
    (emil mol)
  ::
  ++  validate-gall
    |=  [wir=wire mar=@tas dat=*]
    ^+  this
    =/  ford-wir=wire  (welp /validate wir)
    =/  schema=schematic:ford  [%vale [our.bol q.byk.bol] mar dat]
    (emit [ost.bol %build ford-wir %.n schema])
  ::
  ++  return-validated
    |=  [wir=wire res=(each cage tang)]
    ^+  this
    ?>  ?=([@tas @tas @tas *] wir)
    =/  app=@tas  i.t.t.wir
    =/  sub=path  t.t.t.wir
    =.  gall.arg.vew   (~(put by gall.arg.vew) [app sub] res)
    send-error-or-build-page
  ::
  ++  send-error-or-build-page
    ^+  this
    =/  clay-vase=(each vase (unit tang))
      %+  roll  (flop clay.dep.vew)
      |=  [[bem=beam car=care:clay] out=(each vase (unit tang))]
      ?:  ?=(%.n -.out)  ::  short-circuit
        out
      =/  cur=(unit (each cage tang))  (~(get by clay.arg.vew) [bem car])
      ?~  cur 
        [%.n ~]
      ?:  ?=(%.n -.u.cur)
        [%.n `p.u.cur]
      =/  old-vas=vase  p.out
      =/  new-vas=vase  q.p.u.cur
      ?:  =(old-vas *vase)
        [%.y new-vas]
      [%.y [(cell p.new-vas p.old-vas) [q.new-vas q.old-vas]]]
    ::
    ?:  ?=(%.n -.clay-vase)
      =.  out.vew
        ?~  p.clay-vase
          [%pending ~]
        [%error (tang-dom-json u.p.clay-vase)]
      send-output
    ::
    =/  gall-vase=(each vase (unit tang))
      %+  roll  (flop gall.dep.vew)
      |=  [[app=@tas sub=path] out=(each vase (unit tang))]
      ?:  ?=(%.n -.out)  ::  short-circuit
        out
      =/  cur=(unit (each cage tang))  (~(get by gall.arg.vew) [app sub])
      ?~  cur 
        [%.n ~]
      ?:  ?=(%.n -.u.cur)
        [%.n `p.u.cur]
      =/  old-vas=vase  p.out
      =/  new-vas=vase  q.p.u.cur
      ?:  =(old-vas *vase)
        [%.y new-vas]
      [%.y [(cell p.new-vas p.old-vas) [q.new-vas q.old-vas]]]
    ::
    ?:  ?=(%.n -.gall-vase)
      =.  out.vew
        ?~  p.gall-vase
          [%pending ~]
        [%error (tang-dom-json u.p.gall-vase)]
      send-output

    =/  arg-vase=(unit vase)
      ?:  &(=([%.y *vase] clay-vase) =([%.y *vase] gall-vase))
        ~
      ?:  =([%.y *vase] clay-vase)
        `p.gall-vase
      ?:  =([%.y *vase] gall-vase)
        `p.clay-vase
      `[(cell p.p.clay-vase p.p.gall-vase) [q.p.clay-vase q.p.gall-vase]]
    (build-output arg-vase)
  ::
  ++  build-output
    |=  arg=(unit vase)
    ^+  this
    =.  this  kill-output
    =/  wir=wire  /render/[nom]/[ren.dep.vew]
    =.  req-handle.vew  (~(put by req-handle.vew) wir ost.bol)
    =/  schema=schematic:ford
      ?~  arg
        %+  cast-mark  %json
        %+  cast-mark  %lyre-dom
        (render ren.dep.vew)
      %+  cast-mark  %json
      %+  cast-mark  %lyre-dom
      %+  call-gate
        (render ren.dep.vew)
      [%$ %noun u.arg]
    (emit [ost.bol %build wir %.y schema])
  ::
  ++  kill-output
    ^+  this
    =/  wir=wire  /render/[nom]/[ren.dep.vew]
    =/  bon=(unit bone)  (~(get by req-handle.vew) wir)
    ?~  bon
      this
    =.  req-handle.vew  (~(del by req-handle.vew) wir)
    (emit [u.bon %kill wir ~])
  ::
  ++  update-output
    |=  [wir=wire res=(each cage tang)]
    ^+  this
    =.  out.vew
      ?:  ?=(%.y -.res)
        [%complete ;;(json q.q.p.res)]
      [%error (tang-dom-json p.res)]
    send-output
  ::
  ++  send-output
    ^+  this
    =/  jon=json
      %-  pairs:enjs:format
      :~  :+  %sessions   %a
          %+  turn  ~(tap in ~(key by ses))
          |=  nom=@tas
          ^-  json
          [%s nom]
      ::
          current+s+cur
      ::
          status+s+-.out.vew
      ::
          data++.out.vew
      ==
    %-  emil
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [b=bone *]
    ^-  move
    [b %diff %json jon]
  ::
  --
::
++  session-json
  ^-  json
  =/  out=output  out:(~(got by ses) cur)
  %-  pairs:enjs:format
  :~  :+  %sessions   %a
      %+  turn  ~(tap in ~(key by ses))
      |=  nom=@tas
      ^-  json
      [%s nom]
  ::
      current+s+cur
  ::
      status+s+-.out
  ::
      data++.out
  ==
::
::
::  ford helper functions
::
++  render
  |=  ren=renderer
  ^-  schematic:ford
  :*  %core
      [our.bol q.byk.bol]
      /hoon/[ren]/lyre/lib
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
::  main action entry-point
::
++  poke-lyre-action
  |=  act=action
  ^-  (quip move _this)
  ?-  -.act
      %new-session
::    =.  ses  (snoc ses initial-view)
::    =.  cur  (dec (lent ses))
    [~ this]
::    (get-view-json /diff ost.bol)
  ::
      %delete-session
::    =.  ses  (oust [id.act 1] ses)
::    =.  cur
::      ?:  =(cur 0)  0
::      ?:((gte cur id.act) (dec cur) cur)
    [~ this]
::    (get-view-json /diff ost.bol)
  ::
      %switch-session
::    ?>  (lth id.act (lent ses))
::    =.  cur  id.act
    [~ this]
::    (get-view-json /diff ost.bol)
  ::
      %set-path
    [~ this]
::    =?  pax.act  =(~[%$] pax.act)  /
::    =.  ses  ;:(welp (scag cur ses) [pax.act]~ (slag +(cur) ses))
::    (get-view-json /diff ost.bol)
  ==
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  %+  turn  (prey:pubsub:userlib /primary bol)
  |=  [b=bone *]
  ^-  move
  [b %diff %json session-json]
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
      [[~ [%'~lyre' ~]] *]
    :_  this
    [ost.bol %http-response (manx-response:app (index session-json))]~
  ==
::
--

