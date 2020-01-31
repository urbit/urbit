/-  lsp-sur=language-server
/+  *server,
    auto=language-server-complete,
    lsp-parser=language-server-parser,
    easy-print=language-server-easy-print,
    rune-snippet=language-server-rune-snippet,
    build=language-server-build,
    default-agent
|%
+$  card  card:agent:gall
+$  lsp-req
  $:  uri=@t
      $%  [%sync changes=(list change)]
          [%completion position]
          [%commit @ud]
          [%hover position]
      ==
  ==
::
+$  change
  $:  range=(unit range)
      range-length=(unit @ud)
      text=@t
  ==
::
+$  range
  $:  start=position
      end=position
  ==
::
+$  position
  [row=@ud col=@ud]
::
+$  state-zero
  $:  %0
      bufs=(map uri=@t buf=wall)
      builds=(map uri=@t =vase)
      ford-diagnostics=(map uri=@t (list diagnostic:lsp-sur))
      preludes=(map uri=@t type)
  ==
+$  versioned-state
  $%
    state-zero
  ==
--
^-  agent:gall
=|  state-zero
=*  state  -
=<
  |_  =bowl:gall
  +*  this      .
      lsp-core  +>
      lsp       ~(. lsp-core bowl)
      def       ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^+  on-init:*agent:gall
    ^-  (quip card _this)
    ~&  >  %lsp-init
    :_  this  :_  ~
    :*  %pass  /connect
        %arvo  %e
        %connect  [~ /'~language-server-protocol']  %language-server
    ==
  ::
  ++  on-save   !>(state)
  ++  on-load
    ^+  on-load:*agent:gall
    |=  old-state=vase
    ^-  (quip card _this)
    ~&  >  %lsp-upgrade
    [~ this(state *state-zero)]
  ::
  ++  on-poke
    ^+  on-poke:*agent:gall
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+    mark  (on-poke:def mark vase)
          %language-server-rpc-notification
        (on-notification:lsp !<(all:notification:lsp-sur vase))
          %language-server-rpc-request
        (on-request:lsp !<(all:request:lsp-sur vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ?:  ?=([%primary ~] path)
      `this
    ?.  ?=([%http-response @ ~] path)
      (on-watch:def path)
    `this
  ++  on-leave  on-leave:def
  ++  on-peek  on-peek:def
  ++  on-agent  on-agent:def
  ++  on-arvo
    ^+  on-arvo:*agent:gall
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  sign-arvo  (on-arvo:def wire sign-arvo)
        [%e %bound *]  `state
        [%f *]  (handle-build:lsp wire +.sign-arvo)
      ==
    [cards this]
  ::
  ++  on-fail   on-fail:def
  --
::
|_  bow=bowl:gall
::
++  json-response
  |=  [eyre-id=@ta jon=json]
  ^-  (list card)
  (give-simple-payload:app eyre-id (json-response:gen (json-to-octs jon)))
::
++  give-rpc-notification
  |=  res=out:notification:lsp-sur
  ^-  (list card)
  :_  ~
  [%give %fact ~[/primary] %language-server-rpc-notification !>(res)]
::
++  on-notification
  |=  not=all:notification:lsp-sur
  ^-  (quip card _state)
  =^  cards  state
    ?+  -.not  [~ state]
        %text-document--did-open    (handle-did-open +.not)
        %text-document--did-change  (handle-did-change +.not)
        %text-document--did-save    (handle-did-save +.not)
        %text-document--did-close   (handle-did-close +.not)
        %exit                       handle-exit
    ==
  [cards state]
++  on-request
  |=  req=all:request:lsp-sur
  ^-  (quip card _state)
  =^  cards  state
    ?+  -.req  [~ state]
      %text-document--hover       (handle-hover req)
      %text-document--completion  (handle-completion req)
    ==
  [cards state]
::
++  get-subject
  |=  uri=@t
  ^-  type
  (~(gut by preludes) uri -:!>(..zuse))
::
++  handle-completion
  |=  com=text-document--completion:request:lsp-sur
  ^-  (quip card _state)
  :_  state
  %^  give-rpc-response  %text-document--completion  id.com
  =/  buf=wall
    (~(got by bufs) uri.com)
  =/  txt=tape
    (zing (join "\0a" buf))
  =/  pos
    (get-pos buf row.com col.com)
  =/  rune  (rune-snippet (swag [(safe-sub pos 2) 2] txt))
  ?^  rune  rune
  =/  tab-list
    %^  tab-list-tape:auto
      (~(gut by preludes) uri.com -:!>(..zuse))
    pos  txt
  ?:  ?=(%| -.tab-list)  ~
  ?~  p.tab-list  ~
  ?~  u.p.tab-list  ~
  (turn u.p.tab-list make-completion-item)
::
++  make-completion-item
  |=  [name=term =type]
  ^-  completion-item:lsp-sur
  =/  doc
    %-  crip
    ;:  weld
      "`"
      ~(ram re ~(duck easy-print type))
      "`"
    ==
  [name 1 doc '' name 1]
::
++  give-rpc-response
  |=  res=all:response:lsp-sur
  ^-  (list card)
  :_  ~
  [%give %fact ~[/primary] %language-server-rpc-response !>(res)]
::
++  handle-exit
  ^-  (quip card _state)
  ~&  >  %lsp-shutdown
  :_  *state-zero
  %-  zing
  %+  turn
    ~(tap in ~(key by builds))
  |=  uri=@t
  :+  [%pass /ford/[uri] %arvo %f %kill ~]
  [%pass /ford/[uri]/deps %arvo %f %kill ~]
  ~
::
++  handle-did-close
  |=  [uri=@t version=(unit @)]
  ^-  (quip card _state)
  =.  bufs
    (~(del by bufs) uri)
  =.  ford-diagnostics
    (~(del by ford-diagnostics) uri)
  =.  builds
    (~(del by builds) uri)
  :_  state
  :~
    [%pass /ford/[uri] %arvo %f %kill ~]
    [%pass /ford/[uri]/deps %arvo %f %kill ~]
  ==
::
++  handle-did-save
  |=  [uri=@t version=(unit @)]
  ^-  (quip card _state)
  :_  state
  :_  (give-rpc-notification (get-diagnostics uri))
  :*
    %pass
    /commit
    %agent
    [our.bow %hood]
    %poke
    %kiln-commit
    !>([q.byk.bow |])
  ==
::
++  handle-did-change
  |=  [document=versioned-doc-id:lsp-sur changes=(list change:lsp-sur)]
  ^-  (quip card _state)
  =/  updated=wall
    (sync-buf (~(got by bufs) uri.document) changes)
  =.  bufs
    (~(put by bufs) uri.document updated)
  `state
::
++  handle-build
  |=  [=path =gift:able:ford]
  ^-  (quip card _state)
  ?.  ?=([%made *] gift)
    [~ state]
  ?.  ?=([%complete *] result.gift)
    [~ state]
  =/  uri=@t
    (snag 1 path)
  =/  =build-result:ford
    build-result.result.gift
  ?+  build-result  [~ state]
        ::
      [%success %plan *]
    =.  preludes
      (~(put by preludes) uri -:vase.build-result)
    [~ state]
        ::
      [%success %core *]
    =.  builds
      (~(put by builds) uri vase.build-result)
    =.  ford-diagnostics
      (~(del by ford-diagnostics) uri)
    :_  state
    (give-rpc-notification (get-diagnostics uri))
        ::
      [%error *]
    =/  error-ranges=(list =range:lsp-sur)
      (get-errors-from-tang:build uri message.build-result)
    ?~  error-ranges
      [~ state]
    =.  ford-diagnostics
      %+  ~(put by ford-diagnostics)
        uri
      [i.error-ranges 1 'Build Error']~
    :_  state
    (give-rpc-notification (get-diagnostics uri))
  ==
::
++  get-diagnostics
  |=  uri=@t
  ^-  out:notification:lsp-sur
  :+  %text-document--publish-diagnostics
    uri
  %+  weld
    (~(gut by ford-diagnostics) uri ~)
  (get-parser-diagnostics uri)
::
++  get-build-deps
  |=  [=path buf=wall]
  ^-  schematic:ford
  =/  parse=(like scaffold:ford)
    %+  (lsp-parser [byk.bow path])  [1 1]
    (zing (join "\0a" buf))
  =/  =scaffold:ford
    ?~  q.parse  *scaffold:ford
    p.u.q.parse
  :*  %plan
    [[our.bow %home] (flop path)]
    *coin
    scaffold(sources `(list hoon)`~[[%cnts ~[[%& 1]] ~]])
  ==
::
++  handle-did-open
  |=  item=text-document-item:lsp-sur
  ^-  (quip card _state)
  =/  buf=wall
    (to-wall (trip text.item))
  =.  bufs
    (~(put by bufs) uri.item buf)
  =/  =path
    (uri-to-path:build uri.item)
  =/  =schematic:ford
    [%core [our.bow %home] (flop path)]
  =/  dep-schematic=schematic:ford
    (get-build-deps path buf)
  :_  state
  %+  weld
    (give-rpc-notification (get-diagnostics uri.item))
  ^-  (list card)
  :~
    [%pass /ford/[uri.item] %arvo %f %build live=%.y schematic]
    [%pass /ford/[uri.item]/deps %arvo %f %build live=%.y dep-schematic]
  ==
::
++  get-parser-diagnostics
  |=  uri=@t
  ^-  (list diagnostic:lsp-sur)
  =/  t=tape
    (zing (join "\0a" `wall`(~(got by bufs) uri)))
  =/  parse
    (lily:auto t (lsp-parser *beam))
  ?.  ?=(%| -.parse)
    ~
  =/  loc=position:lsp-sur
    [(dec -.p.parse) +.p.parse]
  :_  ~
  [[loc loc] 1 'Syntax Error']
::
++  handle-hover
  |=  hov=text-document--hover:request:lsp-sur
  ^-  (quip card _state)
  :_  state
  %^  give-rpc-response  %text-document--hover  id.hov
  =/  buf=wall
    (~(got by bufs) uri.hov)
  =/  txt
    (zing (join "\0a" buf))
  =/  tab-list
    %^  tab-list-tape:auto
        (~(gut by preludes) uri.hov -:!>(..zuse))
      (get-pos buf row.hov col.hov)
    txt
  ?:  ?=(%| -.tab-list)  ~
  ?~  p.tab-list  ~
  ?~  u.p.tab-list  ~
  :-  ~
  %-  crip
  ;:  weld
    "`"
    ~(ram re ~(duck easy-print detail.i.u.p.tab-list))
    "`"
  ==
::
++  sync-buf
  |=  [buf=wall changes=(list change:lsp-sur)]
  |-  ^-  wall
  ?~  changes
    buf
  ?:  ?|(?=(~ range.i.changes) ?=(~ range-length.i.changes))
    =/  =wain  (to-wain:format text.i.changes)
    =.  buf  (turn wain trip)
    $(changes t.changes)
  =/  =tape      (zing (join "\0a" buf))
  =/  start-pos  (get-pos buf start.u.range.i.changes)
  =/  end-pos    (get-pos buf end.u.range.i.changes)
  =.  tape
    ;:  weld
      (scag start-pos tape)
      (trip text.i.changes)
      (slag end-pos tape)
    ==
  =.  buf  (to-wall tape)
  $(changes t.changes)
::
++  to-wall
  |=  =tape
  ^-  wall
  %+  roll  (flop tape)
  |=  [char=@tD =wall]
  ?~  wall
    [[char ~] ~]
  ?:  =('\0a' char)
    [~ wall]
  [[char i.wall] t.wall]
::
++  get-pos
  |=  [buf=wall position]
  ^-  @ud
  ?~  buf
    0
  ?:  =(0 row)
    col
  %+  add  +((lent i.buf))  ::  +1 because newline
  $(row (dec row), buf t.buf)
::
++  safe-sub
  |=  [a=@ b=@]
  ?:  (gth b a)
    0
  (sub a b)
::
--
