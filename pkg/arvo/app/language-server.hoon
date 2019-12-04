/+  *server,
    auto=language-server-complete,
    lsp-parser=language-server-parser,
    easy-print=language-server-easy-print,
    rune-snippet=language-server-rune-snippet,
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
+$  all-state  bufs=(map uri=@t buf=wall)
--
^-  agent:gall
=|  all-state
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
    [~ this(state !<(all-state old-state))]
  ::
  ++  on-poke
    ^+  on-poke:*agent:gall
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+    mark  (on-poke:def mark vase)
          %handle-http-request
        (handle-http-request:lsp !<([eyre-id=@ta inbound-request:eyre] vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ?.  ?=([%http-response @ ~] path)
      (on-watch:def path)
    `this
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-agent  on-agent:def
  ++  on-arvo
    ^+  on-arvo:*agent:gall
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  wire  (on-arvo:def wire sign-arvo)
        [%connect ~]  ?>(?=(%bound +<.sign-arvo) `state)
      ==
    [cards this]
  ::
  ++  on-fail   on-fail:def
  --
::
|_  bow=bowl:gall
::
++  parser
  =,  dejs:format
  |^
  %:  ot
    uri+so
    :-  %data
    %-  of
    :~  sync+sync
        completion+position
        commit+ni
        hover+position
    ==
    ~
  ==
  ::
  ++  sync
    %-  ar
    %:  ou
      range+(uf ~ (pe ~ range))
      'rangeLength'^(uf ~ (pe ~ ni))
      text+(un so)
      ~
    ==
  ::
  ++  range
    %:  ot
      start+position
      end+position
      ~
    ==
  ::
  ++  position
    %:  ot
      line+ni
      character+ni
      ~
    ==
  --
::
++  json-response
  |=  [eyre-id=@ta jon=json]
  ^-  (list card)
  (give-simple-payload:app eyre-id (json-response:gen (json-to-octs jon)))
::
::  +handle-http-request: received on a new connection established
::
++  handle-http-request
  |=  [eyre-id=@ta =inbound-request:eyre]
  ^-  (quip card _state)
  ?>  ?=(^ body.request.inbound-request)
  =/  =lsp-req
    %-  parser
    (need (de-json:html q.u.body.request.inbound-request))
  =/  buf  (~(gut by bufs) uri.lsp-req *wall)
  =^  cards  buf
    ?-  +<.lsp-req
      %sync        (handle-sync buf eyre-id +>.lsp-req)
      %completion  (handle-completion buf eyre-id +>.lsp-req)
      %commit      (handle-commit buf eyre-id uri.lsp-req)
      %hover       (handle-hover buf eyre-id +>.lsp-req)
    ==
  =.  bufs
    (~(put by bufs) uri.lsp-req buf)
  [cards state]
::
++  regen-diagnostics
  |=  buf=wall
  ^-  json
  =/  t=tape
    (zing (join "\0a" buf))
  =/  parse
    (lily:auto t (lsp-parser *beam))
  ?:  ?=(%| -.parse)
    (format-diagnostic p.parse)
  =,  enjs:format
  %-  pairs
  :~  good+b+&
  ==
::
++  format-diagnostic
  |=  [row=@ col=@]
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  good+b+|
      :+  %diagnostics  %a  :_  ~
      =/  loc  (pairs line+(numb (dec row)) character+(numb col) ~)
      %-  pairs
      :~  range+(pairs start+loc end+loc ~)
          severity+n+'1'
          message+s+'syntax error'
      ==
  ==
::
++  handle-commit
  |=  [buf=wall eyre-id=@ta uri=@t]
  ^-  [(list card) wall]
  :_  buf
  =/  jon
    (regen-diagnostics buf)
  :_  (json-response eyre-id jon)
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
++  handle-hover
  |=  [buf=wall eyre-id=@ta row=@ud col=@ud]
  ^-  [(list card) wall]
  =/  txt
    (zing (join "\0a" buf))
  =+  (get-id:auto (get-pos buf row col) txt)
  ?~  id
    [(json-response eyre-id *json) buf]
  =/  match=(unit [=term =type])
    (search-exact:auto u.id (get-identifiers:auto -:!>(..zuse)))
  ?~  match
    [(json-response eyre-id *json) buf]
  =/  contents
    %-  crip
    ;:  weld
      "`"
      ~(ram re ~(duck easy-print type.u.match))
      "`"
    ==
  :_  buf
  %+  json-response  eyre-id
  %-  pairs:enjs:format
  [contents+s+contents ~]
::
++  handle-sync
  |=  [buf=wall eyre-id=@ta changes=(list change)]
  :-  (json-response eyre-id *json)
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
++  handle-completion
  |=  [buf=wall eyre-id=@ta row=@ud col=@ud]
  ^-  [(list card) wall]
  =/  =tape  (zing (join "\0a" buf))
  =/  pos  (get-pos buf row col)
  :_  buf
  ::  Check if we're on a rune
  ::
  =/  rune  (swag [(safe-sub pos 2) 2] tape)
  ?:  (~(has by runes:rune-snippet) rune)
    (json-response eyre-id (rune-snippet rune))
  ::  Don't run on large files because it's slow
  ::
  ?:  (gth (lent buf) 1.000)
    =,  enjs:format
    (json-response eyre-id (pairs good+b+& result+~ ~))
  ::
  =/  tl
    (tab-list-tape:auto -:!>(..zuse) pos tape)
  =,  enjs:format
  %+  json-response  eyre-id
  ?:  ?=(%| -.tl)
    (format-diagnostic p.tl)
  ?~  p.tl
    *json
  %-  pairs
  :~  good+b+&
  ::
      :-  %result
      %-  pairs
      :~  'isIncomplete'^b+&
      ::
          :-  %items
          :-  %a
          =/  lots  (gth (lent u.p.tl) 10)
          %-  flop
          %+  turn  (scag 50 u.p.tl)
          |=  [=term =type]
          ?:  lots
            (frond label+s+term)
          =/  detail  (crip ~(ram re ~(duck easy-print type)))
          (pairs label+s+term detail+s+detail ~)
      ==
  ==
--
