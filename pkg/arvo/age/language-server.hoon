/+  *server,
    auto=language-server-complete,
    easy-print=language-server-easy-print,
    rune-snippet=language-server-rune-snippet,
    default-agent
|%
+$  card  card:agent:mall
+$  lsp-req
  $:  uri=@t
      $%  [%sync changes=(list change)]
          [%completion position]
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
^-  agent:mall
=|  all-state
=*  state  -
=<
  |_  =bowl:mall
  +*  this      .
      lsp-core  +>
      lsp       ~(. lsp-core bowl)
      def       ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^+  on-init:*agent:mall
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
    ^+  on-load:*agent:mall
    |=  old-state=vase
    ^-  (quip card _this)
    ~&  >  %lsp-upgrade
    [~ this(state !<(all-state old-state))]
  ::
  ++  on-poke
    ^+  on-poke:*agent:mall
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+    mark  (on-poke:def mark vase)
          %handle-http-request
        (handle-http-request:lsp !<(inbound-request:eyre vase))
      ==
    [cards this]
  ::
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-agent  on-agent:def
  ++  on-arvo
    ^+  on-arvo:*agent:mall
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
|_  bow=bowl:mall
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
::  +handle-http-request: received on a new connection established
::
++  handle-http-request
  %-  (require-authorization:app state)
  |=  =inbound-request:eyre
  ^-  (quip card all-state)
  ?>  ?=(^ body.request.inbound-request)
  =/  =lsp-req
    %-  parser
    (need (de-json:html q.u.body.request.inbound-request))
  =/  buf  (~(gut by bufs) uri.lsp-req *wall)
  =^  out-jon  buf
    ?-  +<.lsp-req
      %sync        (handle-sync buf +>.lsp-req)
      %completion  (handle-completion buf +>.lsp-req)
    ==
  =.  bufs
    (~(put by bufs) uri.lsp-req buf)
  [[%give %http-response (json-response:app (json-to-octs out-jon))]~ state]
::
++  handle-sync
  |=  [buf=wall changes=(list change)]
  :-  *json
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
  |=  [buf=wall row=@ud col=@ud]
  ^-  [json wall]
  =/  =tape  (zing (join "\0a" buf))
  =/  pos  (get-pos buf row col)
  :_  buf
  ::  Check if we're on a rune
  ::
  =/  rune  (swag [(safe-sub pos 2) 2] tape)
  ?:  (~(has by runes:rune-snippet) rune)
    (rune-snippet rune)
  ::  Don't run on large files because it's slow
  ::
  ?:  (gth (lent buf) 1.000)
    =,  enjs:format
    (pairs good+b+& result+~ ~)
  ::
  =/  tl
    (tab-list-tape:auto -:!>(..zuse) pos tape)
  =,  enjs:format
  ?:  ?=(%| -.tl)
    %-  pairs
    :~  good+b+|
        :+  %diagnostics  %a  :_  ~
        =/  loc  (pairs line+(numb (dec row.p.tl)) character+(numb col.p.tl) ~)
        %-  pairs
        :~  range+(pairs start+loc end+loc ~)
            severity+n+'1'
            message+s+'syntax error'
        ==
    ==
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
