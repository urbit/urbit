/+  *server, auto, easy-print, rune-snippet
|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%connect wire binding:eyre term]
      [%disconnect wire binding:eyre]
      [%http-response =http-event:http]
  ==
::
+$  lsp-req 
  $%  [%sync changes=(list change)]
      [%completion position]
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
+$  state  buf=wall
--
::
|_  [bow=bowl:gall state]
::
++  this  .
++  tall
  (ifix [gay gay] tall:vast)
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ~&  >  %lsp-prep
  ?~  old
    :_  this
    [ost.bow %connect / [~ /'~language-server-protocol'] %language-server]~
  [~ this(buf u.old)]
::
::  alerts us that we were bound.
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
::  +poke-handle-http-request: received on a new connection established
::
++  parser
  =,  dejs:format
  |^
  %-  of
  :~  sync+sync
      completion+position
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
++  poke-handle-http-request
  %-  (require-authorization:app ost.bow move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ?>  ?=(^ body.request.inbound-request)
  =/  =lsp-req
    %-  parser
    (need (de-json:html q.u.body.request.inbound-request))
  =^  out-jon  buf
    ?-  -.lsp-req
      %sync        (handle-sync +.lsp-req)
      %completion  (handle-completion +.lsp-req)
    ==
  [[ost.bow %http-response (json-response:app (json-to-octs out-jon))]~ this]
::
++  handle-sync
  |=  changes=(list change)
  :-  *json
  |-  ^-  wall
  ?~  changes
    buf
  ?:  ?|(?=(~ range.i.changes) ?=(~ range-length.i.changes))
    =/  =wain  (to-wain:format text.i.changes)
    =.  buf  (turn wain trip)
    $(changes t.changes)
  =/  =tape      (zing (join "\0a" buf))
  =/  start-pos  (get-pos start.u.range.i.changes)
  =/  end-pos    (get-pos end.u.range.i.changes)
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
  |=  position
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
  |=  [row=@ud col=@ud]
  ^-  [json wall]
  =/  =tape  (zing (join "\0a" buf))
  =/  pos  (get-pos row col)
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
