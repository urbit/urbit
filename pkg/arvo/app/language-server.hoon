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
  $%  [%sync text=@t]
      [%completion row=@ud col=@ud]
  ==
::
+$  state
  [buf=@t cache=(tri @tD [hair hoon]) cache-size=@]
--
::
|_  [bow=bowl:gall state]
::
++  this  .
++  tall-cached
  (ifix [gay gay] tall:[%*(. vast fat cache)])
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ~&  >  %lsp-prep
  ?~  old
    :_  this
    [ost.bow %connect / [~ /'~language-server-protocol'] %language-server]~
  [~ this(+<+< -.u.old)]
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
++  poke-handle-http-request
  %-  (require-authorization:app ost.bow move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ?>  ?=(^ body.request.inbound-request)
  =/  =lsp-req
    %.  (need (de-json:html q.u.body.request.inbound-request))
    =,  dejs:format
    %-  of
    :~  sync+so
        completion+(ot line+ni character+ni ~)
    ==
  =^  out-jon  +<+.this
    ?-  -.lsp-req
      %sync        (handle-sync +.lsp-req)
      %completion  (handle-completion +.lsp-req)
    ==
  [[ost.bow %http-response (json-response:app (json-to-octs out-jon))]~ this]
::
++  handle-sync
  |=  text=@t
  =,  enjs:format
  [*json text cache cache-size]
::
::
++  ingest
  =|  count=@
  |=  =wall
  ^-  (tri @tD [hair hoon])
  ?~  wall
    [~ ~]
  =.  count  +(count)
  ?:  (gth count (sub (lent wall) cache-size))
    cache
  =.  cache  $(wall t.wall)
  ?:  (lth count (sub (sub (lent wall) cache-size) 100))
    cache
  ~?  =(0 (mod (lent wall) 100))
    [%ingest-round (lent wall)]
  =/  =tape  (zing (join "\0a" wall))
  =/  mab  (try tape tall-cached)
  ?~  mab
    cache
  ::  ~&  >>>  [- +<]:u.mab
  (~(put up cache) u.mab)
::
++  try
  |*  [los=tape sab=rule]
  =+  vex=(sab [[1 1] los])
  ?~  q.vex
    ~
  ::  Finished with spaces remaining
  ::
  =/  rows  (sub p.p.q.u.q.vex 1)
  =/  cols
    ?:  =(0 rows)
      (sub q.p.q.u.q.vex 1)
    q.p.q.u.q.vex
  =/  used-tape
    (scag (sub (lent los) (lent q.q.u.q.vex)) los)
  ?.  ?=([?(%32 %10) *] (flop used-tape))
    ::  ~&  >>>  no=used-tape
    ~
  :-  ~
  ^=  u
  ::  ~&  >  [p.q.u.q.vex ppv=p.p.vex qpv=q.p.vex tol=(sub (lent los) (lent q.q.u.q.vex)) ll=(lent los) lq=(lent q.q.u.q.vex)]
  ::  ~&  >>  yes=used-tape
  [used-tape [rows cols] p.u.q.vex]
::
++  safe-sub
  |=  [a=@ b=@]
  ?:  (gth b a)
    0
  (sub a b)
::
++  handle-completion
  |=  [row=@ud col=@ud]
  ^-  [json @t (tri @tD [hair hoon]) @]
  =/  =wain  (to-wain:format buf)
  =/  =wall  (turn wain trip)
  ::  =?  cache  (lth cache-size (lent wall))   (ingest wall)
  ::  =.  cache-size  (min (lent wall) (add cache-size 100))
  =/  =tape  (zing (join "\0a" wall))
  =/  pos
    |-  ^-  @ud
    ?~  wain
      0
    ?:  =(0 row)
      col
    %+  add  +((met 3 i.wain))  ::  +1 because newline
    $(row (dec row), wain t.wain)
  :_  [buf cache cache-size]
  ~&  >>>  bef=(swag [(safe-sub pos 2) 2] tape)
  =/  rune  (swag [(safe-sub pos 2) 2] tape)
  ?:  (~(has by runes:rune-snippet) rune)
    (rune-snippet rune)
  ::
  =/  tl
    (tab-list-tape:auto -:!>(..zuse) pos tape cache)
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
