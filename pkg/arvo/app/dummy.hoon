/-  hall
|%
+$  move  [bone card]
+$  card
  $%  [%diff %hall-telegrams (list telegram:hall)]
  ==
+$  state  (list telegram:hall)
--
|_  [bol=bowl:gall sty=state]
::
++  prep
  |=  old=(unit *)
  :_  this(sty *state)
  %+  turn  (prey:pubsub:userlib /sub-path bol)
  |=  [b=bone *]
  [b %diff %hall-telegrams *state]
::
++  this  .
::
++  poke-noun
  |=  a=@t
  ^-  (quip move _this)
  ?+  a
    =/  sep=speech:hall    [%lin & a]
    =/  tho=thought:hall   [*serial:hall *audience:hall now.bol sep]
    =/  tel=telegram:hall  [our.bol tho]
    ::
    ~&  old+sty
    =.  sty  [tel sty]
    ~&  new+sty
    ::
    :_  this
    %+  turn  (prey:pubsub:userlib /sub-path bol)
    |=  [b=bone *]
    [b %diff %hall-telegrams sty]
  ::
      %print-state
    ~&  sty
    [~ this]
  ::
      %flush-state
    :_  this(sty *state)
    %+  turn  (prey:pubsub:userlib /sub-path bol)
    |=  [b=bone *]
    [b %diff %hall-telegrams *state]
  ==
::
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  ~&  jon
  =/  parse
    %-  ot:dejs:format
    :~  chat-input+so:dejs:format
    ==
  =/  sep=speech:hall    [%lin & (parse jon)]
  =/  tho=thought:hall   [*serial:hall *audience:hall now.bol sep]
  =/  tel=telegram:hall  [our.bol tho]
  ::
  =.  sty  [tel sty]
  ::
  :_  this
  %+  turn  (prey:pubsub:userlib /sub-path bol)
  |=  [b=bone *]
  [b %diff %hall-telegrams sty]
::
++  peer-sub-path
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  %+  turn  (prey:pubsub:userlib /sub-path bol)
  |=  [b=bone *]
  [b %diff %hall-telegrams sty]
--
