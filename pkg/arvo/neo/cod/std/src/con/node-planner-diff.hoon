/@  node
/@  planner-diff
/-  manx-utils
:-  [%node %$ %planner-diff]
|=  nod=node
^-  planner-diff
|^
  =/  mu  ~(. manx-utils nod)
  =/  head  (@tas (got:mu %head))
  ?+    head  ~|(%bad-head !!)
      %move
    =/  start  (parse-date (vol:mu "start"))
    =/  end  (parse-date (vol:mu "end"))
    =?  end  (gth start end)  (add start ~d15)
    [%move start end]
  ::
      %make
    =/  text  (vol:mu "text")
    =/  kind
      %-  ?(%event %chapter)
      (vol:mu "kind")
    =/  start  (parse-date (vol:mu "start"))
    =/  end  (parse-date (vol:mu "end"))
    =/  length
      ?:  (lth start end)  (sub end start)
      ?:  =(kind %event)  ~h1
      ~d7
    [%make start kind length text |]
  ::
      %make-event
    =/  text  (vol:mu "text")
    =/  date  (need (val:mu "date"))
    =/  start
      %-  parse-date
      %-  crip
      "{date}T{(fall (val:mu "start") "00:00")}"
    =/  end
      %-  parse-date
      %-  crip
      "{date}T{(fall (val:mu "end") "00:00")}"
    =/  length
      ?:  (lth start end)  (sub end start)
      ~s0
    [%make start %event length text |]
  ::
      %edit-event
    =/  text  (vol:mu "text")
    =/  id  (slav %da (got:mu %planner-id))
    =/  date  (need (val:mu "date"))
    =/  start
      %-  parse-date
      %-  crip
      "{date}T{(fall (val:mu "start") "00:00")}"
    =/  end
      %-  parse-date
      %-  crip
      "{date}T{(fall (val:mu "end") "00:00")}"
    =/  length
      ?:  (lth start end)  (sub end start)
      ~s0
    [%edit id start %event length text |]
  ::
      %edit
    =/  id  (slav %da (got:mu %planner-id))
    =/  text  (vol:mu "text")
    =/  kind
      %-  ?(%event %chapter)
      (got:mu %kind)
    =/  start  (parse-date (vol:mu "start"))
    =/  end  (parse-date (vol:mu "end"))
    =/  length
      ?:  (lth start end)  (sub end start)
      ?:  =(kind %event)  ~h1
      ~d7
    [%edit id start kind length text |]
  ::
      %tomb
    =/  id  (slav %da (got:mu %planner-id))
    [%tomb id]
  ==
++  parse-date
  |=  =cord
  ^-  @da
  =/  tape  (trip cord)
  =/  y  (scan (swag [0 4] tape) dum:ag)
  =/  m  (scan (swag [5 2] tape) dum:ag)
  =/  d  (scan (swag [8 2] tape) dum:ag)
  =/  time  (gth (lent tape) 14)
  =/  h
    ?.  time  0
    (scan (swag [11 2] tape) dum:ag)
  =/  n
    ?.  time  0
    (scan (swag [14 2] tape) dum:ag)
  %-  year
  [[%.y y] m [d h n 0 ~]]
--
