/@  node
/@  planner-diff
/-  _/manx-utils
:-  [%node %planner-diff]
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
    =/  when  (parse-date (got:mu %when))
    [%make when %event ~h2 text |]
  ::
      %edit
    =/  id  (slav %da (got:mu %planner-id))
    =/  text  (vol:mu "text")
    =/  when  (parse-date (vol:mu "when"))
    [%edit id when %event ~h2 text |]
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
  %-  year
  [[%.y y] m [d 0 0 0 ~]]
--
