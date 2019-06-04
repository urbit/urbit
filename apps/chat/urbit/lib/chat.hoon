/-  hall
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:http-server term]
      [%peer wire dock path]
      [%quit ~]
      [%poke wire dock poke]
      [%peer wire dock path]
      [%pull wire dock ~]
      [%diff diff]
  ==
::
+$  diff
  $%  [%hall-rumor rumor:hall]
      [%chat-initial streams]
      [%chat-update update]
      [%chat-config streams]
  ==
::
+$  poke
  $%  [%hall-action action:hall]
      [%noun [@tas path]]
  ==
::
+$  state
  $%  [%0 str=streams]
  ==
::
+$  streams
  $:  ::  inbox config
      ::
      inbox=config:hall
      ::  names and configs of all circles we know about
      ::
      configs=(map circle:hall (unit config:hall))
      ::  messages for all circles we know about
      ::
      messages=(map circle:hall (list envelope:hall))
      ::
      ::
      circles=(set name:hall)
      ::
      ::
      peers=(map circle:hall (set @p))
  ==
::
+$  update
  $%  [%inbox con=config:hall]
      [%message cir=circle:hall env=envelope:hall]
      [%messages cir=circle:hall start=@ud end=@ud env=(list envelope:hall)]
      [%config cir=circle:hall con=config:hall]
      [%circles cir=(set name:hall)]
      [%peers cir=circle:hall per=(set @p)]
  ==
::
+$  action  [%actions lis=(list action:hall)]

+$  pareto-mark
  $%
    [%json pax=path name=@t jon=json]
    [%path name=@t obj=(map @t json)]
  ==
::
::
::
::  +utilities
::
+$  indices-internal-state
  $:  
    lis=(list [circle:hall @])
    item=[cir=circle:hall count=@ud]
    index=@
  ==
::
++  generate-circle-indices
  |=  wir=wire
  ^-  (list [circle:hall @])
  =/  data
    %^  spin  (swag [0 (lent wir)] wir)  *indices-internal-state
    |=  [a=@ta b=indices-internal-state]
    ^-  [* out=indices-internal-state]
    =/  switch  (dvr index.b 3)
    ?:  =(q.switch 0)  :: remainder 0, should be a ship
      ?:  =(index.b 0)  ::  if item is null, don't add to list
        :-  0
        %=  b
          hos.cir.item  (slav %p a)
          index  +(index.b)
        ==
      ::  if item is not null, add to list
      :-  0
      %=  b
        hos.cir.item  (slav %p a)
        nom.cir.item  *name:hall
        count.item  0
        lis  (snoc lis.b item.b)
        index  +(index.b)
      ==
    ?:  =(q.switch 1)  :: remainder 1, should be a circle name
      :-  0
      %=  b
        nom.cir.item  a
        index  +(index.b)
      ==
    ?:  =(q.switch 2)  :: remainder 2, should be a number
      :-  0
      %=  b
        count.item  (need (rush a dem))
        index  +(index.b)
      ==
    !!  ::  impossible
  ?:  =(index.q.data 0)
    ~
  (snoc lis.q.data item.q.data)
::
--
::
