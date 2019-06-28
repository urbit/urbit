/-  hall
/+  hall-json
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
      [%chat-update update]
      [%chat-config streams]
      [%json json]
  ==
::
+$  poke
  $%  [%hall-action action:hall]
      [%launch-action [@tas path @t]]
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
      [%delete cir=circle:hall]
  ==
::
+$  action  [%actions lis=(list action:hall)]
::
::
::  +utilities
::
++  msg-to-json
  =,  enjs:format
  |=  upd=update
  ^-  json
  ?>  ?=(%messages -.upd)
  %+  frond  %update
  %-  pairs
  :~
    :-  %messages
    %-  pairs
    :~
      [%circle (circ:enjs:hall-json cir.upd)]
      [%start (numb start.upd)]
      [%end (numb end.upd)]
      [%envelopes [%a (turn env.upd enve:enjs:hall-json)]]
    ==
  ==
::
++  config-to-json
  |=  str=streams
  =,  enjs:format
  ^-  json
  %+  frond  %chat
  %-  pairs
  :~  
    ::
      [%inbox (conf:enjs:hall-json inbox.str)]
    ::
      :-  %configs
      %-  pairs
      %+  turn  ~(tap by configs.str)
        |=  [cir=circle:hall con=(unit config:hall)]
        ^-  [@t json]
        :-  (crip (circ:en-tape:hall-json cir))
        ?~(con ~ (conf:enjs:hall-json u.con))
    ::
      :-  %circles  :-  %a
      %+  turn  ~(tap in circles.str)
        |=  nom=name:hall
        [%s nom]
    ::
      :-  %peers
      %-  pairs
      %+  turn  ~(tap by peers.str)
        |=  [cir=circle:hall per=(set @p)]
        ^-  [@t json]
        :-  (crip (circ:en-tape:hall-json cir))
        [%a (turn ~(tap in per) ship)]
    ::
  ==
::
++  numbers-to-json
  |=  num=(list [circle:hall @ud])
  ^-  json
  =,  enjs:format
  %+  frond  %chat
  %-  pairs
  :~
    ::
    ::  %config
      :-  %numbers
      :-  %a
      %+  turn  num
      |=  [cir=circle:hall len=@ud]
      ^-  json
      %-  pairs
      :~
        [%circle (circ:enjs:hall-json cir)]
        [%length (numb len)]
      ==
  ==
::
--
::
