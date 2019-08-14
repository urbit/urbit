/-  *chat, hall
/+  hall-json
|%
::
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
--
