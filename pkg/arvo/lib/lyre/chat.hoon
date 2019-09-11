/-  *lyre, hall
|=  tels=(list telegram:hall)
^-  dom
:*  %padding  20  20  20  20
  :-  %vertical
  %-  flop
  ^-  (list dom)
  :-  
    :^  %form  %dummy  %json
    :-  %horizontal
    :~  [%text-input %chat-input]
        [%submit [%text 'poast']]
    ==
  %+  turn  tels
  |=  tel=telegram:hall
  ^-  dom
  ?>  ?=(%lin -.sep.tel)
  [%text msg.sep.tel]
==
