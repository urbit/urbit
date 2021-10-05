::
::::  /hoon/tang/mar
  ::
/?    310
::
=,  format
|_  tan=(list tank)
++  grad  %noun
++  grow
  |%
  ++  noun  tan
  ++  json
    =/  result=(each (list ^json) tang)
      (mule |.((turn tan tank:enjs:format)))
    ?-  -.result
      %&  a+p.result
      %|  a+[a+[%s '[[output rendering error]]']~]~
    ==
  ::
  ++  elem
    =-  ;pre:code:"{(of-wall -)}"
    ^-  wall  %-  zing  ^-  (list wall)
    (turn (flop tan) |=(a=tank (wash 0^160 a)))
  --
++  grab                                                ::  convert from
  |%
  ++  noun  (list ^tank)                                ::  clam from %noun
  ++  tank  |=(a=^tank [a]~)
  --
--
