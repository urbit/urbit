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
  --
++  grab                                                ::  convert from
  |%
  ++  noun  (list ^tank)                                ::  clam from %noun
  ++  tank  |=(a=^tank [a]~)
  --
--
