/-  *group-store
=,  enjs:format
|%
++  groups-to-json
  |=  grp=group-initial
  =,  enjs:format
  ^-  ^json
  %+  frond  %group-initial
  %-  pairs
  %+  turn  ~(tap by grp)
  |=  [=path =group]
  ^-  [@t ^json]
  :-  (spat path)
  (sa group ship:enjs:format)
::
++  sa                                                :::  set as array
  |*  {a/(set) b/$-(* json)}
  ^-  json
  [%a (turn ~(tap in a) b)]
::
++  as                                                :: array as set
  =,  dejs:format
  |*  a/fist
  (cu ~(gas in *(set _$:a)) (ar a))
::
++  pa 
  |=  a/path
  ^-  json
  s+(spat a)
--
