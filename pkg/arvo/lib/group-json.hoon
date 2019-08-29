/-  *groups
=,  enjs:format
|%
++  groups-to-json
  |=  grp=group-initial
  =,  enjs:format
  ^-  ^json
  %+  frond  %groups-initial
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
++  pa 
  |=  a/path
  ^-  json
  s+(spat a)
--
