/-  *group-store
|%
++  groups-to-json
  |=  grp=groups
  ^-  json
  =,  enjs:format
  %+  frond  %group-initial
  %-  pairs
  %+  turn  ~(tap by grp)
  |=  [pax=^path =group]
  ^-  [@t json]
  :-  (spat pax)
  (set-to-array group ship:enjs:format)
::
++  set-to-array
  |*  {a/(set) b/$-(* json)}
  ^-  json
  [%a (turn ~(tap in a) b)]
--
