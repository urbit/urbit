=,  enjs:format
|%
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
