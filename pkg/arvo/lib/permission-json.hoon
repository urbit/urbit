/-  *permission-store
|%
++  permission-to-json
  |=  pem=permission-map
  =,  enjs:format
  ^-  json
  %+  frond  %permission-initial
  %-  pairs
  %+  turn  ~(tap by pem)
  |=  [=path =permission]
  ^-  [cord json]
  :-  (spat path)
  %-  pairs
  :~  [%kind s+kind.permission]
      [%who [%a (turn ~(tap in who.permission) ship)]]
  ==
::
++  ki
  =,  dejs:format
  ^-  $-(json kind)
  (su (perk %black %white ~))
::
++  pa 
  |=  a/path
  ^-  json
  s+(spat a)
::
++  as                                                :: array as set
  =,  dejs:format
  |*  a/fist
  (cu ~(gas in *(set _$:a)) (ar a))
::
++  sa                                                :: string as ta
  |=  jon=^json
  ?>  ?=([%s *] jon)
  (scot %tas p.jon)
--

