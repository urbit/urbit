::  Print useful diagnostic information
::
:-  %say
|=  [[now=time * bec=beak] ~ ~]
=*  our      p.bec
=/  sponsor  (sein:title our now our)
:-  %noun
=<
:~
  [%base-hash .^(@uv %cz (pathify ~.base ~))]
  [%home-hash .^(@uv %cz (pathify ~.home ~))]
  ::
  (info %our our)
  (info %sponsor sponsor)
  (info %dopzod ~dopzod)
  ::
  ["Compare lifes and rifts to values here:"]
  ["https://etherscan.io/address/azimuth.eth#readContract"]
  ["  life - getKeyRevisionNumber"]
  ["  rift - getContinuityNumber"]
==
|%
++  pathify
  |=  [a=@ta b=(unit ship)]
  ^-  path
  =/  o=@ta  (scot %p our)
  =/  n=@ta  (scot %da now)
  ?~  b  ~[o a n]
  ~[o a n (scot %p u.b)]
::
++  info
  |=  [=term =ship]
  ::  unitized life and rift
  =/  lyfe  .^((unit @ud) %j (pathify ~.lyfe `ship))
  =/  ryft  .^((unit @ud) %j (pathify ~.ryft `ship))
  :*  term
      ship=ship
      point=(crip (slag 2 (scow %ui ship)))
      ::  report as units
      life=lyfe
      rift=ryft
  ==
--
