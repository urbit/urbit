::  Print useful diagnostic information
::
:-  %say
|=  [[now=time * bec=beak] ~ ~]
=*  our      p.bec
=/  sponsor  (sein:title our now our)
:-  %noun
=<
:~
  [%base-hash base-hash]
  [%home-hash .^(@uv %cz (pathify ~.home ~))]
  [%kids-hash .^(@uv %cz (pathify ~.kids ~))]
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
::
++  base-hash
  =/  parent  (scot %p (sein:title our now our))
  =+  .^(=cass:clay %cs /[parent]/kids/1/late/foo)
  .^(@uv %cz /[parent]/kids/(scot %ud ud.cass))
--
