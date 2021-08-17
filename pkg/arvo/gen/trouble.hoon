::  Print useful diagnostic information
::
::  base-hash: loosely, the most recent successfully applied update.
::             Technically, the mergebase of %home with OTA source
::  sour-hash: most recently downloaded update (not necessarily applied)
::  home-hash: hash of %home desk, which may differ if you have changed
::             it, for example with notebooks or 3rd party apps
::  kids-hash: hash of the %kids desk, which is what you serve to your
::             children
::  glob-hash: hash of the glob, which is the js for landscape
::
/+  version
:-  %say
|=  [[now=time * bec=beak] ~ ~]
=*  our      p.bec
=/  sponsor  (sein:title our now our)
:-  %noun
=<
:~
  [%base-hash (base-hash:version our now)]
  [%sour-hash sour-hash]
  [%home-hash .^(@uv %cz (pathify ~.home ~))]
  [%kids-hash .^(@uv %cz (pathify ~.kids ~))]
  [%glob-hash glob-state]
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
++  sour-hash
  =+  .^  ota=(unit [=ship =desk =aeon:clay])
          %gx  /(scot %p our)/hood/(scot %da now)/kiln/ota/noun
      ==
  ?~  ota
    *@uv
  =/  parent  (scot %p ship.u.ota)
  =+  .^(=cass:clay %cs /[parent]/[desk.u.ota]/1/late/foo)
  .^(@uv %cz /[parent]/[desk.u.ota]/(scot %ud ud.cass))
::
++  glob-state
  ^-  [@uv @tas]
  =<  [hash ?~(glob %waiting ?:(-.u.glob %done %trying))]
  !<  [@ud hash=@uv glob=(unit [? *])]
  .^(vase %gx (weld (pathify ~.glob ~) /dbug/state/noun))
--
