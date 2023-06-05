^?  |%
++  remake-set
  |*  s=(tree)
  (sy ~(tap in s))
::
++  remake-map
  |*  m=(tree)
  (molt ~(tap by m))
::
++  remake-jug
  |*  j=(tree [* (tree)])
  %-  remake-map
  (~(run by j) remake-set)
::
++  remake-map-of-map
  |*  mm=(tree [* (tree)])
  %-  remake-map
  (~(run by mm) remake-map)
--
