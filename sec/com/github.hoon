::  Test url +https://api.github.com/user
::
::::  /hoon/github/com/sec
  ::
/+    basic-auth
!:
|_  {bal/(bale keys:basic-auth) $~}
++  aut  ~(standard basic-auth bal)
++  out  out-adding-header:aut
--
