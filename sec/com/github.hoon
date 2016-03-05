::  Test url +https://api.github.com/user
::
::::  /hoon/github/com/sec
  ::
/+    basic-auth
!:
|_  {bal/(bale keys:basic-auth) $~}
++  out  (basic-auth bal)
--
