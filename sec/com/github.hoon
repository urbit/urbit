::  Test url +https://api.github.com/user
::
::::  /hoon/github/com/sec
  ::
/+    basic-auth
::
|_  {bal/(bale:eyre keys:basic-auth) $~}
++  aut  ~(standard basic-auth bal ~)
++  filter-request  out-adding-header:aut
--
