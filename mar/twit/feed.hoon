::  Twitter statuses
::
::::  /hoon/feed/twit/mar
  ::
/+  twitter, httr-to-json
|_  (list post:twitter)
++  grab
  |%
  ++  noun  (list post:twitter)
  ++  json  (corl need (ar:jo post:parse:twitter))
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  --
++  grow
  |%
  ++  tank  >[+<]<
  --
--
