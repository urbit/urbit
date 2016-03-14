::  Twitter statuses
::
::::  /hoon/feed/twit/mar
  ::
/+  twitter, httr-to-json
|_  (list stat:twitter)
++  grab
  |%
  ++  noun  (list stat:twitter)
  ++  json  (corl need (ar:jo stat:parse:twitter))
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  --
++  grow
  |%
  ++  tank  >[+<]<
  --
--
