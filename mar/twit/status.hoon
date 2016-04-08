::  Twitter status
::
::::  /hoon/status/twit/mar
  ::
/+  twitter, httr-to-json
|_  stat:twitter
++  grab
  |%
  ++  noun  stat:twitter
  ++  json  (corl need stat:parse:twitter)
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  --
++  grow
  |%
  ++  tank  >[+<]<
  --
--
