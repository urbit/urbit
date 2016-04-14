::  Twitter status
::
::::  /hoon/post/twit/mar
  ::
/+  twitter, httr-to-json
|_  post:twitter
++  grab
  |%
  ++  noun  post:twitter
  ++  json  (corl need post:parse:twitter)
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  --
++  grow
  |%
  ++  tank  >[+<]<
  --
--
