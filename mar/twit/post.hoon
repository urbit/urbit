::  Twitter status
::
::::  /hoon/post/twit/mar
  ::
/+  twitter, httr-to-json
|_  post:twitter
++  grab
  |%
  ++  noun  post:twitter
  ++  json  post:reparse:twitter
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  --
++  grow
  |%
  ++  tank  >[+<]<
  --
--
