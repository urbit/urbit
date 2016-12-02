::  List of twitter users
::
::::  /hoon/usel/twit/mar
  ::
/+  twitter, httr-to-json
|_  (list who/@ta)
++  grab
  |%
  ++  noun  (list who/@ta)
  ++  json  usel:reparse:twitter
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  --
++  grow
  |%
  ++  tank  >[+<]<
  --
--
