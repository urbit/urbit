::  Twitter statuses
::
::::  /hoon/feed/twit/mar
  ::
/-  hall
/+  twitter, httr-to-json, old-zuse
=,  old-zuse
=,  format
|_  fed/(list post:twitter)
++  grab
  |%
  ++  noun  (list post:twitter)
  ++  json  (ar:dejs post:reparse:twitter)
  ++  httr  (cork httr-to-json json)  ::  XX mark translation
  --
++  grow
  |%
  ++  tank  >[fed]<
  ++  hall-speeches
    =+  r=render:twitter
    %+  turn  fed
    |=  a/post:twitter  ^-  speech:hall
    :*  %api  %twitter
        who.a
        (user-url.r who.a)
        txt.a
        txt.a
        (post-url.r who.a id.a)
        (joba now+(jode now.a))
    ==
  --
--
