::  Twitter statuses
::
::::  /hoon/feed/twit/mar
  ::
/-  talk
/+  twitter, httr-to-json, old-zuse
=,  old-zuse
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
  ++  talk-speeches
    =+  r=render:twitter
    %+  turn  fed
    |=  a/post:twitter  ^-  speech:talk
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
