::
::::  /mar/hall/rumor/hoon
  ::
/-    hall
/+    hall-json
::
|_  rum/rumor:hall
::
++  grab                                                :::  convert from
  |%
  ++  noun  rumor:hall                                  :::  from %noun
  ++  json                                              :::  from %json
    =,  dejs:hall-json
    =,  dejs-soft:format
    |=  a/json
    ^-  rumor:hall
    =-  (need ((of -) a))
    :~  client+ruci
        circles+(ot add+bo cir+so ~)
        public+(ot add+bo cir+circ ~)
        ::  burden not needed
        circle+ruso
    ==
  --
::
++  grow                                                :::  convert to
  |%
  ++  json                                              :::  to %json
    =,  enjs:hall-json
    =,  enjs:format
    %+  frond  -.rum
    ?+  -.rum  !!
      $client   (ruci rum.rum)
      $circles  (pairs add+b+add.rum cir+s+cir.rum ~)
      $public   (pairs add+b+add.rum cir+(circ cir.rum) ~)
      ::  burden not needed
      $circle   (ruso rum.rum)
    ==
  --
--
