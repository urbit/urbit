::
::::  /mar/hall/rumor/hoon
  ::
/-    hall
/+    hall-json
::
|_  rum/rumor:hall
::
++  grab                                                ::>  convert from
  |%
  ++  noun  rumor:hall                                  ::<  from %noun
  ++  json                                              ::>  from %json
    =>  [. dejs:hall-json]  ::TODO  =,
    =,  dejs-soft:format
    |=  a/json
    ^-  rumor:hall
    =-  (need ((of -) a))
    :~  client+ruci
        public+(ot add+bo cir+circ ~)
        ::  burden not needed
        circle+ruso
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
    =>  [. enjs:hall-json]  ::TODO  =,
    =,  enjs:format
    %+  frond  -.rum
    ?+  -.rum  !!
      $client  (ruci rum.rum)
      $public  (pairs add+b+add.rum cir+(circ cir.rum) ~)
      ::  burden not needed
      $circle  (ruso rum.rum)
    ==
  --
--
