::
::::  /hoon/rumor/talk/mar
  ::
/-    talk
/+    talk-json
::
|_  rum/rumor:talk
::
++  grab                                                ::>  convert from
  |%
  ++  noun  rumor:talk                                  ::<  from %noun
  ++  json                                              ::>  from %json
    =>  [. dejs:talk-json]  ::TODO  =,
    =,  dejs-soft:format
    |=  a/json
    ^-  rumor:talk
    =-  (need ((of -) a))
    :~  reader+rure
        friend+(ot add+bo cir+circ ~)
        ::  burden not needed
        circle+ruso
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
    =>  [. enjs:talk-json]  ::TODO  =,
    =,  enjs:format
    %+  frond  -.rum
    ?+  -.rum  !!
      $reader  (rure rum.rum)
      $friend  (pairs add+b+add.rum cir+(circ cir.rum) ~)
      ::  burden not needed
      $circle  (ruso rum.rum)
    ==
  --
--
