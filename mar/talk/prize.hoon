::
::::  /hoon/prize/talk/mar
  ::
/-    talk
/+    talk-json
::
|_  piz/prize:talk
::
++  grab                                                ::>  convert from
  |%
  ++  noun  prize:talk                                  ::<  from %noun
  ++  json                                              ::>  from %json
    =>  [. dejs:talk-json]  ::TODO  =,
    =,  dejs-soft:format
    |=  a/json
    ^-  prize:talk
    =-  (need ((of -) a))
    :~  reader+pire
        friend+(as circ)
        ::  burden not needed
        circle+pack
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
    =>  [. enjs:talk-json]  ::TODO  =,
    =,  enjs:format
    %+  frond  -.piz
    ?+  -.piz  !!
      $reader  (pire +.piz)
      $friend  (sa cis.piz circ)
      ::  burden not needed
      $circle  (pack +.piz)
    ==
  --
--
