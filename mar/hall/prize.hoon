::
::::  /mar/hall/prize/hoon
  ::
/-    hall
/+    hall-json
::
|_  piz/prize:hall
::
++  grab                                                ::>  convert from
  |%
  ++  noun  prize:hall                                  ::<  from %noun
  ++  json                                              ::>  from %json
    =>  [. dejs:hall-json]  ::TODO  =,
    =,  dejs-soft:format
    |=  a/json
    ^-  prize:hall
    =-  (need ((of -) a))
    :~  client+pici
        public+(as circ)
        ::  burden not needed
        ::  report not needed
        circle+pack
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
    =>  [. enjs:hall-json]  ::TODO  =,
    =,  enjs:format
    %+  frond  -.piz
    ?+  -.piz  !!
      $client  (pici +.piz)
      $public  (sa cis.piz circ)
      ::  burden not needed
      ::  report not needed
      $circle  (pack +.piz)
    ==
  --
--
