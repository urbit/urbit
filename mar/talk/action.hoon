::
::::  /mar/talk/rumor/hoon
  ::
/-    talk
/+    talk-json
::
=,  talk
|_  dif/action
::
++  grab                                                ::>  convert from
  |%
  ++  noun  action                                      ::<  from %noun
  ++  json                                              ::>  from %json
    =,  dejs-soft:format
    =,  talk-json:de-json
    |=  a/json  ^-  action
    =-  (need ((of -) a))
    :~  create+ul
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
  --
--
