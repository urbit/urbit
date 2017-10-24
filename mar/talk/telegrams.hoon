::
::::  /hoon/telegrams/talk/mar
  ::
/-    talk
/+    talk-json
::
|_  gaz/(list telegram:talk)
::
++  grab                                                ::>  convert from
  |%
  ++  noun  (list telegram:talk)                        ::<  from %noun
  ::
  ++  mime                                              ::>  from %mime
    |=  ^mime
    (json (rash q.q apex:de-json:html))
  ::
  ++  json                                              ::>  from %json
    =,  dejs-soft:format
    |=  a/json
    ^-  (list telegram:talk)
    (need ((ar gram:dejs:talk-json) a))
  --
::
++  grow                                                ::>  convert to
  |%
  ++  mime                                              ::>  to %mime
    :-  /text/json
    (as-octs:mimes:html (crip (en-json:html json)))
  ::
  ++  json  a+(turn gaz gram:enjs:talk-json)            ::<  to %json
  --
--
