::
::::  /mar/hall/telegrams/hoon
  ::
/-    hall
/+    hall-json
::
|_  gaz/(list telegram:hall)
::
++  grab                                                :::  convert from
  |%
  ++  noun  (list telegram:hall)                        :::  from %noun
  ::
  ++  mime                                              :::  from %mime
    |=  ^mime
    (json (rash q.q apex:de-json:html))
  ::
  ++  json                                              :::  from %json
    =,  dejs-soft:format
    |=  a/json
    ^-  (list telegram:hall)
    (need ((ar gram:dejs:hall-json) a))
  --
::
++  grow                                                :::  convert to
  |%
  ++  mime                                              :::  to %mime
    :-  /text/json
    (as-octs:mimes:html (crip (en-json:html json)))
  ::
  ++  json  a+(turn gaz gram:enjs:hall-json)            :::  to %json
  --
::
++  grad
  |%
  ++  form  %hall-telegrams
  ++  diff  |=((list telegram:hall) +<)
  ++  pact  |=((list telegram:hall) +<)
  ++  join  |=  {(list telegram:hall) (list telegram:hall)}
            `(unit mime)`~
  --
--
