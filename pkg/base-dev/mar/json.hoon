::
::::  /hoon/json/mar
  ::
/?    310
  ::
::::  compute
  ::
|_  jon=json
::
++  grow                                                   :: convert to
  |%
  ++  mime  [/application/json (as-octs:mimes:html -:txt)] :: convert to %mime
  ++  txt   [(crip (en-json:html jon))]~
  --
++  grab                                                   :: convert from
  |%
  ++  mime  |=([p=mite q=octs] (fall (rush (@t q.q) apex:de-json:html) *json))
  ++  noun  json                                           :: clam from %noun
  ++  numb  numb:enjs:format
  ++  time  time:enjs:format
  --
++  grad  %mime
--
