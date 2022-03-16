::
::::  /hoon/json/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  eyre
=,  format
=,  mimes:html
|_  jon=json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/application/json (as-octs -:txt)]         ::  convert to %mime
  ++  txt   [(crip (en-json:html jon))]~
  --
++  grab                                                ::  convert from
  |%
  ++  mime  |=([p=mite q=octs] (fall (de-json:html (@t q.q)) *json))
  ++  noun  json                                        ::  clam from %noun
  ++  numb  numb:enjs
  ++  time  time:enjs
  --
++  grad  %mime
--
