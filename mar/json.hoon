::
::::  /hoon/json/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  ^eyre
=,  js:eyre
=,  format
=,  html
|_  jon/json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/application/json (taco:bytes:eyre txt)]   ::  convert to %mime
  ++  txt   (crip (en-json jon))
  --
++  grab
  |%                                                    ::  convert from
  ++  mime  |=({p/mite q/octs} (fall (rush (@t q.q) apex:de-json) *json))
  ++  noun  json                                        ::  clam from %noun
  ++  numb  jone
  ++  time  jode
  --
++  grad  %mime
--
