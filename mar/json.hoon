::
::::  /hoon/core/json/mar
  ::
/?  314
  ::
::::  compute
  ::
|_  jon+json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/json (taco txt)]                     ::  convert to %mime
  ++  txt   (crip (pojo jon))
  --
++  grab
  |%                                                    ::  convert from
  ++  mime  |=({p+mite q+octs} (fall (rush (@t q.q) apex:poja) *json))
  ++  noun  json                                        ::  clam from %noun
  ++  numb  jone
  ++  time  jode
  --
++  grad  %mime
--
