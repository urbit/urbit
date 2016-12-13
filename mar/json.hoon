::
::::  /hoon/json/mar
  ::
/?    310
  ::
::::  compute
  ::
|_  jon/json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/application/json (tact (pojo jon))]       ::  convert to %mime
  ++  txt   (lore (crip (pojo jon)))
  --
++  grab
  |%                                                    ::  convert from
  ++  mime  |=({p/mite q/octs} (fall (rush (@t q.q) apex:poja) *json))
  ++  noun  json                                        ::  clam from %noun
  ++  numb  jone
  ++  time  jode
  --
++  grad  %mime
--
