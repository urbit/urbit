::
::::  /hoon/json/tree/mar
  ::
/?  314
  ::
::::  compute
  ::
|_  jon=json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/json (tact (pojo jon))]              ::  convert to %mime
  --
++  grab
  |%                                                    ::  convert from
::  ++  mime  |=([p=mite q=octs] (fall (rush (,@t q.q) apex:poja) *json))
  ++  noun  json                                        ::  clam from %noun
  --
--
