::
::::  /hoon/json/tree/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  markup
|_  jon/json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/json (tact (pojo jon))]              ::  convert to %mime
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  json                                        ::  clam from %noun
  --
--
