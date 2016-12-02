::
::::  /hoon/json/tree/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  bytes:eyre
=,  html
|_  jon/json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/json (tact (en-json jon))]              ::  convert to %mime
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  json                                        ::  clam from %noun
  --
--
