::
::::  /hoon/json/collection/mar
  ::
/?    309
  ::
::::  compute
  ::
=,  eyre
=,  format
=,  html
|_  jon/json
::
++  grow                                                ::  convert to
  |%
  ++  txt   (crip (en-json jon))
  ++  json  jon
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  ^json                                        ::  clam from %noun
  ++  json  ^json
  --
++  grad  %mime
--
