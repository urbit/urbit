::
::::  /hoon/json/tree/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  mimes:html
=,  html
|_  jon/json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/json (as-octs (en-json jon))]        ::  convert to %mime
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  json                                        ::  clam from %noun
  --
--
