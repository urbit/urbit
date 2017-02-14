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
  ++  mime  [/text/json (as-octt (en-json jon))]        ::  convert to %mime
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  json                                        ::  clam from %noun
  --
--
