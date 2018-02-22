::
::::  /hoon/edit/collections/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  mimes:html
=,  html
|_  man/manx
::
++  grow                                                ::  convert to
  |%
  ::++  mime  [/text/json (as-octt (en-json jon))]        ::  convert to %mime
  ++  elem  man
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  manx                                        ::  clam from %noun
  --
--
