::
::::  /hoon/ask-mail/mar
  ::
/?    310
=,  format
=,  mimes:html
|_  txt/cord
::
++  grab                                                ::  convert from
  |%
  ++  noun  @t                                          ::  clam from %noun
  ++  json  so:dejs
  --
++  grow
  |%
  ++  psal  ;div: {(trip txt)}
  ++  mime  [text+/plain (as-octs txt)]
  --
--
