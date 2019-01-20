::
::::  /hoon/x-urb/mar
  ::
/?    310
=,  format
=,  mimes:html
=,  html
::
|_  max=manx
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/html (as-octs html)]                 ::  convert to %mime
  ++  html  (crip (en-xml hymn))                        ::  convert to %html
  ++  hymn  max
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  manx                                        ::  clam from %noun
  --
--
