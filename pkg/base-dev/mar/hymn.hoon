::
::::  /hoon/hymn/mar
  ::
/?    310
=,  mimes:html
=,  html
|_  own=manx
::
++  grad  %noun
++  grow                                                ::  convert to
  |%
  ++  html  (crip (en-xml own))                         ::  convert to %html
  ++  mime  [/text/html (as-octs html)]                 ::  convert to %mime
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
--        --
