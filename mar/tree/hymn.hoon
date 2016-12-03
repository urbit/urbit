::
::::  /hoon/hymn/tree/mar
  ::
/?    310
=,  mimes:html
|_  own/manx
::
++  grow                                                ::  convert to
  |%
  ++  html  (crip (poxo own))                           ::  convert to %html
  ++  mime  [/text/html (as-octs html)]                 ::  convert to %mime
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
--        --
