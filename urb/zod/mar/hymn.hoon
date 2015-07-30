::
::::  /hoon/core/hymn/mar
  ::
/?  314
|_  own=manx
::
++  grow                                                ::  convert to
  |%
  ++  html  (crip (poxo own))                          ::  convert to %html
  ++  mime  [/text/html (taco html)]                    ::  convert to %mime
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
--        --
