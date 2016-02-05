::
::::  /hoon/elem/mar
  ::
/?  314
|_  own=manx
::
++  grow                                                ::  convert to
  |%
  ++  hymn  ;html:(head body:"+{own}") ::  convert to %hymn
  ++  html  (crip (poxo hymn))                          ::  convert to %html
  ++  mime  [/text/html (taco html)]                    ::  convert to %mime
  --
++  garb  /snip                                         ::  grabbed by
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
--        --
