::
::::  /hoon/core/elem/mar
  ::
/?  314
|_  own=manx
::
++  grow                                                ::  convert to
  |%
  ++  hymn  ;html:(head:title:"Untitled" body:"+{own}") ::  convert to %hymn
  ++  html  (crip (poxo hymn))                          ::  convert to %html
  ++  mime  [/text/html (taco html)]                    ::  convert to %mime
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
--        --
