::
::::  /hoon/elem/mar
  ::
/?    310
=,  mimes:encoding
=,  encoding
|_  own=manx
::
++  grad  %mime
++  grow                                                ::  convert to
  |%
  ++  hymn  ;html:(head body:"+{own}")                  ::  convert to %hymn
  ++  html  (crip (en-xml hymn))                        ::  convert to %html
  ++  mime  [/text/html (as-octs html)]                 ::  convert to %mime
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
--        --
