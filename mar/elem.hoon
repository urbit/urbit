::
::::  /hoon/elem/mar
  ::
/?    310
=,  html
|_  own/manx
::
++  grow                                                ::  convert to
  |%
  ++  hymn  ;html:(head body:"+{own}")                  ::  convert to %hymn
  ++  html  (crip (en-xml hymn))                ::  convert to %html
  ++  mime  [/text/html (taco:bytes:eyre html)]         ::  convert to %mime
  --
++  garb  /snip                                         ::  grabbed by
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
--        --
