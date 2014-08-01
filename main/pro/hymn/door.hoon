::
::::  /hoon/core/hymn/pro
  ::
/?  314
  ::
::::  compute
  ::
|_  own=manx
::
++  grow                                                ::  convert to
  |%
  ++  html  (crip (xmlt | own ~))                       ::  convert to %html
  ++  mime  [/text/html (taco html)]                    ::  convert to %mime
  --
--

