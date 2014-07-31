::
::::  /hoon/core/hymn/pro
  ::
/?  314
  ::
::::  compute
  ::
|_  man=manx
::
++  grow                                                ::  convert to
  |%
  ++  mime                                              ::  convert to %mime
    =+  htm=(crip (xmlt | man ~))
    [/text/html (taco htm)]
  --
--

