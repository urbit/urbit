::
::::  /hoon/core/hymn/pro
  ::
/?  314
  ::
::::  compute
  ::
|_  jon=json
::
++  grow                                                ::  convert to
  |%
  ++  mime                                              ::  convert to %mime
    =+  htm=(crip (pojo jon))
    [/text/html (taco htm)]
  --
--

