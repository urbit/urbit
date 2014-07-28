::
::::  /hoon/core/html/pro
  ::
/?  314
  ::
::::  compute
  ::
|_  htm=@t
::
++  grow                                                ::  convert to
  |%
  ++  mime                                              ::  convert to %mime
    [/text/html (met 3 htm) htm]
  --
--
