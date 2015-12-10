::
::::  /hoon/core/otf/pro
  ::
/?  314
  ::
::::  compute
  ::
|_  otf+mime
::
++  grow                                                ::  convert to
  |%
  ++  mime  otf                                         ::  convert to %mime
  --
++  grab
  |%
  ++  mime  |=({mite p+octs} [/font/otf p])
  ++  noun  ^mime
  --
++  grad  %mime
--
