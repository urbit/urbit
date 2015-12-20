::
::::  /hoon/core/otf/pro
  ::
/?  314
  ::
::::  compute
  ::
|_  wof/mime
::
++  grow                                                ::  convert to
  |%
  ++  mime  wof                                         ::  convert to %mime
  --
++  grab
  |%
  ++  mime  |=({mite p/octs} [/application/font-woff p])
  ++  noun  ^mime
  --
++  grad  %mime
--
