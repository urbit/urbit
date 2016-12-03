::
::::  /hoon/jam-crub/mar
  ::
/?    310
::
=,  mimes:html
|_  mud/@
++  grow
  |%
  ++  mime  [/application/octet-stream (as-octs mud)]
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  @                                           ::  clam from %noun
  ++  mime  |=({* octs} q)
  --
++  grad  %mime
--
