::
::::  /hoon/aqua/effect/mar
  ::
/?    310
::
/-  *aquarium
|_  f=aqua-effect
++  grow
  |%
  ++  mime  [/application/octet-stream 0 *@] :: (as-octs:mimes:html (jam f))]
  ++  noun  f
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  aqua-effect                                 ::  clam from %noun
  --
++  grad  %noun
--
