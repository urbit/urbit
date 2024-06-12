::
::::  /hoon/md/mar
  ::
/?    310
::
=,  format
=,  mimes:html
|_  txt=@
::
++  grab                                                ::  convert from
  |%
  ++  mime  |=((pair mite octs) q.q)
  ++  noun  ,@                                        ::  clam from %noun
  --
++  grow
  |%
  ++  mime  [/text/plain (as-octs txt)]
  ++  noun  txt
  --
++  grad  %mime
--
