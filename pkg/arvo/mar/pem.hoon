::  .pem file to list of lines
::
=,  format
=,  mimes:html
|_  pem=wain
::
++  grab                                                ::  convert from
  |%
  ++  mime  |=((pair mite octs) (to-wain q.q))
  ++  noun  wain                                        ::  clam from %noun
  --
++  grow
  =>  v=.
  |%
  ++  mime  =>  v  [/text/plain (as-octs (of-wain pem))]
  --
++  grad  %mime
--
