::  blit: runtime blit structure
::
/+  dill
::
|_  =blit:dill
++  grad  %noun
::  +grab: convert from
::
++  grab
  |%
  ++  noun  blit:dill
  --
::  +grow: convert to
::
++  grow
  |%
  ++  noun  blit
  ++  json  (blit:enjs:dill blit)
  --
--
