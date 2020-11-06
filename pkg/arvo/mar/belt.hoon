::  belt: runtime belt structure
::
|_  =belt:dill
++  grad  %noun
::  +grab: convert from
::
++  grab
  |%
  ++  noun  belt:dill
  ++  json
    ^-  $-(^json belt:dill)
    =,  dejs:format
    %-  of
    :~  aro+(su (perk %d %l %r %u ~))
        bac+ul
        ctl+`$-(json @c)`so
        del+ul
        met+`$-(json @c)`so
        ret+ul
        txt+`$-(json (list @c))`sa
    ==
  --
::  +grow: convert to
::
++  grow
  |%
  ++  noun  belt
  --
--
