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
    |^  :*  key+(ot 'mod'^mod 'key'^bot ~)
            txt+(ar (cu taft so))
            bol
        ==
    ::
    ++  bol
      :~  aro+(su (perk %d %l %r %u ~))
          bac+ul
          del+ul
          hit+(ot 'r'^ni 'c'^ni ~)
          ret+ul
      ==
    ::
    ++  bot
      |=  jon=json
      ?+  jon  !!
        [%s *]  ((cu taft so) jon)
        [%o *]  ((of bol) jon)
      ==
    ::
    ++  mod
      |=  jon=json
      ?~  jon  ~
      ((su (perk %ctl %met %hyp ~)) jon)
    --
  --
::  +grow: convert to
::
++  grow
  |%
  ++  noun  belt
  --
--
