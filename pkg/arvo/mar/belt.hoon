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
    |=  jon=^json
    ^-  belt:dill
    ?:  ?=([%s *] jon)
      (taft p.jon)
    =,  dejs:format
    %.  jon
    %-  of
    |^  :*  mod+(ot 'mod'^mod 'key'^bot ~)
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
      |=  j=json
      ^-  bolt:dill
      ?+  j  !!
        [%s *]  (taft p.j)
        [%o *]  ((of bol) j)
      ==
    ::
    ++  mod
      |=  j=json
      ((su (perk %ctl %met %hyp ~)) j)
    --
  --
::  +grow: convert to
::
++  grow
  |%
  ++  noun  belt
  --
--
