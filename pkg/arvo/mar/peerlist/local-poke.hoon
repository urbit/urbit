/-  *peerlist
::
|_  =local-poke
::
++  grab
  |%
  ++  noun  ^local-poke
  ::
  ++  json
    =,  dejs:format
    %-  of
    |^  :~  [%add (ot 'who'^ship 'group'^(mu so) ~)]
            [%remove (ot 'who'^ship 'group'^(mu so) ~)]
            [%fill (ot 'tag'^so ~)]
            [%clear (ot 'tag'^so ~)]
            [%reset ul]
            [%export (ot 'spur'^path 'which'^tags ~)]
            [%import (ot 'path'^path ~)]
            [%settings sets]
            [%debug ul]
        ==
    ++  ship  (su ;~(pfix sig fed:ag))
    ++  path  (cu stab so)
    ++  tags  (cu ~(gas in *(set tag)) (ar so))
    ++  sets  (ot 'default-public'^bo ~)
    --
  --
::
++  grow
  |%
  ++  noun  local-poke
  --
--