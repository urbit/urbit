::
::::  /hoon/ships/mar
  ::
/?    310
=,  format
|_  all=(list ship)
++  grab  |%
          ++  noun  (list ship)
          ++  json  (ar (su ;~(pfix sig fed:ag))):dejs
          --
++  grow  |%
          ++  json  `^json`[%a (turn all (cork (cury scot %p) (lead %s)))]
          --
++  grad  %json
--
