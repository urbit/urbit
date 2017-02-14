::
::::  /hoon/ships/mar
  ::
/?    310
=,  format
|_  all/(list ship)
++  grab  |%
          ++  noun  (list ship)
          ++  json  (ar (su fed:ag)):dejs
          --
++  grow  |%
          ++  json  `^json`[%a (turn all ship:enjs)]
          --
++  grad  %json
--
