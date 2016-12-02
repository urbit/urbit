::
::::  /hoon/ships/mar
  ::
/?    310
=,  js:eyre
=,  format
|_  all/(list ship)
++  grab  |%
          ++  noun  (list ship)
          ++  json  (ar (su fed:ag)):dejs
          --
++  grow  |%
          ++  json  `^json`[%a (turn all |=(a/ship (jape +:<a>)))]
          --
++  grad  %json
--
