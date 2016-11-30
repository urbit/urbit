::
::::  /hoon/ships/mar
  ::
/?    310
=,  js:eyre
|_  all/(list ship)
++  grab  |%
          ++  noun  (list ship)
          ++  json  (corl need (ar (su fed:ag)):jo)
          --
++  grow  |%
          ++  json  `^json`[%a (turn all |=(a/ship (jape +:<a>)))]
          --
++  grad  %json
--
