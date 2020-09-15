:: TODO: the RTS currently has a problem with executing data jetted values. The
:: cas version below works, but the %+ version returns r even though these
:: should be equivalent.
::
:: =/  r  (rit 5)
:: %+  r  <x 1>  <y 2>
:: (cas r <x 1> <y 2>)

=/  one-two-three
  (gulf 1 3)

(turn one-two-three (add 3))
::(snag 1 one-two-three)
::(weld (gulf 1 3) (gulf 4 6))
