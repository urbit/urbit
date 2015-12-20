::
::::  /hoon#oct3-move#mar
  ::
/?    314
!:
::::
  ::
=+  point={x/@ y/@}
|_  point
::
++  grab                                                ::  convert from
  |%
  ++  json  (corl need (at ni ni ~):jo)                 ::  reparse from %json
  ++  noun  point                                       ::  clam from %noun
  --
--
