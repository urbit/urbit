::
::::  /mar/hall/speeches/hoon
  ::
/?    310
/-  hall
/+  hall,map-to-json
::
=+  hall
|_  gam/(list speech)
::
++  grab
  |%
  ++  noun  (list speech)
  --
::
++  grad
  |%
  ++  form  %hall-speeches
  ++  diff  |=((list speech) +<)
  ++  pact  |=((list speech) +<)
  ++  join  |=({(list speech) (list speech)} `(unit mime)`~)
  --
--
