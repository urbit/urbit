::
::::  /hoon/speeches/talk/mar
  ::
/?    310
/-  talk
/+  talk,map-to-json
::
=+  talk
|_  gam/(list speech)
::
++  grab
  |%
  ++  noun  (list speech)
  --
::
++  grad
  |%
  ++  form  %talk-speeches
  ++  diff  |=((list speech) +<)
  ++  pact  |=((list speech) +<)
  ++  join  |=({(list speech) (list speech)} `(unit mime)`~)
  --
--
