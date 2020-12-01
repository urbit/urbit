::
::::  /hoon/mime/mar
  ::
/?    310
::
^|
|_  own/mime
++  grow
  ^?
  |%
  ++  jam  `@`q.q.own
  --
::
++  grab                                                ::  convert from
  ^?
  |%
  +$  noun  mime                                  ::  clam from %noun
  ++  tape
    |=(a=^tape [/application/x-urb-unknown (as-octt:mimes:html a)])
  --
++  grad
  ^?
  |%
  ++  form  %mime
  ++  diff  |=(mime +<)
  ++  pact  |=(mime +<)
  ++  join  |=({mime mime} `(unit mime)`~)
  --
--
