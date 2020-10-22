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
    |=(a/_"" [/application/x-urb-unknown (as-octt:mimes:html a)])
  --
++  grad
  ^?
  |%
  ++  form  %mime
  ++  diff  |=(mime +<)
  ++  mash
    |=  [a=[=ship =desk diff=mime] b=[=ship =desk diff=mime]]
    ^-  mime
    diff.b
  ++  pact  |=(mime +<)
  ++  join  |=({mime mime} `(unit mime)`~)
  --
--
