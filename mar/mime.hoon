::
::::  /hoon/core/mime/mar
  ::
/?  314
!:
|_  own+mime
++  grab                                                ::  convert from
  |% 
  ++  noun  mime                                        ::  clam from %noun
  ++  tape  |=(a+__("") [/application/x-urb-unknown (tact a)])
  --
++  grad
  |%
  ++  form  %mime
  ++  diff  |=(mime +<)
  ++  pact  |=(mime +<)
  ++  join  |=({mime mime} `(unit mime)`~)
  --
--
