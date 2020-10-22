::
::::  /hoon/noun/mar
  ::
/?    310
!:
::::  A minimal noun mark
|_  non/*
++  grab  |%
          ++  noun  *
          --
++  grad
  |%
  ++  form  %noun
  ++  diff  |=(* +<)
  ++  mash
    |=  [a=[=ship =desk diff=*] b=[=ship =desk diff=*]]
    ^-  *
    diff.b
  ++  pact  |=(* +<)
  ++  join  |=([* *] *(unit *))
  --
--
