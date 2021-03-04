::
::::  /hoon/noun/mar
  ::
/?    310
!:
::::  A minimal noun mark
|_  non=*
++  grab  |%
          ++  noun  *
          --
++  grad
  |%
  ++  form  %noun
  ++  diff  |=(* +<)
  ++  pact  |=(* +<)
  ++  join  |=([* *] *(unit *))
  ++  mash  |=([[ship desk *] [ship desk *]] `*`~|(%noun-mash !!))
  --
--
