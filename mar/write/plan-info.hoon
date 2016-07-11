::
::::  /hoon/paste/write/mar
  ::
/?    310
!:
|_  {who/@txname loc/@txloc}
++  grab
  |%
  ++  noun  {@txname @txloc}
  ++  json  
    (corl need =>(jo (ot who+so loc+so ~)))
  --
--
