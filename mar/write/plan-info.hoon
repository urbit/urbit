::
::::  /hoon/paste/write/mar
  ::
/?    310
|_  [who=@txname loc=@txloc]
++  grab
  |%
  ++  noun  [@txname @txloc]
  ++  json
    (corl need =>(dejs-soft:format (ot who+so loc+so ~)))
  --
--
