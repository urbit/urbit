::  print [len] %ames flows, sorted by number-per-ship
::
:-  %say
|=  [[now=@da tick=@ud @ our=@p ^] arg=$@(~ [len=@ ~]) ~]
:-  %noun
::
=;  flows
  ^-  (list [=ship open=[out-open=@ out-closing=@ in=@ nax=@] corked=@])
  =/  len   ?^(arg len.arg 50)
  (scag len (sort flows |=([[@ [a=@ud *] *] @ [b=@ud *] *] (gth a b))))
::
=/  peers-map
  .^  (map ship ?(%alien %known))
      %ax  (en-bema [our %$ [da+now ud+tick]] /peers)
  ==
=/  peers=(list ship)
  %+  murn  ~(tap by peers-map)
  |=  [=ship val=?(%alien %known)]
  ?:  =(ship our)
    ~  ::  this is weird, but we saw it
  ?-  val
    %alien  ~
    %known  (some ship)
  ==
::
^-  (list [=ship open=[out-open=@ out-closing=@ in=@ nax=@] corked=@])
%+  turn  peers
|=  =ship
=+  .^  =ship-state:ames
        %ax  (en-bema [our %$ [da+now ud+tick]] /peers/(scot %p ship))
    ==
=/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
=/  corked  ~(wyt in corked.peer-state)
=-  [ship - corked]
::
=+  %+  roll  ~(tap in ~(key by snd.peer-state))
    |=  [b=bone [out=(list bone) in=(list bone) nax=(list bone)]]
    =/  m  (mod b 4)
    ?+  m  ~|([%odd-bone b] !!)
      %0  [[b out] in nax]
      %1  [out [b in] nax]
      %3  [out in [b nax]]
    ==
=/  [out-closing=(list bone) out-open=(list bone)]
  (skid out ~(has ^in closing.peer-state))
[(lent out-open) (lent out-closing) (lent in) (lent nax)]
