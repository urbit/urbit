::  print [len] %ames flows, sorted by number-per-ship
::
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] arg=$@(~ [len=@ ~]) ~]
:-  %noun
::
=;  flows
  ^-  (list [=ship open=@ closing=@ corked=@])
  =/  len   ?^(arg len.arg 50)
  (scag len (sort flows |=([[@ a=@ud *] @ b=@ud *] (gth a b))))
::
=/  peers-map
  .^  (map ship ?(%alien %known))
      %ax  /(scot %p p.bec)//(scot %da now)/peers
  ==
=/  peers=(list ship)
  %+  murn  ~(tap by peers-map)
  |=  [=ship val=?(%alien %known)]
  ?:  =(ship p.bec)
    ~  ::  this is weird, but we saw it
  ?-  val
    %alien  ~
    %known  (some ship)
  ==
::
^-  (list [=ship open=@ closing=@ corked=@])
%+  turn  peers
|=  =ship
=+  .^  =ship-state:ames
        %ax  /(scot %p p.bec)//(scot %da now)/peers/(scot %p ship)
    ==
=/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
=/  closing  ~(wyt in closing.peer-state)
=/  corked  ~(wyt in corked.peer-state)
=/  open  (sub ~(wyt by snd.peer-state) closing)
[ship open closing corked]
