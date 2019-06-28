::  Set admin users
::
::::  /hoon/admins/ask/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        {who/(list ship) ~}
    ==
:-  %ask-admins  ^-  (set ship)
~?  =(~ who)  %admins-unset
(silt who)
