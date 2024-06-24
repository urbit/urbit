::  Drum: Connect to a console-enabled app
::
::::  /hoon/link/hood/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  [@ @ @ our=@p ^]
        arg=$?([dap=term ~] [who=ship dap=term ~])
        drum-session=@ta
    ==
:-  %drum-link
:-  drum-session
?~  +.arg
  [our dap.arg]
[who.arg dap.arg]
