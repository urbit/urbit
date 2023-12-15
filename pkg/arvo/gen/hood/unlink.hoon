::  Drum: disconnect from console app
::
::::  /hoon/unlink/hood/gen
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
:-  %drum-unlink
:-  drum-session
?~  +.arg
  [our dap.arg]
[who.arg dap.arg]
