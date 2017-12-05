::  Eyre: show web base path
::
::::  /hoon/serving/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  [[now=time @ our=ship ^] ~ ~]
:-  %noun
.^(path %e (en-beam:format [our %serv da+now] /))
