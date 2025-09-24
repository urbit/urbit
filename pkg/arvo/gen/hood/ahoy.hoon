::  Migrate peer to Directed Messaging
::
::    (by default it runs on dry mode and forces a test migration)
::
::::  /hoon/ahoy/hood/gen
  ::
/?    310
:-  %say
|=([^ [who=ship ~] dry=_& ~] helm-send-ahoy/who^dry^force=%.y)
