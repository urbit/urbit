|%
++  aqua-event
  $%  [%init-ship who=ship]
      [%pause-events who=ship]
      [%event who=ship ovo=unix-event]
  ==
::
++  aqua-effects
  ,[who=ship ovo=(list unix-effect)]
::
++  unix-event
  %+  pair  wire
  $%  [%wack p=@]
      [%whom p=ship]
      [%live p=@ud q=(unit @ud)]
      [%barn ~]
      [%boot %fake p=ship]
      unix-task
  ==
::
++  unix-effect
  %+  pair  wire
  $%  [%blit p=(list blit:dill)]
      [%send p=lane:ames q=@]
      [%doze p=(unit @da)]
      [%thus p=@ud q=(unit hiss:eyre)]
      [%ergo p=@tas q=mode:clay]
  ==
+=  pill  [boot-ova=* kernel-ova=(list unix-event) userspace-ova=(list unix-event)]
--
