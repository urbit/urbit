::  |install: install the .rem desk from .her into local .lac desk
::
::      > |install ~zod %landscape
::    installs ~zod's %landscape desk into our %landscape desk.
::
::      > |install ~zod %landscape, =local %portrait
::    installs ~zod's %landscape desk into our %portrait desk.
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[her=@p rem=desk ~] local=@tas]
    ==
=/  loc=desk  ?:(=(%$ local) rem local)
[%kiln-install loc her rem]
