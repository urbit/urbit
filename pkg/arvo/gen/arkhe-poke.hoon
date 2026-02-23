::  /gen/arkhe-poke.hoon
::  Envia uma intenção para o agente Arkhe (local) e aguarda resultado.
::
/+  arkhe
:-  %say
|=  [* [goal=@tas ~] *]
^-  cage
=/  int=intent:arkhe  [goal ~ ~]
[%arkhe-handover-request !>(int)]
