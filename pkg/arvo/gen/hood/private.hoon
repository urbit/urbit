::  Kiln: make (subtree in) desk privately readable.
::
::::  /gen/hood/private/hoon
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=[des=desk may=?(~ [pax=path ~])] ~]
    ==
:-  %kiln-permission
[des ?~(may / pax.may) |]:arg
