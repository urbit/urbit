::  Kiln: make (subtree in) desk privately readable.
::
::::  /gen/hood/private/hoon
  ::
:-  %say
|=  $:  ^
        [arg=[des=desk may=?(~ [pax=path ~])] ~]
    ==
:-  %kiln-permission
[des ?~(may / pax.may) |]:arg
