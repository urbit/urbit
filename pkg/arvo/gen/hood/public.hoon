::  Kiln: make (subtree in) desk publicly readable.
::
::::  /gen/hood/public/hoon
  ::
:-  %say
|=  $:  ^
        [arg=[des=desk may=?(~ [pax=path ~])] ~]
    ==
:-  %kiln-permission
[des ?~(may / pax.may) &]:arg
