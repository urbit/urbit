::  Make a desk (or subtree) publicly readable
::
::::  /gen/hood/public/hoon
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=[des=desk may=?(~ [pax=path ~])] ~]
    ==
:-  %kiln-permission
[des ?~(may / pax.may) &]:arg
