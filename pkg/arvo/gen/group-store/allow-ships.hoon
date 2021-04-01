::  group-store|allow-ships: remove ships from banlist
::
/-  *group, *group-store
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [[=ship =term ships=(list ship) ~] ~]
    ==
:-  %group-update-0
^-  action
[%change-policy [ship term] %open %allow-ships (sy ships)]
