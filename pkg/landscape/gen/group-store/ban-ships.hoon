::  group-store|ban-ships: ban members from a group
::
/-  *group, *group-store
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [[=ship =term ships=(list ship) ~] ~]
    ==
:-  %group-update-0
^-  action
[%change-policy [ship term] %open %ban-ships (sy ships)]
