::  group-store|ban-ranks: ban ranks for group
::
/-  *group, *group-store
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [[=ship =term ranks=(list rank:title) ~] ~]
    ==
:-  %group-update
^-  action
[%change-policy [ship term] %ban-ranks (sy ranks)]
