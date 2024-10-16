::  |pp-config: update %dojo's pretty printer settings. 
::
::      > :dojo|pp-config [%verbosity %lest]
::    set verbosity to %lest
::
::      > :dojo|pp-config [%depth 10]
::    set maximum depth to 10
::
::      > :dojo|pp-config [%add-custom %map map-printer]
::    set custom printer for maps
::
::::  /hoon/pp-config/hood/gen
  ::
/?    310
::
::::
  ::
:-  %say
=>  |%
    +$  arg
      $%  [%depth p=@ud]
          [%verbosity p=?(%base %lest %most)]
          [%add-custom p=term q=ppin:us]
          [%del-custom p=term]
          [%set-all-custom p=(map term ppin:us)]
      ==
    --
|=  $:  [now=@da eny=@ bec=beak]
        [[=arg ~] ~]
    ==
[%pp-config arg]

