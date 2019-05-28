/-  hall
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:http-server term]
      [%peer wire dock path]
      [%quit ~]
      [%poke wire dock poke]
      [%peer wire dock path]
      [%pull wire dock ~]
      [%diff diff]
  ==
::
+$  diff
  $%  [%hall-rumor rumor:hall]
      [%chat-streams streams]
      [%chat-update update]
  ==
::
+$  poke
  $%  [%hall-action action:hall]
  ==
::
+$  state
  $%  [%0 str=streams]
  ==
::
+$  streams
  $:  ::  inbox config
      ::
      inbox=config:hall
      ::  names and configs of all circles we know about
      ::
      configs=(map circle:hall (unit config:hall))
      ::  messages for all circles we know about
      ::
      messages=(map circle:hall (list envelope:hall))
      ::
      ::
      circles=(set name:hall)
      ::
      ::
      peers=(map circle:hall (set @p))
  ==
::
+$  update
  $%  [%inbox con=config:hall]
      [%message cir=circle:hall env=envelope:hall]
      [%config cir=circle:hall con=config:hall]
      [%circles cir=(set name:hall)]
      [%peers cir=circle:hall per=(set @p)]
  ==
::
+$  action  [%actions lis=(list action:hall)]
::
--
::
