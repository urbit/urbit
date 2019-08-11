/-  hall
::
|%
+$  diff
  $%  [%hall-rumor rumor:hall]
      [%chat-update update]
      [%chat-config streams]
      [%json json]
  ==
::
+$  poke
  $%  [%hall-action action:hall]
      [%launch-action [@tas path @t]]
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
      [%messages cir=circle:hall start=@ud end=@ud env=(list envelope:hall)]
      [%config cir=circle:hall con=config:hall]
      [%circles cir=(set name:hall)]
      [%peers cir=circle:hall per=(set @p)]
      [%delete cir=circle:hall]
  ==
::
+$  action  [%actions lis=(list action:hall)]
--