|%
+$  event
  $%  [%on-init ~]
      [%on-load ~]
      [%on-poke =mark]
      [%on-watch =path]
      [%on-leave =path]
      [%on-agent =wire sign=term]
      [%on-arvo =wire vane=term sign=term]
      [%on-fail =term]
  ==
::
+$  event-plus
  $:  act=@ud
      now=@da
      src=@p
      =cause
      effects=(list effect)
  ==
::
+$  cause
  $%  [%on-init ~]
      [%on-load ~]
      [%on-poke =mark]
      [%on-watch =path]
      [%on-leave =path]
      [%on-agent =wire sign=term mug=@ux]  ::TODO  %fact should show mark?
      [%on-arvo =wire vane=term sign=term]
      [%on-fail =term]
  ==
::
+$  effect
  $%  [%poke =wire =gill:gall =mark mug=@ux]
      [%watch =wire =gill:gall =path]
      [%leave =wire =gill:gall]
      [%fact paths=(list path) =mark mug=@ux]
      [%kick paths=(list path)]
      [%arvo =wire vane=term task=term]
  ==
--
