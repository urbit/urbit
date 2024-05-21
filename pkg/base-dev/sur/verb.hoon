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
      sap=path
      =cause
      effects=(list effect)
  ==
::
+$  cause
  $%  [%on-init ~]
      [%on-load ~]
      [%on-poke =mark mug=@ux]
      [%on-watch =path]
      [%on-leave =path]
      [%on-agent =wire =sign]
      [%on-arvo =wire vane=term sign=term]
      [%on-fail =term]
  ==
::
+$  sign
  $%  [%poke-ack ack=?]
      [%watch-ack ack=?]
      [%kick ~]
      [%fact =mark mug=@ux]
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
