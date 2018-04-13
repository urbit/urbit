/-  constitution, ethereum
|%
++  parse-id
  |=  id=@t
  ^-  ships:function:^constitution
  |^
    %+  rash  id
    ;~  pose
      (function %ships 'ships' shipname)
      (function %get-spawned 'getSpawned' shipname)
    ==
  ::
  ++  function
    |*  [tag=@tas fun=@t rul=rule]
    ;~(plug (cold tag (jest fun)) (ifix [pel per] rul))
  ::
  ++  shipname
    ;~(pfix sig fed:ag)
  --
--
