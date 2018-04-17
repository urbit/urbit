/-  constitution, ethereum
/+  ethereum
=>  [^constitution ethereum]
|%
++  parse-id
  |=  id=@t
  ^-  ships:function
  |^
    %+  rash  id
    ;~  pose
      (function %ships 'ships' shipname)
      (function %get-spawned 'getSpawned' shipname)
      (function %dns-domains 'dnsDomain' dem:ag)
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
