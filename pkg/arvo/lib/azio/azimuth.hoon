::  azio/azimuth: contract read calls for azimuth.sol
::
::TODO  implement rest of the azimuth.sol read functions
::
/+  ethio, strandio, *ethereum, *azimuth
::
^|
|_  [url=@ta contract=address]
++  call-id
  |=  [func=@t args=(list @t)]
  ^-  @t
  :((cury cat 3) func '(' (reel (join ', ' args) (cury cat 3)) ')')
::
++  get-owned-points
  |=  =address
  =/  m  (strand:strandio ,(list @p))
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'getOwnedPoints' (scot %ux address) ~)
    [contract 'getOwnedPoints(address)' [%address address]~]
  %-  pure:m
  ;;  (list @p)
  (decode-results:rpc res [%array %uint]~)
::
++  get-transferring-for
  |=  =address
  =/  m  (strand:strandio ,(list @p))
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'getTransferringFor' (scot %ux address) ~)
    [contract 'getTransferringFor(address)' [%address address]~]
  %-  pure:m
  ;;  (list @p)
  (decode-results:rpc res [%array %uint]~)
::
++  rights
  |=  who=ship
  =/  m  (strand:strandio ,deed:eth-noun)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'rights' (scot %p who) ~)
    [contract 'rights(uint32)' [%uint `@`who]~]
  %-  pure:m
  (decode-results:rpc res deed:eth-type)
::
++  is-spawn-proxy
  |=  [who=ship =address]
  =/  m  (strand:strandio ,?)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'isSpawnProxy' (scot %p who) (scot %ux address) ~)
    :+  contract
      'isSpawnProxy(uint32,address)'
    :~  [%uint `@`who]
        [%address address]
    ==
  %-  pure:m
  (decode-results:rpc res [%bool]~)
::
++  is-live
  |=  who=ship
  =/  m  (strand:strandio ,?)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'isLive' (scot %p who) ~)
    [contract 'isLive(uint32)' [%uint `@`who]~]
  %-  pure:m
  (decode-results:rpc res [%bool]~)
::
++  get-sponsor
  |=  who=ship
  =/  m  (strand:strandio ,ship)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'getSponsor' (scot %p who) ~)
    [contract 'getSponsor(uint32)' [%uint `@`who]~]
  %-  pure:m
  ^-  @
  (decode-results:rpc res [%uint]~)
::
++  get-spawn-count
  |=  who=ship
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'getSpawnCount' (scot %p who) ~)
    [contract 'getSpawnCount(uint32)' [%uint `@`who]~]
  %-  pure:m
  (decode-results:rpc res [%uint ~])
::
++  get-spawned
  |=  who=ship
  =/  m  (strand:strandio ,(list ship))
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'getSpawned' (scot %p who) ~)
    [contract 'getSpawned(uint32)' [%uint `@`who]~]
  %-  pure:m
  ;;  (list ship)
  (decode-results:rpc res [%array %uint]~)
--
