::  azio/ecliptic: contract read calls for ecliptic.sol
::
::TODO  implement rest of the ecliptic.sol read functions
::
/+  ethio, strandio, *ethereum
::
^|
|_  [url=@ta contract=address]
++  call-id
  |=  [func=@t args=(list @t)]
  ^-  @t
  :((cury cat 3) func '(' (reel (join ', ' args) (cury cat 3)) ')')
::
++  get-spawn-limit
  |=  [who=ship when=@da]
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'getSpawnLimit' (scot %p who) (scot %da when) ~)
    :+  contract
      'getSpawnLimit(uint32,uint256)'
    :~  [%uint `@`who]
        [%uint (unt:chrono:userlib when)]
    ==
  %-  pure:m
  (decode-results:rpc res [%uint]~)
--
