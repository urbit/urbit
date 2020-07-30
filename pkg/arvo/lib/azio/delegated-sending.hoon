::  azio/delegated-sending: contract read calls for delegatedsending.sol
::
::TODO  implement rest of the delegated-sending.sol read functions
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
++  pools
  |=  [inviter=ship star=ship]
  ?>  ?=(%king (clan:title star))
  =/  m  (strand:strandio ,@ud)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'pools' (scot %p inviter) (scot %p star) ~)
    :+  contract
      'pools(uint32,uint16)'
    :~  [%uint `@`inviter]
        [%uint `@`star]
    ==
  %-  pure:m
  (decode-results:rpc res [%uint]~)
::
++  get-pool
  |=  inviter=ship
  =/  m  (strand:strandio ,ship)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'getPool' (scot %p inviter) ~)
    [contract 'getPool(uint32)' [%uint `@`inviter]~]
  %-  pure:m
  ^-  @
  (decode-results:rpc res [%uint]~)
::
++  get-pool-stars
  |=  inviter=ship
  =/  m  (strand:strandio ,(list ship))
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'getPoolStars' (scot %p inviter) ~)
    [contract 'getPoolStars(uint32)' [%uint `@`inviter]~]
  %-  pure:m
  ;;  (list ship)
  (decode-results:rpc res [%array %uint]~)
::
::NOTE  result of ~zod ship wasn't invited
++  invited-by
  |=  invitee=ship
  =/  m  (strand:strandio ,ship)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'invitedBy' (scot %p invitee) ~)
    [contract 'invitedBy(uint32)' [%uint `@`invitee]~]
  %-  pure:m
  ^-  @
  (decode-results:rpc res [%uint]~)
::
--
