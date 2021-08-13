::  azio/linear: contract read calls for linearstarrelease.sol
::
::TODO  implement rest of the read functions
::
/+  ethio, strandio, *ethereum, *azimuth
::
^|
|_  [url=@ta contract=address]
+$  batch
  $:  windup=@ud
      rate-unit=@ud
      withdrawn=@ud
      rate=@ud
      amount=@ud
      approved=address
  ==
::
++  call-id
  |=  [func=@t args=(list @t)]
  ^-  @t
  :((cury cat 3) func '(' (reel (join ', ' args) (cury cat 3)) ')')
::
++  batches
  |=  who=address
  =/  m  (strand:strandio ,batch)
  ^-  form:m
  ;<  res=@t  bind:m
    %^  read-contract:ethio  url
      `(call-id 'batches' (scot %ux who) ~)
    [contract 'batches(address)' [%address who]~]
  %-  pure:m
  %+  decode-results:rpc  res
  :~  %uint
      %uint
      %uint
      %uint
      %uint
      %address
  ==
--