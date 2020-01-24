::  eth/get-timestamps: query ethereum block timestamps
::
::    produces list of @da result
::
/+  ethio, strandio
=,  ethereum-types
=,  able:jael
::
|=  args=vase
=+  !<([url=@t blocks=(list @ud)] args)
=/  m  (strand:strandio ,vase)
=|  out=(list [block=@ud timestamp=@da])
|^  ^-  form:m
    =*  loop  $
    ?:  =(~ blocks)  (pure:m !>(out))  ::TODO  TMI
    ;<  res=(list [@t json])  bind:m
      (request-blocks (scag 100 blocks))
    %_  loop
      out     (weld out (parse-results res))
      blocks  (slag 100 blocks)
    ==
::
++  request-blocks
  |=  blocks=(list @ud)
  %+  request-batch-rpc-strict:ethio  url
  %+  turn  blocks
  |=  block=@ud
  ^-  [(unit @t) request:rpc:ethereum]
  :-  `(scot %ud block)
  [%eth-get-block-by-number block |]
::
++  parse-results
  |=  res=(list [@t json])
  ^+  out
  %+  turn  res
  |=  [id=@t =json]
  ^-  [@ud @da]
  :-  (slav %ud id)
  %-  from-unix:chrono:userlib
  %-  parse-hex-result:rpc:ethereum
  ~|  json
  ?>  ?=(%o -.json)
  (~(got by p.json) 'timestamp')
--
