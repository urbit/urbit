::  eth/get-timestamps: query ethereum block timestamps
::
::    produces list of @da result
::
/+  ethereum, eth-provider, strandio
=,  ethereum-types
=,  jael
::
|=  args=vase
=+  !<([url=@t blocks=(list @ud)] args)
=/  m  (strand:strandio ,vase)
=|  out=(list [block=@ud timestamp=@da])
|^  ^-  form:m
    =*  loop  $
    ?:  =(~ blocks)  (pure:m !>(out))  ::TODO  TMI
    ;<  res=(list [id=(unit @t) res=response:rpc:ethereum])  bind:m
      (request-blocks (scag 100 blocks))
    %_  loop
      out     (weld out (parse-results res))
      blocks  (slag 100 blocks)
    ==
::
++  request-blocks
  |=  blocks=(list @ud)
  %-  request-batch-rpc-strict:eth-provider
  %+  turn  blocks
  |=  block=@ud
  ^-  [(unit @t) request:rpc:ethereum]
  :-  `(scot %ud block)
  [%eth-get-block-by-number block |]
::
++  parse-results
  |=  res=(list [id=(unit @t) res=response:rpc:ethereum])
  ^+  out
  %+  turn  res
  |=  [id=(unit @t) resp=response:rpc:ethereum]
  ^-  [@ud @da]
  ?~  id  !!
  ?>  ?=([%block (unit block:rpc:ethereum)] resp)
  ?~  +.resp  !!
  =/  block  +>.resp
  ~&  [%resp timestamp.u.block]
  ?~  +.resp  !!
  :-  (slav %ud +.id)
  timestamp.u.block
--
