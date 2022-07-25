::  eth/get-tx-receipts
::
::    asks an ethereum node for transaction receipts from a list of transaction
::    hashes. returns a (list [@t json]), where @t is the transaction hash in
::    hex written as a cord, and json is the receipt
::
/-  ethdata=eth-provider
/+  ethereum, *strandio, eth-provider
=,  jael
::
|=  args=vase
=+  !<([url=@t tx-hashes=(list @ux)] args)
=/  m  (strand ,vase)
=|  out=(list [@t json])
|^
^-  form:m
=*  loop  $
?:  =(~ tx-hashes)  (pure:m !>(out))
;<  res2=ethout:ethdata  bind:m
  (request-receipts url (scag 100 tx-hashes))
?>  ?=(%request-batch-rpc-strict -.res2)
=/  res  +.res2
%_  loop
  out        (welp out res)
  tx-hashes  (slag 100 tx-hashes)
==
::
++  request-receipts
  |=  [url=@t tx-hashes=(list @ux)]
  %-  eth-provider  
  :-  %request-batch-rpc-strict
  %+  turn  tx-hashes
  |=  txh=@ux
  ^-  [(unit @t) request:rpc:ethereum]
  :-  `(crip '0' 'x' ((x-co:co 64) txh))
  [%eth-get-transaction-receipt txh]
--
