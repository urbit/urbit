::  eth/get-tx-receipts
::
::    asks an ethereum node for transaction receipts from a list of transaction
::    hashes. returns a (list [@t json]), where @t is the transaction hash in
::    hex written as a cord, and json is the receipt
::
/+  ethereum, ethio, *strandio
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
;<  res=(list [@t json])  bind:m
  (request-receipts url (scag 100 tx-hashes))
%_  loop
  out        (welp out res)
  tx-hashes  (slag 100 tx-hashes)
==
::
++  request-receipts
  |=  [url=@t tx-hashes=(list @ux)]
  %+  request-batch-rpc-strict:ethio  url
  %+  turn  tx-hashes
  |=  txh=@ux
  ^-  [(unit @t) request:rpc:ethereum]
  :-  `(crip '0' 'x' ((x-co:co 64) txh))
  [%eth-get-transaction-receipt txh]
--
