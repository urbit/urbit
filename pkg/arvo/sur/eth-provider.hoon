/-  rpc=json-rpc
/+  ethereum
=,  ethereum-types
=,  jael
|%
+$  url  @ta
+$  active  ?(%local %provider %client)
+$  local  =url
+$  provider  [=url kids=? clients=(set @p)]
+$  client  provider=@p
+$  provider-mode
  $%
  [%local =local]
  [%provider =provider]
  [%client =client]
  ==
+$  action
  $%  [%configure =provider-mode]
      [%provide rid=@ta reqs=(list [id=(unit @t) req=request:rpc:ethereum])] :: request-rpc
  ==
+$  ethin
  $%  
      [%request-rpc id=(unit @t) req=request:rpc:ethereum]
      $:
      %request-batch-rpc-strict  
      reqs=(list [id=(unit @t) req=request:rpc:ethereum])
      ==
      $:
      %request-batch-rpc-loose 
      reqs=(list [id=(unit @t) req=request:rpc:ethereum])
      ==
      [%read-contract req=proto-read-request:rpc:ethereum]
      [%batch-read-contract-strict reqs=(list proto-read-request:rpc:ethereum)]
      [%get-latest-block ?]  :: ? not really used
      [%get-block-by-number =number:block]
      [%get-tx-by-hash tx-hash=@ux]
      [%get-logs-by-hash =hash:block contracts=(list address) =topics]
    ::
      $:  %get-logs-by-range 
          contracts=(list address) 
          =topics 
          =from=number:block 
          =to=number:block
      ==
    ::
      [%get-next-nonce =address]
      [%get-balance =address]
  ==
+$  id-response  [id=(unit @t) res=response:rpc:ethereum]
+$  id-dirty-response  [id=(unit @t) res=dirty-response:rpc:ethereum]
::  TODO each type (each )
+$  ethout  [error=(unit [code=@t message=@t]) (list id-dirty-response)]
--
