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
+$  ethout  (list [id=(unit @t) dirty-response:rpc:ethereum])
:: +$  ethout
::   $%  
::       [%request-rpc res=json]
::       [%request-batch-rpc-strict res=(list [id=@t =json])]
::       [%request-batch-rpc-loose res=(list response:json-rpc)]
::       [%read-contract res=@t]
::       [%batch-read-contract-strict results=(list [id=@t res=@t])]
::       [%get-latest-block =block]
::       [%get-block-by-number =block]
::       [%get-tx-by-hash res=transaction-result:rpc:ethereum]
::       [%get-logs-by-hash res=(list event-log:rpc:ethereum)]
::       [%get-logs-by-range res=(list event-log:rpc:ethereum)]
::       [%get-next-nonce nonce=@ud]
::       [%get-balance balance=@ud]
::   ==
--
