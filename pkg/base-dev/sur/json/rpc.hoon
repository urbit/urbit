::  json-rpc: protocol types
::
|%
+$  batch-request
  $%  [%a p=(list request)]
      [%o p=request]
  ==
::
+$  request
  $:  id=@t
      jsonrpc=@t
      method=@t
      params=request-params
  ==
::
+$  request-params
  $%  [%list (list json)]
      [%map (map @t json)]
      [%object (list (pair @t json))]
  ==
+$  response
  $~  [%fail *httr:eyre]
  $%  [%result id=@t res=json]
      [%error id=@t code=@t message=@t]  ::TODO  data?
      [%fail hit=httr:eyre]
      [%batch bas=(list response)]
  ==
--
