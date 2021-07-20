::  json-rpc: protocol types
::
|%
+$  request
  $:  id=@t
      method=@t
      params=request-params
  ==
::
+$  request-params
  $%  [%list (list json)]
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
