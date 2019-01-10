|%
++  raw-response
  $~  [%fail *httr:eyre]
  $%  [%result id=@t res=@t]
      [%error id=@t code=@t message=@t]  ::TODO  data?
      [%fail hit=httr:eyre]
      [%batch bas=(list response)]
  ==
::
++  response  ::TODO  id should be optional
  $~  [%fail *httr:eyre]
  $%  [%result id=@t res=json]
      [%error id=@t code=@t message=@t]  ::TODO  data?
      [%fail hit=httr:eyre]
      [%batch bas=(list response)]
  ==
--
