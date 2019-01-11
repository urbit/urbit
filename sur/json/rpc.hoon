|%
++  response  ::TODO  id should be optional
  $%  [%result id=@t res=json]
      [%error id=@t code=@t message=@t]  ::TODO  data?
      [%fail hit=httr:eyre]
      [%batch bas=(list response)]
  ==
--
