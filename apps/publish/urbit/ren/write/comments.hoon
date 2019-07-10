/-  write
/+  write
/=  result
  /^  (list comment:write)
  /;  
  |=  comments=(map knot comment:write)
  ^-  (list [comment-info:write @t])
  %+  sort  ~(val by comments)
  |=  [a=comment:write b=comment:write]
  ^-  ?
  (lte date-created.info.a date-created.info.b)
::
  /_  /write-comment/
result
