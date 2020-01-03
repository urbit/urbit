/-  publish
/+  publish
/=  result
  /^  (list comment:publish)
  /;
  |=  comments=(map knot comment:publish)
  ^-  (list [comment-info:publish @t])
  %+  sort  ~(val by comments)
  |=  [a=comment:publish b=comment:publish]
  ^-  ?
  (gte date-created.info.a date-created.info.b)
::
  /_  /publish-comment/
result
