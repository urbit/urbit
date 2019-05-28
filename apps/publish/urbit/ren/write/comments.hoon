/-  write
/+  write, cram, elem-to-react-json
/=  args  /$  ,[beam *]
/=  result
  /^  (list [comment-info:write manx])
  /;  
  |=  $=  comments
      %+  map  knot
      $:  comment-front=(map knot cord)
          comment-content=manx
          ~
      ==
      ^-  (list [comment-info:write manx])
::      XX  sort this list
      %+  turn  ~(tap by comments)
      |=  [fil=knot front=(map knot cord) content=manx ~]
      ^-  [comment-info:write manx]
      [(front-to-comment-info:write front) content]
::
  /_  
  /.  /&front&/udon/
      /&elem&/udon/
  ==
result
