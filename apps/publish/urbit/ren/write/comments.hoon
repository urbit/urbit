/-  write
/+  write, cram, elem-to-react-json
/=  args  /$  ,[beam *]
/=  result
  /^  (list [comment-info:write @t])
  /;  
  |=  $=  comments
      %+  map  knot
      $:  comment-front=(map knot cord)
          comment-content=wain
          ~
      ==
      ^-  (list [comment-info:write @t])
::      XX  sort this list
      %+  turn  ~(tap by comments)
      |=  [fil=knot front=(map knot cord) content=wain ~]
      ^-  [comment-info:write @t]
      :-  (front-to-comment-info:write front) 
      (of-wain:format (slag 8 content))
::
  /_  
  /.  /&front&/udon/
      /&txt&/udon/
  ==
result
