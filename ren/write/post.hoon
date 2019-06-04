/-  write
/+  write, cram, elem-to-react-json
/=  args  /$  ,[beam *]
/=  result
  /^  [post-info:write manx]
  /;  
  |=  $:  post-front=(map knot cord)
          post-content=manx
          ~
      ==
      :-  (front-to-post-info:write post-front)
      post-content
::
  /.  /&front&/udon/
      /&elem&/udon/
  ==
result
