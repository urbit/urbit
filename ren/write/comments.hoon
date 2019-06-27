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
      %+  sort
        %+  turn  ~(tap by comments)
        |=  [fil=knot front=(map knot cord) content=wain ~]
        ^-  [comment-info:write @t]
        :-  (front-to-comment-info:write front) 
        (of-wain:format (slag 8 content))
      |=  [a=[com=comment-info:write @t] b=[com=comment-info:write @t]]
      (lte date-created.com.a date-created.com.b)
::
  /_  
  /.  /&front&/udon/
      /&txt&/udon/
  ==
result
