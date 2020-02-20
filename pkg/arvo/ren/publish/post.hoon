/-  publish
/+  publish, cram, elem-to-react-json
/=  args  /$  ,[beam *]
/=  result
  /^  [post-info:publish manx @t]
  /;
  |=  $:  post-front=(map knot cord)
          post-content=manx
          post-raw=wain
          ~
      ==
      :+  (front-to-post-info:publish post-front)
        post-content
      (of-wain:format (slag 11 post-raw))
::
  /.  /&front&/udon/
      /&elem&/udon/
      /&txt&/udon/
  ==
result
