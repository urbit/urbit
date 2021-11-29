/-  post
|%
+$  uid  @uxH
+$  request  (pair uid action)
+$  response 
  %+  pair  uid
  $%  [%ack p=(unit tang)]
  ==
+$  action 
  $%  [%add-post p=ship q=letter:post]
      [%like gid:post]
  ==
      
+$  update
  $%  [%list p=(list post:post)]
      [%feeds p=(set ship)]
  ==
::
+$  poke-status
  $%  [%ack p=(unit tang)]
  ==
--
