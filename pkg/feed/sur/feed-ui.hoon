/-  post
|%
+$  update
  $%  [%list p=(list post:post)]
      [%add-post p=ship q=letter:post]
      [%feeds p=(set ship)]
  ==
::
+$  poke-status
  $%  [%ack p=(unit tang)]
  ==
--
