::  graph-store|add-post: add post to a graph
::
/-  *graph-store
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [[[our=ship name=term] contents=(list content) ~] ~]
    ==
=/  =post  *post
=:  author.post     our
    index.post      [now]~
    time-sent.post  now
    contents.post   contents
==
::
:-  %graph-update-4
^-  update
:-  [our name]
:-  %add-nodes  
%-  ~(gas by *(map index node))
~[[[now]~ [[%& post] [%empty ~]]]]
