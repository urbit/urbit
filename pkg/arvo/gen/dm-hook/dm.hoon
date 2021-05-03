::  graph-store|add-post: add post to a graph
::
/-  *graph-store
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [[him=ship contents=(list content) ~] ~]
    ==
=*  our  p.beak
=/  =post  *post
=:  author.post     our
    index.post      ~[him now]
    time-sent.post  now
    contents.post   contents
==
::
:-  %graph-update-2
^-  update
:-  now
:+  %add-nodes  [our %inbox]
%-  ~(gas by *(map index node))
~[[~[him now] [%&^post [%empty ~]]]]
