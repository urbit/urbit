::  make a unix commit event
::
::    call as > .event/jam +commit-event /path/to/file
::    to be used with ./urbit-binary -I event.jam pier
::
::    XX expand with arbitrary user-defined events?
::    XX only supports files in which +noun:grab in the mark file returns a @t
::       (e.g. hoon files)
::
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] [=path ~] ~]
:-  %noun
?~  beam=(de-beam path)
  ~|(%path-not-beam !!)
=+  .^(file=@t %cx path)
=+  .^(=tube:clay %cc /(scot %p p.bec)/[q.bec]/(scot %da now)/txt/mime)
[/c/sync %into desk=q.u.beam | [s.u.beam [~ !<(mime (tube !>(~[file])))]]~]
