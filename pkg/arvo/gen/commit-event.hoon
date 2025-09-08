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
[/c/sync %info desk=q.u.beam & [s.u.beam %ins %mime !>([/ (as-octs:mimes:html file)])]~]
