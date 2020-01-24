::  Utility functions for constructing tests
::
/-  aquarium
=,  aquarium
|%
::
::  Turn [ship (list unix-event)] into (list ph-event)
::
++  send-events-to
  |=  [who=ship what=(list unix-event)]
  ^-  (list aqua-event)
  %+  turn  what
  |=  ue=unix-event
  [%event who ue]
::
::  Start a ship (low-level; prefer +raw-ship)
::
++  init
  |=  [who=ship keys=(unit dawn-event:able:jael)]
  ^-  (list aqua-event)
  [%init-ship who keys]~
::
::  Send dojo command
::
++  dojo
  |=  [who=ship what=tape]
  ^-  (list aqua-event)
  %+  send-events-to  who
  ^-  (list unix-event)
  :~
    [//term/1 %belt %ctl `@c`%e]
    [//term/1 %belt %ctl `@c`%u]
    [//term/1 %belt %txt ((list @c) what)]
    [//term/1 %belt %ret ~]
  ==
::
::  Control character
::
++  ctrl
  |=  [who=ship what=term]
  ^-  (list ph-event)
  %+  send-events-to  who
  :~  [//term/1 %belt %ctl (,@c what)]
  ==
::
::  Inject a file into a ship
::
++  insert-file
  |=  [who=ship des=desk pax=path txt=@t]
  ^-  (list aqua-event)
  ?>  ?=([@ @ @ *] pax)
  =/  file  [/text/plain (as-octs:mimes:html txt)]
  %+  send-events-to  who
  :~
    [//sync/0v1n.2m9vh %into des | [t.t.t.pax `file]~]
  ==
::
::  Checks whether the given event is a dojo output blit containing the
::  given tape
::
++  is-dojo-output
  |=  [who=ship her=ship uf=unix-effect what=tape]
  ?&  =(who her)
      ?=(%blit -.q.uf)
    ::
      %+  lien  p.q.uf
      |=  =blit:dill
      ?.  ?=(%lin -.blit)
        |
      !=(~ (find what p.blit))
  ==
::
::  Test is successful if +is-dojo-output
::
++  expect-dojo-output
  |=  [who=ship her=ship uf=unix-effect what=tape]
  ^-  (list ph-event)
  ?.  (is-dojo-output who her uf what)
    ~
  [%test-done &]~
::
::  Check whether the given event is an ergo
::
++  is-ergo
  |=  [who=ship her=ship uf=unix-effect]
  ?&  =(who her)
      ?=(%ergo -.q.uf)
  ==
::
::  Check if given effect is an http request; extract
::
++  extract-request
  |=  [uf=unix-effect dest=@t]
  ^-  (unit [num=@ud =request:http])
  ?.  ?=(%request -.q.uf)  ~
  ?.  =(dest url.request.q.uf)  ~
  `[id.q.uf request.q.uf]
::
::  Scry into a running aqua ship
::
++  scry-aqua
  |*  [a=mold our=@p now=@da pax=path]
  .^  a
      %gx
      (scot %p our)
      %aqua
      (scot %da now)
      pax
  ==
--
