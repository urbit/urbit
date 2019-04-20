::  Utility functions for constructing tests
::
/+  ph
=,  ph
|%
::
::  Turn [ship (list unix-event)] into (list ph-event)
::
++  send-events-to
  |=  [who=ship what=(list unix-event)]
  ^-  (list ph-event)
  %+  turn  what
  |=  ue=unix-event
  [%event who ue]
::
::  Start a ship (low-level; prefer +raw-ship)
::
++  init
  |=  [who=ship keys=(unit dawn-event)]
  ^-  (list ph-event)
  [%init-ship who keys]~
::
::  Send dojo command
::
++  dojo
  |=  [who=ship what=tape]
  ^-  (list ph-event)
  %+  send-events-to  who
  ^-  (list unix-event)
  :~
    [//term/1 %belt %ctl `@c`%e]
    [//term/1 %belt %ctl `@c`%u]
    [//term/1 %belt %txt ((list @c) what)]
    [//term/1 %belt %ret ~]
  ==
::
::  Inject a file into a ship
::
++  insert-file
  |=  [who=ship des=desk pax=path txt=@t]
  ^-  (list ph-event)
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
++  extract-thus-to
  |=  [uf=unix-effect dest=@t]
  ^-  (unit [num=@ud mot=moth:eyre])
  ?.  ?=(%thus -.q.uf)  ~
  ?~  q.q.uf  ~
  ?.  =(p.u.q.q.uf (rash dest auri:de-purl:html))  ~
  `[p.q.uf q.u.q.q.uf]
--
