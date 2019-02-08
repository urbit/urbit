::
::::  /hoon/ph/lib
  ::
/-  aquarium
=,  aquarium
|%
::  Defines a complete integration test.
::
::    Perhaps route should take a unix-effect rather than a sign.
::    Similarly, perhaps ++abet should produce a list of
::    unix-events.  Also, perhaps we should support state.
::
::  Perhaps closer to this:
::  ++  test-core
::    $_  ^?
::    |%
::    ++  start  ^?(..abet)
::    ++  route  |~([wire unix-effect] ^?(..abet))
::    ++  abet   *(list unix-event)
::    --
::
++  test-core
  $_  ^?
  |%
  ++  start  *(trel (list ship) (list ph-event) _^?(..start))
  ++  route  |~([ship unix-effect] *(list ph-event))
  --
::
++  ph-event
  $%  [%test-done p=?]
      aqua-event
  ==
::
++  send-events-to
  |=  [who=ship what=(list unix-event)]
  ^-  (list ph-event)
  %+  turn  what
  |=  ovo=unix-event
  [%event who ovo]
::
++  init
  |=  who=ship
  ^-  (list ph-event)
  [%init-ship who]~
::
::  factor out send-events-to
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
++  insert-file
  |=  [who=ship pax=path]
  ^-  (list ph-event)
  ?>  ?=([@ @ @ *] pax)
  =/  file  [/text/plain (as-octs:mimes:html .^(@ %cx pax))]
  %+  send-events-to  who
  :~
    [//sync/0v1n.2m9vh %into i.t.pax | [t.t.t.pax `file]~]
  ==
::
++  expect-dojo-output
  |=  [who=ship her=ship ovo=unix-effect what=tape]
  ^-  (list ph-event)
  ?.  =(who her)
    ~
  ?.  ?=(%blit -.q.ovo)
    ~
  ?.  %+  lien  p.q.ovo
      |=  =blit:dill 
      ?.  ?=(%lin -.blit)
        |
      !=(~ (find what p.blit))
    ~
  [%test-done &]~
--
