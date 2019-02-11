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
  ++  ships  *(list ship)
  ++  start  *(quip ph-event _^?(..start))
  ++  route  |~([ship unix-effect] *(quip ph-event _^?(..start)))
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
++  on-dojo-output
  |=  [who=ship her=ship ovo=unix-effect what=tape fun=$-($~ (list ph-event))]
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
  (fun)
::
++  expect-dojo-output
  |=  [who=ship her=ship ovo=unix-effect what=tape]
  ^-  (list ph-event)
  %-  on-dojo-output
  :^  who  her  ovo
  :-  what
  |=  ~
  [%test-done &]~
::
++  compose-tests
  |=  [a=test-core b=test-core]
  ^-  test-core
  =/  done-with-a  |
  |%
  ::  Union of ships in a and b
  ::
  ++  ships  ~(tap in (~(uni in (silt ships.a)) (silt ships.b)))
  ::
  ::  Start with start of a
  ::
  ++  start
    ^-  (quip ph-event _..start)
    =^  events  a  start:a
    [events ..start]
  ::
  ::  Keep going on a until it's done.  If success, go to b.
  ::
  ::    In theory, we should be able to just swap out the whole core
  ::    for b, but in practice the types are hard, and we generally
  ::    try to avoid changing the structure of a core in the middle
  ::    like that.
  ::
  ++  route
    |=  [who=ship ovo=unix-effect]
    ^-  (quip ph-event _..start)
    ?:  done-with-a
      =^  events  b  (route:b who ovo)
      [events ..start]
    =^  events  a  (route:a who ovo)
    =+  ^-  [done=(list ph-event) other-events=(list ph-event)]
      %+  skid  events
      |=  e=ph-event
      =(%test-done -.e)
    ?~  done
      [other-events ..start]
    ?>  ?=(%test-done -.i.done)
    ?.  p.i.done
      [[%test-done |]~ ..start]
    =.  done-with-a  &
    =^  events-start  b  start:b
    [(weld other-events events-start) ..start]
  --
::
++  head-starts
  |%
  ++  marbud
    ^-  test-core
    |%
    ++  ships  ~[~bud ~marbud]
    ++  start
      ^-  (quip ph-event _..start)
      :_  ..start
      %-  zing
      :~  (init ~bud)
      ==
    ::
    ++  route
      |=  [who=ship ovo=unix-effect]
      ^-  (quip ph-event _..start)
      :_  ..start
      %-  zing
      :~
        %-  on-dojo-output
        :^  ~bud  who  ovo
        :-  "+ /~bud/base/2/web/testing/udon"
        |=  ~
        (init ~marbud)
      ::
        %-  on-dojo-output
        :^  ~marbud  who  ovo
        :-  "; ~bud is your neighbor"
        |=  ~
        [%test-done &]~
      ==
    --
  --
--
