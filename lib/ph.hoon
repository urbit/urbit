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
  ++  label  *term
  ++  ships  *(list ship)
  ++  start  |~(@da *(quip ph-event _^?(..start)))
  ++  route  |~([@da [ship unix-effect]] *(quip ph-event _^?(..start)))
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
++  test-lib
  |_  our=ship
  ++  compose-tests
    |=  [a=test-core b=test-core]
    ^-  test-core
    =/  done-with-a  |
    |%
    ::
    ::  Cache lookup label
    ::
    ++  label  :((cury cat 3) label:a '--1-' label:b)
    ::
    ::  Union of ships in a and b
    ::
    ++  ships  ~(tap in (~(uni in (silt ships.a)) (silt ships.b)))
    ::
    ::  Start with start of a
    ::
    ++  start
      |=  now=@da
      ^-  (quip ph-event _..start)
      =/  have-cache 
        .^  @f
            %gx
            (scot %p our)
            %aqua
            (scot %da now)
            /fleet-snap/[label:a]/noun
        ==
      ~&  [%have-cache label:a have-cache]
      ?:  have-cache
        =.  done-with-a  &
        =/  restore-event  [%restore-snap label:a]
        =^  events-start  b  (start:b now)
        [[restore-event events-start] ..start]
      =^  events  a  (start:a now)
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
      |=  [now=@da who=ship ovo=unix-effect]
      ^-  (quip ph-event _..start)
      ?:  done-with-a
        =^  events  b  (route:b now who ovo)
        [events ..start]
      =^  events  a  (route:a now who ovo)
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
      =/  snap-event  [%snap-ships label:a ships:a]
      =^  events-start  b  (start:b now)
      [(welp other-events [snap-event events-start]) ..start]
    --
  ::
  ::  Don't use directly, or else you might not have a parent.
  ::
  ::    Consider ++galaxy, ++star, ++planet, and ++ship-with-ancestors.
  ::
  ++  raw-ship
    |=  her=ship
    ^-  test-core
    |%
    ++  label  (cat 3 'iinit-' (scot %p her))
    ++  ships  ~[her]
    ++  start
      |=  now=@da
      ^-  (quip ph-event _..start)
      [(init her) ..start]
    ::
    ++  route
      |=  [now=@da who=ship ovo=unix-effect]
      ^-  (quip ph-event _..start)
      :_  ..start
      %-  zing
      ::  This is a pretty bad heuristic, but in general galaxies will
      ::  hit the first of these cases, and other ships will hit the
      ::  second.
      ::
      :~
        %-  on-dojo-output
        :^  her  who  ovo
        :-  "+ /{(scow %p her)}/base/2/web/testing/udon"
        |=  ~
        [%test-done &]~
      ::
        %-  on-dojo-output
        :^  her  who  ovo
        :-  "is your neighbor"
        |=  ~
        [%test-done &]~
      ==
    --
  ++  galaxy
    |=  her=ship
    ?>  =(%czar (clan:title her))
    (raw-ship her)
  ::
  ++  star
    |=  her=ship
    ?>  =(%king (clan:title her))
    %+  compose-tests  (galaxy (^sein:title her))
    (raw-ship her)
  ::
  ++  planet
    |=  her=ship
    ?>  =(%duke (clan:title her))
    %+  compose-tests  (star (^sein:title her))
    (raw-ship her)
  ::
  ++  ship-with-ancestors
    |=  her=ship
    %.  her
    ?-  (clan:title her)
      %czar  galaxy
      %king  star
      %duke  planet
      %earl  ~|(%moon-not-implemented !!)
      %pawn  ~|(%comet-not-implemented !!)
    ==
  --
--
