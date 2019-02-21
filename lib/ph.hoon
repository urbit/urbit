::
::::  /hoon/ph/lib
  ::
/-  aquarium
=,  aquarium
|%
::  Defines a complete integration test.
::
++  test-core
  $_  ^?
  |%
  ::
  ::  Unique name, used as a cache label.
  ::
  ++  label  *term
  ::
  ::  List of ships that are part of the test.
  ::
  ::    We'll only hear effects from these ships, and only these will
  ::    be in the cache points.
  ::
  ++  ships  *(list ship)
  ::
  ::  Called first to kick off the test.
  ::
  ++  start  |~(now=@da *(quip ph-event _^?(..start)))
  ::
  ::  Called on every effect from a ship.
  ::
  ++  route  |~([now=@da ship unix-effect] *(quip ph-event _^?(..start)))
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
  |=  [who=ship pax=path txt=@t]
  ^-  (list ph-event)
  ?>  ?=([@ @ @ *] pax)
  =/  file  [/text/plain (as-octs:mimes:html txt)]
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
++  on-ergo
  |=   [who=ship her=ship ovo=unix-effect fun=$-($~ (list ph-event))]
  ?.  =(who her)
    ~
  ?.  ?=(%ergo -.q.ovo)
    ~
  (fun)
::
++  test-lib
  |_  our=ship
  ++  compose-tests
    |=  [a=test-core b=test-core]
    ^-  test-core
    =/  done-with-a  |
    =>
      |%
      ++  filter-a
        |=  [now=@da events=(list ph-event)]
        ^-  (quip ph-event _..filter-a)
        =+  ^-  [done=(list ph-event) other-events=(list ph-event)]
          %+  skid  events
          |=  e=ph-event
          =(%test-done -.e)
        ?~  done
          [other-events ..filter-a]
        ?>  ?=(%test-done -.i.done)
        ?.  p.i.done
          [[%test-done |]~ ..filter-a]
        =.  done-with-a  &
        =/  snap-event  [%snap-ships label:a ships:a]
        =^  events-start  b  (start:b now)
        [(welp other-events [snap-event events-start]) ..filter-a]
      --
    |%
    ::
    ::  Cache lookup label
    ::
    ++  label  `@tas`:((cury cat 3) label:a '--' label:b)
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
        (scry-aqua ? now /fleet-snap/[label:a]/noun)
      ?:  have-cache
        ~&  [%caching-in label:a label]
        =.  done-with-a  &
        =/  restore-event  [%restore-snap label:a]
        =^  events-start  b  (start:b now)
        =^  events  ..filter-a  (filter-a now restore-event events-start)
        [events ..start]
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
      =^  events  ..filter-a  (filter-a now events)
      [events ..start]
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
    ++  label  (cat 3 'init-' (scot %p her))
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
  ::
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
  ::
  ::  Touches /sur/aquarium/hoon on the given ship.
  ::
  ::    Ship must have been started.
  ::
  ++  touch-file
    |=  her=ship
    ^-  test-core
    =|  warped=@t
    |%
    ++  label  (cat 3 'touch-file-' (scot %p her))
    ++  ships  ~
    ++  start
      |=  now=@da
      ^-  (pair (list ph-event) _..start)
      =/  pax
        /(scot %p our)/home/(scot %da now)/sur/aquarium/hoon
      =.  warped  (cat 3 '=>  .  ' .^(@t %cx pax))
      :_  ..start
      %-  zing
      :~  (dojo her "|mount %")
          (insert-file her pax warped)
      ==
    ::
    ++  route
      |=  [now=@da who=ship ovo=unix-effect]
      ^-  (quip ph-event _..start)
      :_  ..start
      %-  zing
      :~  %-  on-ergo
          :^  her  who  ovo
          |=  $~
          =/  pax  /i/[(scot %p her)]/home/(scot %da now)/sur/aquarium/hoon/noun
          ?:  =(warped (need (scry-aqua (unit @) now pax)))
            [%test-done &]~
          ~&  %not-done-yet
          ~
      ==
    --
  ::
  ::  Checks that /sur/aquarium/hoon has been touched, as by ++touch-file
  ::
  ::    Ship must have been started.
  ::
  ++  check-file-touched
    |=  her=ship
    ^-  test-core
    |%
    ++  label  (cat 3 'check-file-touched-' (scot %p her))
    ++  ships  ~
    ++  start
      |=  now=@da
      ::  mounting is not strictly necessary since we check via scry,
      ::  but this way we don't have to check on every event, just
      ::  ergos (and dojo because we can't guarantee an ergo if the desk
      ::  is already mounted)
      ::
      ~&  %mounting
      [(dojo her "|mount %") ..start]
    ::
    ++  route
      |=  [now=@da who=ship ovo=unix-effect]
      ^-  (quip ph-event _..start)
      =/  cb
        |=  $~
        ~&  %cbing
        =/  pax  /home/(scot %da now)/sur/aquarium/hoon
        =/  warped  (cat 3 '=>  .  ' .^(@t %cx (weld /(scot %p our) pax)))
        =/  aqua-pax  :(weld /i/(scot %p her) pax /noun)
        ?:  =(warped (need (scry-aqua (unit @) now aqua-pax)))
          [%test-done &]~
        ~&  %not-done-yet
        ~
      :_  ..start
      %-  zing
      :~  (on-ergo her who ovo cb)
          (on-dojo-output her who ovo ">=" cb)
      ==
    --
  ::
  ::  Reload vane from filesystem
  ::
  ::    Ship must have been started.
  ::
  ++  reload-vane
    |=  [her=ship vane=term]
    ^-  test-core
    |%
    ++  label  :((cury cat 3) 'reload-vane-' (scot %p her) '-' vane)
    ++  ships  ~
    ++  start
      |=  now=@da
      ^-  (pair (list ph-event) _..start)
      =/  pax
        /(scot %p our)/home/(scot %da now)/sys/vane/[vane]/hoon
      :_  ..start
      %-  zing
      :~  (dojo her "|mount %")
          (insert-file her pax .^(@t %cx pax))
          [%test-done &]~
      ==
    ::
    ++  route
      |=  [now=@da who=ship ovo=unix-effect]
      ^-  (quip ph-event _..start)
      `..start
    --
  ::
  ++  scry-aqua
    |*  [a=mold now=@da pax=path]
    .^  a
        %gx
        (scot %p our)
        %aqua
        (scot %da now)
        pax
    ==
  --
--
