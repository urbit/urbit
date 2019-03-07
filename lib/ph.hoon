::
::::  /hoon/ph/lib
  ::
/-  aquarium
=,  aquarium
|%
::  Defines a complete integration test.
::
++  raw-test-core
  $_  ^?
  |%
  ::
  ::  Unique name, used as a cache label.
  ::
  ++  label  *@ta
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
  ++  route  |~([now=@da ship unix-effect] *[? (quip ph-event _^?(..start)]))
  --
::
++  porcelain-test-core
  $_  ^?
  |%
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
++  porcelain-test
  |=  [label=@ta porcelain=porcelain-test-core]
  ^-  raw-test-core
  |%
  ++  label  ^label
  ++  ships  ~
  ++  start
    |=  now=@da
    =^  events  porcelain  (start:porcelain now)
    [events ..start]
  ::
  ++  route
    |=  args=[@da ship unix-effect]
    =^  events  porcelain  (route:porcelain args)
    [& events ..start]
  --
::
++  send-events-to
  |=  [who=ship what=(list unix-event)]
  ^-  (list ph-event)
  %+  turn  what
  |=  ue=unix-event
  [%event who ue]
::
++  init
  |=  [who=ship keys=(unit dawn-event)]
  ^-  (list ph-event)
  [%init-ship who keys]~
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
  |=  [who=ship des=desk pax=path txt=@t]
  ^-  (list ph-event)
  ?>  ?=([@ @ @ *] pax)
  =/  file  [/text/plain (as-octs:mimes:html txt)]
  %+  send-events-to  who
  :~
    [//sync/0v1n.2m9vh %into des | [t.t.t.pax `file]~]
  ==
::
++  on-dojo-output
  |=  [who=ship her=ship uf=unix-effect what=tape fun=$-($~ (list ph-event))]
  ^-  (list ph-event)
  ?.  =(who her)
    ~
  ?.  ?=(%blit -.q.uf)
    ~
  ?.  %+  lien  p.q.uf
      |=  =blit:dill
      ?.  ?=(%lin -.blit)
        |
      !=(~ (find what p.blit))
    ~
  (fun)
::
++  expect-dojo-output
  |=  [who=ship her=ship uf=unix-effect what=tape]
  ^-  (list ph-event)
  %-  on-dojo-output
  :^  who  her  uf
  :-  what
  |=  ~
  [%test-done &]~
::
++  on-ergo
  |=   [who=ship her=ship uf=unix-effect fun=$-($~ (list ph-event))]
  ?.  =(who her)
    ~
  ?.  ?=(%ergo -.q.uf)
    ~
  (fun)
::
++  azimuth
  |%
  ++  dawn
    |=  who=ship
    ^-  dawn-event
    :*  (need (private-key who))
        (^sein:title who)
        czar
        ~[~['arvo' 'netw' 'ork']]
        0
        `(need (de-purl:html 'http://localhost:8545'))
        ~
    ==
  ::
  ++  czar
    ^-  (map ship [life pass])
    %-  my
    ^-  (list (pair ship [life pass]))
    %+  murn  (gulf 0x0 0xff)
    |=  her=ship
    ^-  (unit [ship life pass])
    =/  pub  (public-key her)
    ?~  pub
      ~
    `[her u.pub]
  ::
  ++  private-key
    |=  who=ship
    =-  (~(get by -) who)
    ^-  (map ship seed:able:jael)
    %-  my
    :~  [~bud ~bud 1 'BbudB' ~]
        [~dev ~dev 1 'Bdev' ~]
    ==
  ::
  ++  public-key
    |=  who=ship
    ^-  (unit [life pass])
    =/  priv  (private-key who)
    ?~  priv
      ~
    =/  cub  (nol:nu:crub:crypto key.u.priv)
    `[lyf.u.priv pub:ex:cub]
  --
::
++  test-lib
  |_  our=ship
  ++  compose-tests
    |=  [a=raw-test-core b=raw-test-core]
    ^-  raw-test-core
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
      |=  [now=@da who=ship uf=unix-effect]
      ^-  [? (quip ph-event _..start)]
      ?:  done-with-a
        =+  ^-  [thru=? events=(list ph-event) cor=raw-test-core]
            (route:b now who uf)
        =.  b  cor
        [thru events ..start]
      =+  ^-  [thru=? events=(list ph-event) cor=raw-test-core]
          (route:a now who uf)
      =.  a  cor
      =^  events  ..filter-a  (filter-a now events)
      [thru events ..start]
    --
  ::
  ::  Don't use directly, or else you might not have a parent.
  ::
  ::    Consider ++galaxy, ++star, ++planet, and ++ship-with-ancestors.
  ::
  ++  raw-ship
    |=  [her=ship keys=(unit dawn-event)]
    ^-  raw-test-core
    |%
    ++  label  :((cury cat 3) 'init-' (scot %p her) '-' (scot %uw (mug (fall keys *dawn-event))))
    ++  ships  ~[her]
    ++  start
      |=  now=@da
      ^-  (quip ph-event _..start)
      [(init her keys) ..start]
    ::
    ++  route
      |=  [now=@da who=ship uf=unix-effect]
      ^-  [? (quip ph-event _..start)]
      :-  &
      :_  ..start
      %-  zing
      ::  This is a pretty bad heuristic, but in general galaxies will
      ::  hit the first of these cases, and other ships will hit the
      ::  second.
      ::
      :~
        %-  on-dojo-output
        :^  her  who  uf
        :-  "+ /{(scow %p her)}/base/2/web/testing/udon"
        |=  ~
        [%test-done &]~
      ::
        %-  on-dojo-output
        :^  her  who  uf
        :-  "is your neighbor"
        |=  ~
        [%test-done &]~
      ==
    --
  ::
  ++  galaxy
    |=  her=ship
    ?>  =(%czar (clan:title her))
    (raw-ship her ~)
  ::
  ++  star
    |=  her=ship
    ?>  =(%king (clan:title her))
    %+  compose-tests  (galaxy (^sein:title her))
    (raw-ship her ~)
  ::
  ++  planet
    |=  her=ship
    ?>  =(%duke (clan:title her))
    %+  compose-tests  (star (^sein:title her))
    (raw-ship her ~)
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
    |=  [her=ship des=desk]
    %+  porcelain-test
      (cat 3 'touch-file-' (scot %p her))
    =|  warped=@t
    |%
    ++  start
      |=  now=@da
      ^-  (pair (list ph-event) _..start)
      =/  pax
        /(scot %p our)/home/(scot %da now)/sur/aquarium/hoon
      =.  warped  (cat 3 '=>  .  ' .^(@t %cx pax))
      :_  ..start
      %-  zing
      :~  (dojo her "|mount /={(trip des)}=")
          (insert-file her des pax warped)
      ==
    ::
    ++  route
      |=  [now=@da who=ship uf=unix-effect]
      ^-  (quip ph-event _..start)
      :_  ..start
      %-  zing
      :~  %-  on-ergo
          :^  her  who  uf
          |=  $~
          =/  pax  /i/(scot %p her)/[des]/(scot %da now)/sur/aquarium/hoon/noun
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
    |=  [her=ship des=desk]
    %+  porcelain-test
      (cat 3 'check-file-touched-' (scot %p her))
    |%
    ++  start
      |=  now=@da
      ::  mounting is not strictly necessary since we check via scry,
      ::  but this way we don't have to check on every event, just
      ::  ergos (and dojo because we can't guarantee an ergo if the desk
      ::  is already mounted)
      ::
      ~&  %mounting
      [(dojo her "|mount /={(trip des)}=") ..start]
    ::
    ++  route
      |=  [now=@da who=ship uf=unix-effect]
      ^-  (quip ph-event _..start)
      =/  cb
        |=  $~
        ~&  %cbing
        =/  pax  /home/(scot %da now)/sur/aquarium/hoon
        =/  warped  (cat 3 '=>  .  ' .^(@t %cx (weld /(scot %p our) pax)))
        =/  aqua-pax
          ;:  weld
              /i/(scot %p her)
              pax(- des)
              /noun
          ==
        ?:  =(warped (need (scry-aqua (unit @) now aqua-pax)))
          [%test-done &]~
        ~&  %not-done-yet
        ~
      :_  ..start
      %-  zing
      :~  (on-ergo her who uf cb)
          (on-dojo-output her who uf ">=" cb)
      ==
    --
  ::
  ::  Reload vane from filesystem
  ::
  ::    Ship must have been started.
  ::
  ++  reload-vane
    |=  [her=ship vane=term]
    %+  porcelain-test
      :((cury cat 3) 'reload-vane-' (scot %p her) '-' vane)
    |%
    ++  start
      |=  now=@da
      ^-  (pair (list ph-event) _..start)
      =/  pax
        /(scot %p our)/home/(scot %da now)/sys/vane/[vane]/hoon
      :_  ..start
      %-  zing
      :~  (dojo her "|mount /=home=")
          (insert-file her %home pax .^(@t %cx pax))
          [%test-done &]~
      ==
    ::
    ++  route
      |=  [now=@da who=ship uf=unix-effect]
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
