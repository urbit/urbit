::  Useful tests for testing things
::
/+  ph, ph-util
=,  ph
=,  ph-util
|=  our=ship
::
::  Useful tests
::
|%
::
::  Never-ending test, for development.
::
++  stall
  |=  ph-input
  [& ~ %wait ~]
::
::  Stall until you run :aqua|dojo ~ship "%go" on any ship.
::
++  please-press-enter
  ^+  *form:(ph ,~)
  |=  pin=ph-input
  :+  &  ~
  ?:  (is-dojo-output who.pin who.pin uf.pin "%go")
    [%done ~]
  [%wait ~]
::
::  Test to produce events unconditionally.
::
++  just-events
  |=  events=(list ph-event)
  =/  m  (ph ,~)
  ^-  form:m
  |=  ph-input
  [& events %done ~]
::
::
::
++  wait-for-dojo
  |=  [her=@p what=tape]
  =/  m  (ph ,~)
  ^-  form:m
  |=  pin=ph-input
  :+  &  ~
  ?.  (is-dojo-output her who.pin uf.pin what)
    [%wait ~]
  [%done ~]
::
::  Boot ship; don't check it succeeded.
::
++  boot-ship
  |=  [her=ship keys=(unit dawn-event:able:jael)]
  ^+  *form:(ph ,~)
  |=  ph-input
  [& (init her keys) %done ~]
::
::  Wait until ship has finished booting.
::
++  check-ship-booted
  |=  her=ship
  ^+  *form:(ph ,~)
  |=  ph-input
  =;  done=?
    :+  &  ~
    ?:  done
      [%done ~]
    [%wait ~]
  ::  This is a pretty bad heuristic, but in general galaxies will
  ::  hit the first of these cases, and other ships will hit the
  ::  second.
  ::
  ?|
    %^  is-dojo-output  her  who  :-  uf
    "clay: committed initial filesystem (all)"
  ::
    %^  is-dojo-output  her  who  :-  uf
    "is your neighbor"
  ==
::
::  Send "|hi" from one ship to another
::
++  send-hi
  |=  [from=@p to=@p]
  =/  m  (ph ,~)
  ;<  ~  bind:m  (just-events (dojo from "|hi {(scow %p to)}"))
  (wait-for-dojo from "hi {(scow %p to)} successful")
::
::  Send "|hi" and wait for "not responding" message
::
++  send-hi-not-responding
  |=  [from=@p to=@p]
  =/  m  (ph ,~)
  ;<  ~  bind:m  (just-events (dojo from "|hi {(scow %p to)}"))
  (wait-for-dojo from "{(scow %p to)} not responding still trying")
::
::  Boot a ship and verify it booted.  Parent must already be booted.
::
++  raw-ship
  |=  [her=ship keys=(unit dawn-event:able:jael)]
  =/  m  (ph ,~)
  ^-  form:m
  ;<  ~  bind:m  (boot-ship her keys)
  (check-ship-booted her)
::
::  Boot a fake star and its parent.
::
++  star
  |=  her=ship
  =/  m  (ph ,~)
  ^-  form:m
  ;<  ~  bind:m  (raw-ship (^sein:title her) ~)
  (raw-ship her ~)
::
::  Boot a fake planet, its parent, and its grandparent.
::
++  planet
  |=  her=ship
  =/  m  (ph ,~)
  ^-  form:m
  ;<  ~  bind:m  (star (^sein:title her))
  (raw-ship her ~)
::
::  Mount a desk.
::
++  mount
  |=  [her=ship des=desk]
  =/  m  (ph ,~)
  ^-  form:m
  ;<  ~  bind:m  (just-events (dojo her "|mount /={(trip des)}="))
  |=  pin=ph-input
  ?:  (is-ergo her who.pin uf.pin)
    [& ~ %done ~]
  [& ~ %wait ~]
::
::  Modify /sur/aquarium/hoon on the given ship
::
++  touch-file
  |=  [her=ship des=desk]
  =/  m  (ph ,@t)
  ^-  form:m
  ;<  ~  bind:m  (mount her des)
  |=  pin=ph-input
  =/  host-pax
    /(scot %p our)/home/(scot %da now.pin)/sur/aquarium/hoon
  =/  pax  /sur/aquarium/hoon
  =/  aqua-pax
    ;:  weld
        /i/(scot %p her)/cx/(scot %p her)/[des]/(scot %da now.pin)
        pax
        /noun
    ==
  =/  warped
    %^  cat  3  '=>  .  '
    (need (scry-aqua (unit @) our now.pin aqua-pax))
  [& (insert-file her des host-pax warped) %done warped]
::
::  Check /sur/aquarium/hoon on the given has the given contents.
::
++  check-file-touched
  |=  [her=ship des=desk warped=@t]
  =/  m  (ph ,~)
  ;<  ~  bind:m  (mount her des)
  ^-  form:m
  |=  pin=ph-input
  ::  %ergo is no longer sufficient because .^ is pinned to beginning of
  ::  the event.  So we hope somebody sets a timer for something.
  ::
  ?.  &(=(her who.pin) ?=(?(%init %ergo %doze) -.q.uf.pin))
    [& ~ %wait ~]
  =/  pax  /sur/aquarium/hoon
  =/  aqua-pax
    ;:  weld
        /i/(scot %p her)/cx/(scot %p her)/[des]/(scot %da now.pin)
        pax
        /noun
    ==
  ?:  =(warped (need (scry-aqua (unit @) our now.pin aqua-pax)))
    [& ~ %done ~]
  [& ~ %wait ~]
--
