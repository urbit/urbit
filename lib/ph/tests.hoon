::  Useful tests for testing things
::
/+  ph, ph-util
=,  ph
=,  ph-util
|=  our=ship
=>  ::  Helper functions, not tests
    ::
    |%
    ::  Scry into a running aqua ship
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
    ::
    --
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
::  Test to produce events unconditionally.
::
++  just-events
  |=  events=(list ph-event)
  =/  m  (ph ,~)
  ^-  form:m
  |=  ph-input
  [& events %done ~]
::
::  Boot ship; don't check it succeeded.
::
++  boot-ship
  |=  [her=ship keys=(unit dawn-event)]
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
    "+ /{(scow %p her)}/base/2/web/testing/udon"
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
  ^-  form:m
  ;<  ~  bind:m
    ^-  form:m
    |=  ph-input
    [& (dojo from "|hi {(scow %p to)}") %done ~]
  ^-  form:m
  |=  input=ph-input
  ^-  output:m
  :+  &  ~
  ?.  (is-dojo-output from who.input uf.input "hi {(scow %p to)} successful")
    [%wait ~]
  [%done ~]
::
::  Boot a ship and verify it booted.  Parent must already be booted.
::
++  raw-ship
  |=  [her=ship keys=(unit dawn-event)]
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
  =/  warped  (cat 3 '=>  .  ' .^(@t %cx host-pax))
  [& (insert-file her des host-pax warped) %done warped]
::
::  Check /sur/aquarium/hoon on the given has the given contents.
::
++  check-file-touched
  |=  [her=ship des=desk warped=@t]
  =/  m  (ph ,~)
  ^-  form:m
  |=  pin=ph-input
  ?.  &(=(her who.pin) ?=(?(%init %ergo) -.q.uf.pin))
    [& ~ %wait ~]
  =/  pax  /home/(scot %da now.pin)/sur/aquarium/hoon
  =/  aqua-pax
    ;:  weld
        /i/(scot %p her)
        pax(- des)
        /noun
    ==
  ?:  =(warped (need (scry-aqua (unit @) now.pin aqua-pax)))
    [& ~ %done ~]
  [& ~ %wait ~]
--
