::  Would love to see a proper stateful terminal handler.  Ideally,
::  you'd be able to ^X into the virtual ship, like the old ^W.
::
::  However, that's probably not the primary way of interacting with
::  it.  In practice, most of the time you'll be running from a file
::  (eg for automated testing) or fanning the same command to multiple
::  ships or otherwise making use of the fact that we can
::  programmatically send events.
::
/-  aquarium, spider
/+  aqua-vane-thread
|%
++  handle-blit
  |=  [who=@p way=wire %blit blits=(list blit:dill)]
  ^-  (list card:agent:gall)
  =/  last-line
    %+  roll  blits
    |=  [b=blit:dill line=tape]
    ?-    -.b
        %put  (tape p.b)
        %klr  (tape (zing (turn p.b tail)))
        %nel  ~&  "{<who>}: {line}"  ""
        %hop  line
        %bel  line
        %clr  ""
        %sag  ~&  [%save-jamfile-to p.b]  line
        %sav  ~&  [%save-file-to p.b]  line
        %url  ~&  [%activate-url p.b]  line
        %wyp  ""
    ==
  ~?  !=(~ last-line)  last-line
  ~
--
::
%+  aqua-vane-thread  ~[%blit]
|_  =bowl:spider
+*  this  .
++  handle-unix-effect
  |=  [who=@p ue=unix-effect:aquarium]
  ^-  (quip card:agent:gall _this)
  =/  cards
    ?+  -.q.ue  ~
      %blit  (handle-blit who ue)
    ==
  [cards this]
::
++  handle-arvo-response  |=(* !!)
--
