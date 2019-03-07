::  Would love to see a proper stateful terminal handler.  Ideally,
::  you'd be able to ^X into the virtual ship, like the old ^W.
::
::  However, that's probably not the primary way of interacting with
::  it.  In practice, most of the time you'll be running from a file
::  (eg for automated testing) or fanning the same command to multiple
::  ships or otherwise making use of the fact that we can
::  programmatically send events.
::
/-  aquarium
=,  aquarium
=>  $~  |%
    +$  move  (pair bone card)
    +$  card
      $%  [%poke wire dock %aqua-events (list aqua-event)]
          [%peer wire dock path]
          [%pull wire dock ~]
      ==
    ::
    +$  state
      $:  %0
          subscribed=_|
      ==
    --
=,  gall
=|  moves=(list move)
|_  $:  bowl
        state
    ==
++  this  .
++  apex  %_(this moves ~)
++  abet  [(flop moves) this]
++  emit-moves
  |=  ms=(list move)
  %_(this moves (weld ms moves))
::
++  emit-aqua-events
  |=  aes=(list aqua-event)
  %-  emit-moves
  [%poke /aqua-events [our %aqua] %aqua-events aes]~
::
++  poke-aqua-vane-control
  |=  command=?(%subscribe %unsubscribe)
  :_  this(subscribed =(command %subscribe)
  (aqua-vane-control-handler subscribed)
::
++  diff-aqua-effects
  |=  [way=wire afs=aqua-effects]
  ^-  (quip move _this)
  =.  this  apex  =<  abet
  |-  ^+  this
  ?~  ufs.afs
    this
  =.  this
    ?+  -.q.i.ufs.afs  this
      %blit     (handle-blit i.ufs.afs)
    --
  $(ufs.afs t.ufs.afs)
::
++  handle-blit
  |=  [way=wire %blit blits=(list blit:dill)]
  ^+  ..abet-pe
  =/  last-line
    %+  roll  blits
    |=  [b=blit:dill line=tape]
    ?-    -.b
        %lin  (tape p.b)
        %mor  ~&  "{<who>}: {line}"  ""
        %hop  line
        %bel  line
        %clr  ""
        %sag  ~&  [%save-jamfile-to p.b]  line
        %sav  ~&  [%save-file-to p.b]  line
        %url  ~&  [%activate-url p.b]  line
    ==
  ~&  last-line
  ..abet-pe
--
