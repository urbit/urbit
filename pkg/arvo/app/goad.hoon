/+  default-agent, verb
%+  verb  |
^-  agent:gall
=>
  |%
  ++  warp
    |=  =bowl:gall
    [%pass /clay %arvo %c %warp our.bowl %home ~ %next %z da+now.bowl /sys]
  ::
  ++  wait
    |=  =bowl:gall
    [%pass /behn %arvo %b %wait +(now.bowl)]
  ::
  ++  goad
    |=  force=?
    :~  [%pass /gall %arvo %g %goad force ~]
    ==
  --
::
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
++  on-init
  ::  subscribe to /sys and do initial goad
  ::
  [[(warp bowl) (wait bowl) ~] this]
::
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  ?:  ?=([%noun * %go] +<)
    [(goad |) this]
  ?:  ?=([%noun * %force] +<)
    [(goad &) this]
  (on-poke:def mark vase)
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ?+    wire  (on-arvo:def wire sign-arvo)
      [%clay ~]
    ::  on writ, wait
    ::
    ?>  ?=(%writ +<.sign-arvo)
    :_  this
    :~  (warp bowl)
        (wait bowl)
    ==
  ::
      [%behn ~]
    ::  on wake, goad
    ::
    ?>  ?=(%wake +<.sign-arvo)
    ?^  error.sign-arvo
      :_  this  :_  ~
      [%pass /dill %arvo %d %flog %crud %goad-fail u.error.sign-arvo]
    %-  (slog leaf+"goad: recompiling all apps" ~)
    [(goad &) this]
  ==
::
++  on-fail   on-fail:def
--
