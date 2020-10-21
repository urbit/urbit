/+  verb, dbug, theater
|%
++  poke
  $%  [%sub =opus-id:theater]
      [%del =opus-id:theater]
      [%print ~]
  ==
--
::
::  =/  hide  ~(. (default:theater *agent:gall %|) *bowl:gall *opera:theater)
::  =/  hider  (on-leave:hide /)
%+  verb  |
%-  agent:dbug
^-  agent:gall
%-  run:theater
^-  subber:theater
|_  [=bowl:gall =opera:theater]
+*  this  .
    def  ~(. (default:theater this %|) bowl opera)
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  ^-  [(list card:agent:gall) (set opus-id:theater) _this]
  :-  ~  :_  this
  =+  !<(=poke vase)
  ?-    -.poke
      %sub  (~(put in ~(key by opera)) opus-id.poke)
      %del  (~(del in ~(key by opera)) opus-id.poke)
      %print
    =/  hide
      %-  ~(urn by opera)
      |=  [=opus-id:theater vase=(unit ^vase)]
      (slog >opus-id< ?~(vase >~< (sell u.vase)) ~)
    ~(key by opera)
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  [(list card:agent:gall) (set opus-id:theater) _this]
  `~(key by opera)^this
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
