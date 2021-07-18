::  pull-hook-virt: virtualisation for pull-hook
/-  *resource
|_  =bowl:gall
++  mule-scry
  |=  [ref=* raw=*]
  =/  pax=(unit path)
    ((soft path) raw)
  ?~  pax  ~
  ?.  ?=([@ @ @ @ *] u.pax)  ~
  =/  ship
    (slaw %p i.t.u.pax)
  =/  ved
    (slay i.t.t.t.u.pax)
  =/  dat
    ?~  ved  now.bowl
    =/  cas=(unit case)
      ((soft case) p.u.ved)
    ?~  cas  now.bowl
    ?:  ?=(%da -.u.cas)
      p.u.cas
    now.bowl
  ::  catch bad gall scries early
  ?:  ?&  =((end 3 i.u.pax) %g)
          ?|  !=(`our.bowl ship)
              !=(dat now.bowl)
          ==
      ==
    ~
  ``.^(* u.pax)
::
++  kick-mule
  |=  [rid=resource trp=(trap *)]
  ^-  (unit (unit path))
  =/  res=toon
    (mock [trp %9 2 %0 1] mule-scry)
  =/  pax=(unit path)
    !<  (unit path) 
    :-  -:!>(*(unit path)) 
    ?:(?=(%0 -.res) p.res ~)
  ?:  !?=(%0 -.res)
    =/  =tang
      :+  leaf+"failed kick handler, please report" 
        leaf+"{<rid>} in {(trip dap.bowl)}"
      ?:  ?=(%2 -.res)
        p.res
      ?>  ?=(%1 -.res)
      =/  maybe-path=(unit path)  ((soft path) p.res)
      ?~  maybe-path  ~
      [(smyt u.maybe-path) ~]
    ((slog tang) ~)
  `pax
--
