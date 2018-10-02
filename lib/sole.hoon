::
::::  /hoon/sole/lib
  ::
/?    310
/-    sole
=,  sole
::
::::
  ::
|_  sole-share                                          ::  shared-state engine
++  abet  +<
++  apply
  |=  ted/sole-edit
  ^+  +>
  ?-    -.ted
    $del  +>.$(buf (weld (scag p.ted buf) (slag +(p.ted) buf)))
    $ins  +>.$(buf (weld (scag p.ted buf) `_buf`[q.ted (slag p.ted buf)]))
    $mor  |-  ^+  +>.^$
          ?~  p.ted
            +>.^$
          $(p.ted t.p.ted, +>.^$ ^$(ted i.p.ted))
    $nop  +>.$
    $set  +>.$(buf p.ted)
  ==
::
::::
::  ++transmute: symmetric operational transformation.
::
::  for any sole state +>, obeys
::
::      =+  [x=(transmute a b) y=(transmute b a)]
::      .=  (apply:(apply a) x)
::          (apply:(apply b) y)
::
++  transmute                                         ::  dex as after sin
  |=  {sin/sole-edit dex/sole-edit}
  ~|  [%transmute sin dex]
  ^-  sole-edit
  ?:  ?=($mor -.sin)
    |-  ^-  sole-edit
    ?~  p.sin  dex
    $(p.sin t.p.sin, dex ^$(sin i.p.sin))
  ::
  ?:  ?=($mor -.dex)
    :-  %mor
    |-  ^-  (list sole-edit)
    ?~  p.dex  ~
    [^$(dex i.p.dex) $(p.dex t.p.dex)]
  ::
  ?:  |(?=($nop -.sin) ?=($nop -.dex))  dex
  ?:  ?=($set -.sin)                    [%nop ~]
  ?:  ?=($set -.dex)                    dex
  ::
  ?-    -.sin
      $del
    ?-  -.dex
      $del  ?:  =(p.sin p.dex)  [%nop ~]
            ?:((lth p.sin p.dex) dex(p (dec p.dex)) dex)
      $ins  ?:((lth p.sin p.dex) dex(p (dec p.dex)) dex)
    ==
  ::
      $ins
    ?-  -.dex
      $del  ?:((lte p.sin p.dex) dex(p +(p.dex)) dex)
      $ins  ?:  =(p.sin p.dex)
              ?:((lth q.sin q.dex) dex dex(p +(p.dex)))
            ?:((lte p.sin p.dex) dex(p +(p.dex)) dex)
    ==
  ==
::
++  commit                                            ::  local change
  |=  ted/sole-edit
  ^-  sole-share
  abet:(apply(own.ven +(own.ven), leg [ted leg]) ted)
::
::::
::  ++inverse: inverse of change in context.
::
::  for any sole state +>, obeys
::
::      =(+> (apply:(apply a) (inverse a)))
::
++  inverse                                           ::  relative inverse
  |=  ted/sole-edit
  ^-  sole-edit
  =.  ted  ?.(?=({$mor * ~} ted) ted i.p.ted)
  ?-  -.ted
    $del  [%ins p.ted (snag p.ted buf)]
    $ins  [%del p.ted]
    $mor  :-  %mor
          %-  flop
          |-  ^-  (list sole-edit)
          ?~  p.ted  ~
          :-  ^$(ted i.p.ted)
          $(p.ted t.p.ted, +>.^$ (apply i.p.ted))
    $nop  [%nop ~]
    $set  [%set buf]
  ==
::
++  receive                                           ::  naturalize event
  |=  sole-change
  ^-  {sole-edit sole-share}
  ?.  &(=(his.ler his.ven) (lte own.ler own.ven))
    ~&  [%receive-sync his+[his.ler his.ven] own+[own.ler own.ven]]
    !!
  ?>  &(=(his.ler his.ven) (lte own.ler own.ven))
  ?>  |(!=(own.ler own.ven) =(`@`0 haw) =(haw (sham buf)))
  =.  leg  (scag (sub own.ven own.ler) leg)
  ::  ~?  !=(own.ler own.ven)  [%miss-leg leg]
  =+  dat=(transmute [%mor leg] ted)
  ::  ~?  !=(~ leg)  [%transmute from+ted to+dat ~]
  [dat abet:(apply(his.ven +(his.ven)) dat)]
::
++  remit                                             ::  conditional accept
  |=  {cal/sole-change ask/$-((list @c) ?)}
  ^-  {(unit sole-change) sole-share}
  =+  old=buf
  =^  dat  +>+<.$  (receive cal)
  ?:  (ask buf)
    [~ +>+<.$]
  =^  lic  +>+<.$  (transmit (inverse(buf old) dat))
  [`lic +>+<.$]
::
++  transmit                                          ::  outgoing change
  |=  ted/sole-edit
  ^-  {sole-change sole-share}
  [[[his.ven own.ven] (sham buf) ted] (commit ted)]
::
++  transceive                                        ::  receive and invert
  |=  sole-change
  ^-  {sole-edit sole-share}
  =+  old=buf
  =^  dat  +>+<.$  (receive +<.$)
  [(inverse(buf old) dat) +>+<.$]
::
++  transpose                                         ::  adjust position
  |=  pos/@ud
  =+  dat=(transmute [%mor leg] [%ins pos `@c`0])
  ?>  ?=($ins -.dat)
  p.dat
++  ask
  |%
  ++  output                                              ::  construct result
    |*  pro/*                                             ::
    [p=*(list tank) q=[%& p=[~ u=pro]]]                   ::
  ::                                                      ::
  ++  print                                               ::  add output tank
    |*  {tan/tank res/(sole-result)}                      ::
    ?@  res  res                                          ::
    [p=[i=tan t=p.res] q=q.res]                           ::
  ::                                                      ::
  ++  prompt                                              ::  construct prompt
    |*  {pom/sole-prompt mor/(sole-dialog)}               ::
    [p=*(list tank) q=[%| p=pom q=mor]]                   ::
  ::                                                      ::
  ++  curl                                                ::  fetch url
    =|  usr/knot                                          ::
    |*  {pul/_purl:eyre fun/$-(httr:eyre *)}              ::
    :-  p=*(list tank)                                    ::
    q=[%| p=`usr q=[pul %get ~ ~] r=fun]                  ::
  ::                                                      ::
  ++  no-result                                           ::  empty result
    [p=*(list tank) q=[%& ~]]                             ::
  ::                                                      ::
  ++  parse                                               ::  parse by rule
    |*  {sef/rule fun/$-(* *)}                            ::
    |=  txt/sole-input                                    ::
    =+  vex=(sef [0 0] txt)                               ::
    ?:  |(!=((lent txt) q.p.vex) ?=($~ q.vex))            ::
      q.p.vex                                             ::
    (fun p.u.q.vex)                                       ::
  --
--
