::  App  construction utilities
::
::::  /hoon/sh-utils/lib
  ::
|%
++  append
  |*  a=*
  |*  b=*
  [b a]
::
++  hapt  (pair ship path)
--
!:
::::
  ::
|%
++  args-done  |*(ref=_,[(list) ^] (add-exit (add-nice ref)))  ::  accept args
::
++  add-exit                          ::  add "kill self" kiss
  |*  ref=_,[(list) ^]
  %+  add-resp  [%pass / %g %cide %$]
  ref
::
++  add-nice                          ::  return "succcess" response
  |*  ref=_,[(list) ^]
  %+  add-resp  [%give %nice ~]
  ref
::
++  add-subs                          ::  add gall subscription
  |*  [hat=[hapt ship path] ref=_,[(list) ^]]
  (add-resp [%pass /show %g %show hat] ref)
::
++  args-into-gate                    ::  poke--args from gate: output and exit
  |*  [con=[* [hide *] *] gat=_,[@ *]]
  %-  args-done
  %^  gate-outs  con
    |=(a=$+<.gat [%rush (gat a)])
  ,_`con
::
++  args-into-resp                    ::  compute gifts with gate and exit
  |*  [con=[* [hide *] *] gat=_,(pole ,[@ *])]
  %-  args-done
  |*  [ost=bone * arg=$+<.gat]
  :_  con
  %.  (gat arg)
  |*  a=(pole ,[@ *])
  ?~  a  ~ 
  a(- [ost %give -.a], + $(a +.a))
::
++  add-output                        ::  send gift to /out
  |*  [con=[* [hide *] *] ote=[@ *] ref=_,[(list) ^]]
  =>  .(+<- `[* [hid=hide *] *]`con)
  =+  sus=(~(tap in `(set bone)`(~(get ju pus.hid) /out)))
  =+  mof=(turn sus (append [%give ote]))
  |=  $+<.ref
  =+  neu=(ref +<)
  neu(- (welp mof -.neu))
::
++  add-resp                          ::  add response move to requesting bone
  |*  [mof=[@ @ *] ref=_,[(list) ^]]
  |*  [ost=bone $?@(+<.ref ~ +<+.ref)]
  =+  neu=(ref +<)
  neu(- [[ost mof] -.neu])
::
++  gate-give                         ::  respond with computed gift
  |*  [gat=_,[@ *] ref=_,[(list) ^]]
  (gate-move |*($+<.gat [%give (gat +<)]) ref)
::
++  gate-bang                         ::  respond with computed note
  |*  [gat=_,[@ @ *] ref=_,[(list) ^]]
  (gate-move |*($+<.gat [%pass /bang (gat +<)]) ref)
::
++  gate-mess                         ::  respond with local message
  |*  [con=[* [hide *] *] gat=_,[@ @ *] ref=_,[(list) ^]]
  =>  .(+<- `[* [hid=hide *] *]`con)
  %-  gate-move  :_  ref
  |*  $+<.gat
  =+  `[imp=path mez=cage]`(gat +<)
  [%pass /poke %g %mess [our.hid imp] our.hid mez]
::
++  gate-move                         ::  respond with computed move
  |*  [gat=_,(mold) ref=_,[(list) ^]]
  |*  [ost=bone * arg=$+<.gat]
  ((add-resp (gat arg) ref) +<)
::
::
++  gate-outs                         ::  send computed gift to /out
  |*  [con=[* [hide *] *] gat=_,[@ *] ref=_,[(list) ^]]
  |*  [ost=bone * arg=$+<.gat]
  ((add-output con (gat arg) ref) +<)
::
++  listen-in                         ::  recieve standard input
  |*  [con=[* [hide *] *] ref=_,[(list) ^]]
  =>  .(+<- `[* [hid=hide *] *]`con)
  (add-subs [[our +.imp] our /in/[-.imp]]:hid ref)
::
++  print
  |*  [con=[* [hide *] *] tap=tape]
  (add-output con [%rush %tang [%leaf tap] ~] ,_[~ con])
::
++  verify                            ::  type-check with context and move
  |*  [con=^ mof=[@ *]]
  |*  ref=_,[(list ,_mof) _con]
  |*  $+<.ref
  ^-  [(list ,_mof) _con]
  (ref +<)
--
