!:
|%
++  append
  |*  a=*
  |*  b=*
  [b a]
::
::
++  args-done  |*(ref=_,[(list) ^] (add-exit (add-nice ref)))
::
++  add-exit
  |*  ref=_,[(list) ^]
  %+  add-resp  [%pass / %g %cide %$]
  ref
::
++  add-nice
  |*  ref=_,[(list) ^]
  %+  add-resp  [%give %nice ~]
  ref
::
++  args-into-gate
  |*  [con=[* [hide *] *] gat=_,[@ *]]
  %-  args-done
  %^  gate-outs  con
    |=(a=_+<.gat [%rush (gat a)])
  ,_`con
::
++  args-into-resp
  |*  [con=[* [hide *] *] gat=_,(pole ,[@ *])]
  %-  args-done
  |*  [ost=bone * arg=_+<.gat]
  :_  con
  %.  (gat arg)
  |*  a=(pole ,[@ *])
  ?~  a  ~ 
  a(- [ost %give -.a], + $(a +.a))
::
++  add-output
  |*  [con=[* [hide *] *] ote=[@ *] ref=_,[(list) ^]]
  =>  .(+<-.con `hid=hide`+<-.con)
  =+  sus=(~(tap in `(set bone)`(~(get ju pus.hid.con) /out)))
  =+  mof=(turn sus (append [%give ote]))
  |=  _+<.ref
  =+  neu=(ref +<)
  neu(- (welp mof -.neu))
::
++  add-resp
  |*  [mof=[@ @ *] ref=_,[(list) ^]]
  |*  [ost=bone _?@(+<.ref ~ +<+.ref)]
  =+  neu=(ref +<)
  neu(- [[ost mof] -.neu])
::
++  gate-give
  |*  [gat=_,[@ @ *] ref=_,[(list) ^]]
  (gate-move |*(_+<.gat [%give (gat +<)]) ref)
::
++  gate-bang
  |*  [gat=_,[@ @ *] ref=_,[(list) ^]]
  (gate-move |*(_+<.gat [%pass /bang (gat +<)]) ref)
::
++  gate-move
  |*  [gat=_,_*(mold) ref=_,[(list) ^]]
  |*  [ost=bone * arg=_+<.gat]
  ((add-resp (gat arg) ref) +<)
::
++  gate-outs
  |*  [con=[* [hide *] *] gat=_,[@ *] ref=_,[(list) ^]]
  |*  [ost=bone * arg=_+<.gat]
  ((add-output con (gat arg) ref) +<)
::
++  verify
  |*  [con=^ mof=[@ *]]
  |*  ref=_,[(list ,_mof) _con]
  |*  _+<.ref
  ^-  [(list ,_mof) _con]
  (ref +<)
--
