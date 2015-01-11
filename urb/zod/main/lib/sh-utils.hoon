|%
++  append
  |*  a=*
  |*  b=*
  [b a]
::
++  args-into-gate
  |*  [con=[* [hide *] *] gat=_,[@ *]]
  |=  [ost=bone ship arg=_+<.gat]
  :_  con
  :*  [ost %pass / %g %cide %$]
      [ost %give %nice ~]
      =>  .(+<-.con `hid=hide`+<-.con)
      %-  turn  :_  (append [%give %rush (gat arg)])
      (~(tap in `(set bone)`(~(get ju pus.hid.con) /out)))
  ==
++  gate-bang
  |*  [gat=_,[@ @ *] ref=_,[(list) ^]]
  |=  [ost=bone ship arg=_+<.gat]
  =+  neu=(ref +<)
  neu(- [(gat arg) -.neu])
--
