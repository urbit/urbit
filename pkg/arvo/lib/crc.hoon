~%  %crc  ..part  ~
|%
++  crc32
  ~/  %crc32
  |=  data=octs
  ^-  @ux
  ?:  =(q.data 0x0)
    0x0
  =/  input-list  (weld (reap (sub p.data (met 3 q.data)) 0x0) (rip 3 q.data))
  %+  mix  0xffff.ffff
  %+  roll  input-list
  |:  [a=1 acc=0xffff.ffff]
  (mix (snag (dis (mix acc a) 0xff) crc32-table) (rsh [0 8] acc))
::
++  crc32-table
  ^-  (list @ux)
  %+  turn  (gulf 0 255)
  |=  i=@
  %+  roll  (gulf 0 7)
  |:  [a=1 acc=i]
  ?:  (gth (dis acc 1) 0)
    (mix 0xedb8.8320 (rsh [0 1] acc))
  (rsh [0 1] acc)
--
