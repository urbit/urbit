~%  %crc  ..part  ~
|%
++  crc32
  ~/  %crc32
  |=  file=octs
  (update-crc 0x0 file)
++  make-crc-table
  ^-  (list @ux)
  %+  turn  (gulf 0 255)
  |=  i=@
  %+  roll  (gulf 0 7)
  |:  [b=1 acc=i]
  ?:  (gth (dis acc 1) 0)
    (mix 0xedb8.8320 (rsh [0 1] acc))
  (rsh [0 1] acc)
++  update-crc
  |=  [crc=@ux file=octs]
  ^-  @ux
  ?:  =(q.file 0x0)
    0x0
  =/  crc-table  make-crc-table
  =/  input-list  (weld (reap (sub p.file (met 3 q.file)) 0x0) (rip 3 q.file))
  =/  c  %+  roll  input-list
  |:  [a=1 acc=(mix crc 0xffff.ffff)]
  (mix (snag (dis (mix acc a) 0xff) crc-table) (rsh [0 8] acc))
  (mix c 0xffff.ffff)
--
