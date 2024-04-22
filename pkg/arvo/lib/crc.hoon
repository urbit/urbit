~%  %crc  ..part  ~
|%
++  crc32
  ~/  %crc32
  |=  file=octs
  (update-crc 0x0 file)
++  make-crc-table
  ^-  (list @ux)
  %+  turn  (gulf 0 255)  |=  a=@
    %+  roll  (gulf 0 7)  |:  [b=1 c=a]
      ?:  (gth (dis c 1) 0)
        (mix 0xedb8.8320 (rsh [0 1] c))
      (rsh [0 1] c)
++  update-crc
  |=  [crc=@ux file=octs]
  ^-  @ux
  ?:  =(q.file 0x0)
    0x0
  =/  c  (mix crc 0xffff.ffff)
  =/  n  0

  =/  crc-table  make-crc-table
  =/  input-list  (rip 3 q.file)
  =/  leading-zeros  (sub p.file (met 3 q.file))
  =?  input-list  (gth leading-zeros 0)
    =/  i  0
    |-
    ?:  =(i leading-zeros)
      input-list
    $(input-list [0x0 input-list], i +(i))
  |-
  ?:  =(n (lent input-list))
    (mix c 0xffff.ffff)
  =/  index  (dis (mix c (snag n input-list)) 0xff)
  $(c (mix (snag index crc-table) (rsh [0 8] c)), n +(n))
--
