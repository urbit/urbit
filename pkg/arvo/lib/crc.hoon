::  ~%  %unzip  ..part  ~
|=  file=octs
=<
(crc32 file)
|%
++  make-crc-table
  ^-  (list @ux)
  =|  crc-table=(list @ux)
  =/  i  0
  =/  j  0
  |-
    =/  c  i
    ?:  =(i 256)
      (flop crc-table)
    =/  temp  c
    =.  c  |-
      ?:  =(j 8)
        temp
      =.  temp
        ?:  (gth (dis temp 1) 0)
          (mix 0xedb8.8320 (rsh [0 1] temp))
        (rsh [0 1] temp)
      $(j +(j))
    $(crc-table [`@ux`c crc-table], i +(i))
++  update-crc
  |=  [crc=@ux file=octs]
  ^-  @ux
  =/  c  (mix crc 0xffff.ffff)
  =/  n  0

  =/  crc-table  make-crc-table
  =/  leading-zeros  (sub p.file (met 3 q.file))
  =|  lz-list=(list @ux)
  =?  lz-list  (gth leading-zeros 0)
    =/  i  0
    |-
      ?:  =(i leading-zeros)
        lz-list
      $(lz-list [`@ux`0 lz-list], i +(i))
  =|  buf-list=(list @ux)
  =.  buf-list  (rip 3 q.file)
  =.  buf-list  (weld lz-list buf-list)
  ::  ~&  buf-list
  |-
    ?:  =(n (lent buf-list))
      (mix c 0xffff.ffff)
    =/  index  (dis (mix c (snag n buf-list)) 0xff)
    $(c (mix (snag index crc-table) (rsh [0 8] c)), n +(n))
++  crc32
  |=  file=octs
  ^-  @ux

  =/  crc  0x0
  ?:  =(q.file 0x0)
    0x0
  (update-crc crc file)
--
