/+  *test
/=  behn-raw  /sys/vane/behn
=/  behn-gate  (behn-raw ~bus)
=/  scry  *roof
=*  move  move:behn-gate
::
|%
++  test-wake-no
  ^-  tang
  =/  wen  ~1111.1.1
  =/  arg  [~[/vere] [%wake ~]]
  -:(call ~ wen arg ~)
::
++  test-wake-no-wait
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/vere] [%born ~]]
  =/  a-out  ~
  =^  a  behn-gate  (call `%a wen a-arg a-out)
  ::
  =/  b-arg  [~[/vere] [%wake ~]]
  =/  b-out  ~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  (weld a b)
::
++  test-wake-no-born
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/foo] [%wait +(wen)]]
  =/  a-out  ~
  =^  a  behn-gate  (call `%a wen a-arg a-out)
  ::
  =/  b-arg  [~[/vere] [%wake ~]]
  =/  b-out  ~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  (weld a b)
::
++  test-wake
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/vere] [%born ~]]
  =/  a-out  ~
  =^  a  behn-gate
    (call ~ wen a-arg a-out)
  ::
  =/  b-arg  [~[/foo] [%wait +(wen)]]
  =/  b-out=(list move)  [~[/vere] [%give [%doze `+(wen)]]]~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  =/  c-arg  [~[/vere] [%wake ~]]
  =/  c-out=(list move)  [~[/foo] [%give [%wake ~]]]~
  =^  c  behn-gate  (call `%c +(wen) c-arg c-out)
  ::
  :(weld a b c)
::
++  test-born
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/foo] [%wait +(wen)]]
  =/  a-out  ~
  =^  a  behn-gate  (call `%a wen a-arg a-out)
  ::
  =/  b-arg  [~[/vere] [%born ~]]
  =/  b-out=(list move)  [~[/vere] [%give [%doze `+(wen)]]]~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  (weld a b)
::
++  test-many-ordered
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/vere] [%born ~]]
  =/  a-out  ~
  =^  a  behn-gate  (call `%a wen a-arg a-out)
  ::
  =/  b-arg  [~[/foo] [%wait +(wen)]]
  =/  b-out=(list move)  [~[/vere] [%give [%doze `+(wen)]]]~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  =/  c-arg  [~[/foo] [%wait (add 2 wen)]]
  =/  c-out  ~
  =^  c  behn-gate  (call `%c wen c-arg c-out)
  ::
  =/  d-arg  [~[/foo] [%wait (add 3 wen)]]
  =/  d-out  ~
  =^  d  behn-gate  (call `%d wen d-arg d-out)
  ::
  =/  e-arg  [~[/vere] [%wake ~]]
  =/  e-out=(list move)
    :~  [~[/vere] [%give [%doze `(add 2 wen)]]]
        [~[/foo] [%give [%wake ~]]]
    ==
  =^  e  behn-gate  (call `%e (add 4 wen) e-arg e-out)
  ::
  :(weld a b c d e)
::
++  test-many-ordered-lag
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/vere] [%born ~]]
  =/  a-out  ~
  =^  a  behn-gate  (call `%a wen a-arg a-out)
  ::
  =/  b-arg  [~[/foo] [%wait +(wen)]]
  =/  b-out=(list move)  [~[/vere] [%give [%doze `+(wen)]]]~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  =/  c-arg  [~[/foo] [%wait (add 2 wen)]]
  =/  c-out  ~
  =^  c  behn-gate  (call `%c wen c-arg c-out)
  ::
  =/  d-arg  [~[/foo] [%wait (add 3 wen)]]
  =/  d-out  ~
  =^  d  behn-gate  (call `%d wen d-arg d-out)
  ::
  =/  e-arg  [~[/vere] [%wake ~]]
  =/  e-out=(list move)
    :~  [~[/vere] [%give [%doze `(add 2 wen)]]]
        [~[/foo] [%give [%wake ~]]]
    ==
  =^  e  behn-gate  (call `%e +(wen) e-arg e-out)
  ::
  :(weld a b c d e)
::
++  test-many-unordered
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/vere] [%born ~]]
  =/  a-out  ~
  =^  a  behn-gate  (call `%a wen a-arg a-out)
  ::
  =/  b-arg  [~[/foo] [%wait (add 2 wen)]]
  =/  b-out=(list move)  [~[/vere] [%give [%doze `(add 2 wen)]]]~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  =/  c-arg  [~[/foo] [%wait (add 3 wen)]]
  =/  c-out  ~
  =^  c  behn-gate  (call `%c wen c-arg c-out)
  ::
  =/  d-arg  [~[/foo] [%wait +(wen)]]
  =/  d-out=(list move)  [~[/vere] [%give [%doze `+(wen)]]]~
  =^  d  behn-gate  (call `%d wen d-arg d-out)
  ::
  =/  e-arg  [~[/vere] [%wake ~]]
  =/  e-out=(list move)
    :~  [~[/vere] [%give [%doze `(add 2 wen)]]]
        [~[/foo] [%give [%wake ~]]]
    ==
  =^  e  behn-gate  (call `%e (add 4 wen) e-arg e-out)
  ::
  :(weld a b c d e)
::
++  test-same-ordered-lag
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/vere] [%born ~]]
  =/  a-out  ~
  =^  a  behn-gate  (call `%a wen a-arg a-out)
  ::
  =/  b-arg  [~[/foo] [%wait (add 2 wen)]]
  =/  b-out=(list move)  [~[/vere] [%give [%doze `(add 2 wen)]]]~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  =/  c-arg  [~[/foo] [%wait (add 2 wen)]]
  =/  c-out  ~
  =^  c  behn-gate  (call `%c wen c-arg c-out)
  ::
  =/  d-arg  [~[/foo] [%wait (add 2 wen)]]
  =/  d-out  ~
  =^  d  behn-gate  (call `%d wen d-arg d-out)
  ::
  =/  e-arg  [~[/vere] [%wake ~]]
  =/  e-out=(list move)
    :~  [~[/vere] [%give [%doze `(add 2 wen)]]]
        [~[/foo] [%give [%wake ~]]]
    ==
  =^  e  behn-gate  (call `%e (add 3 wen) e-arg e-out)
  ::
  :(weld a b c d e)
::
++  test-rest
  ^-  tang
  =/  wen  ~1111.1.1
  ::
  =/  a-arg  [~[/vere] [%born ~]]
  =/  a-out  ~
  =^  a  behn-gate  (call `%a wen a-arg a-out)
  ::
  =/  b-arg  [~[/foo] [%wait (add 2 wen)]]
  =/  b-out=(list move)  [~[/vere] [%give [%doze `(add 2 wen)]]]~
  =^  b  behn-gate  (call `%b wen b-arg b-out)
  ::
  =/  c-arg  [~[/foo] [%wait (add 3 wen)]]
  =/  c-out  ~
  =^  c  behn-gate  (call `%c wen c-arg c-out)
  ::
  =/  d-arg  [~[/foo] [%rest (add 2 wen)]]
  =/  d-out=(list move)  [~[/vere] [%give [%doze `(add 3 wen)]]]~
  =^  d  behn-gate  (call `%d wen d-arg d-out)
  ::
  =/  e-arg  [~[/vere] [%wake ~]]
  =/  e-out=(list move)  [~[/foo] [%give [%wake ~]]]~
  =^  e  behn-gate  (call `%e (add 4 wen) e-arg e-out)
  ::
  :(weld a b c d e)
::
++  call
  =|  lac=?
  |=  $:  label=(unit @tas)
          now=@da
          args=[=duct task=(hobo task:behn)]
          expected-moves=(list move)
      ==
  =/  behn-core  (behn-gate now=now eny=`@`0xdead.beef scry=scry)
  =^  moves  behn-gate
    (call:behn-core duct.args dud=~ task.args)
  ::
  ~?  !lac  moves
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  [?~(label output ?~(output ~ [u.label output])) behn-gate]
--
