=/  a  2
=/  b  [1 2]
=/  t  [@ @]
=>
|%
++  foo
  ^-  ?:(=(a 0) @ ^)
  ?:  =(a 0)
    a
  b
++  bar
  ^-  @
  a
--
:*  +3(a 3)
    foo
    bar.foo
    foo(a 0)
    foo(b [3 3])
==
