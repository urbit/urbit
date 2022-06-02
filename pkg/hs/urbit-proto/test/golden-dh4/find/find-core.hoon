=/  a  1
=/  b  2
=>
|%
++  foo  1/@
++  bar  2/@
++  baz  3/@
--
=/  c  3
=/  d  4
=/  bar  [5 6]
=>
|%
++  baz  [7 8]/^
++  qux  9/@
--
:*  a
    b
    c
    d
    foo
    bar
    baz
    qux
==
