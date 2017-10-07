::
::  part 1: parse the file into {uppers}
::
/-  unicode-data
::
::  while this works, it'd be better to build range based data structures like
::  golang does. golang uses flat tables that it binary searches over. storage
::  as a binary tree?
::
/=  uppers
  /;  |=  a/(list line:unicode-data)
      =|  ret/(map @c @c)
      |-
      ^-  (map @c @c)
      ?~  a
        ret
      ?~  up.i.a
        $(a t.a)
      $(a t.a, ret (~(put by ret) code.i.a u.up.i.a))
  /:  /===/lib/unicode-data  /&unicode-data&/txt/
::
::  part 2: utility core
::
|%
++  transform
  |=  {a/tape fun/$-(@c @c)}
  %-  tufa
  (turn (tuba a) fun)
::
++  to-upper
  |=  a/@c
  ^-  @c
  ::  special case ascii to not perform map lookup.
  ?:  (lte a max-ascii)
    ?:  &((gte a 'a') (lte a 'z'))
      (sub a 32)
    a
  =+  x=(~(get by uppers) a)
  (fall x a)
::
++  max-ascii  `@c`0x7f
--
::
::  part 3: generator
::
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        {n/tape $~}
        $~
    ==
:-  %noun
(transform n to-upper)
