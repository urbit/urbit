::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  {^ {{txt/@tas $~} $~}}
~&  %foobar
=+  bar=32
=>  |%
    ++  funq
      ^?
      |%
      ++  add   |=({a/@ b/@} (sub a b))
      ++  mook  txt
      --
    --
=,  funq
~&  %one
=+  foo=mook
~&  [%foo (^add 2 2)]
=<  $
|%
++  $
  :-  %noun
  (crip (weld "hello, " (trip mook)))
--
