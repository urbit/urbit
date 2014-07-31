::
::::  /hoon/core/zing/pro
  ::
/?  314
/-  zing
|_  zig=zing
::
++  grab                                                ::  convert from
  |%
  ++  noun                                              ::  convert from %noun
    |=  src=*
    ^+  +>+
    +>+(zig (zing src))
  --
--
