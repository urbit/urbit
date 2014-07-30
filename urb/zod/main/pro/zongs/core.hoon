::
::::  /hoon/core/zing/pro
  ::
/?  314
/-  zong
|_  zog=(list zong)
::
++  grab                                                ::  convert from
  |%
  ++  noun                                              ::  convert from %noun
    |=  src=*
    ^+  +>+
    +>+(zog ((list zong) src))
  --
--
