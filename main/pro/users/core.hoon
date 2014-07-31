::
::::  /hoon/core/users/pro
  ::
/?  314
/-  users
|_  use=users
::
++  grab                                                ::  convert from
  |%
  ++  noun                                              ::  convert from %noun
    |=  src=*
    ^+  +>+
    +>+(use (users src))
  --
--
