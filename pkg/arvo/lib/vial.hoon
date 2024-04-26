=>  %1  =>
~%  %v.1  ~  ~
|%
++  vial-version  +
--  =>
~%  %one  +  ~
|%
++  fat
  ~/  %fat
  |=  [a=* b=*]  42
::
++  inc
  ~/  %inc
  |=  a=@
  ^-  @
  +(a)
--
~%  %two  +  ~
|%
++  foo
  ~/  %foo
  |=  c=@
  ^-  @
  +(c)
--

:: [l=/v.1 s=[& %v.1]]
::
:: [l=/one/v.1 s=[[& &] [<one battery> %v.1]]
:: [l=/fat/one/v.1 s=[[& | & &] <fat battery> ~ <one battery> %v.1]
:: [l=/inc/one/v.1 s=[[& | & &] <inc battery> ~ <one battery> %v.1]
::
:: [l=/two/v.1 s=[[& &] [<one battery> %v.1]]
:: [l=/foo/two/v.1 s=[[& | & &] <foo battery> ~ <two battery> %v.1]
