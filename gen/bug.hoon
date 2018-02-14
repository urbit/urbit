::  "Hello world" sample generator  
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
!:
^%
:-  %say
|=  *
=>  |%
++  raip
  |*  {a/$-(* *) b/$-(* *)} 
  {p/a q/b}
::
::
++  etre
  |*  a/$-(* *) 
  $@  ~ 
  {n/a l/(etre a) r/(etre a)}
::
++  mapp  
  |*  {a/$-(* *) b/$-(* *)} 
  =+  foo=(raip a b)
  (etre foo)
::
++  ankh                                                ::  expanded node
  $~  [~ ~]
  $:  fil/(unit {p/@uvI q/cage})                        ::  file
      dir/(mapp @ta ankh)                                ::  folders
  ==                                                    ::
--
=>  |%
    ++  de
      |=  foo/@ud
      !!
    ++  moo
      $:ankh
    --
:-  %noun
=+  foo=moo
"hello, world"
