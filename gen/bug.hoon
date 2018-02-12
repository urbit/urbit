::  "Hello world" sample generator  
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
!:
:-  %say
|=  *
=,  clay
=>  |%
++  kant  ^%
          $~  [%leaf ~]                                 ::
          $%  {$leaf p/tape}                            ::  printing formats
              $:  $palm                                 ::  backstep list
                  p/@ud
                  q/(list kant)                         ::
              ==                                        ::
              $:  $rose                                 ::  flat list
                  p/{p/tape q/tape r/tape}              ::  mid open close
                  q/(list kant)                         ::
              ==                                        ::
          ==                                            ::
--
=>  |%
    ++  de
      |=  foo/@ud
      !!
    --
:-  %noun
"hello, world"
