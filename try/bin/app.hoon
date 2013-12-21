!:
::  /=main=/bin/tach/hoon
::
=>  %=    .
        +
      =>  +
      |%
      ++  word
        %hello
      ::
      ++  spag                                                ::  index
        |#  [a=@ b=(list)]
        ?~  b
          ~|('snag-fail' !!)
        ?:  =(0 a)
          ~_  b 
          i.b
        $(b t.b, a (dec a))
      ::
      ++  humo                                                ::  homogenize
        |#  a=(list)
        |-
        ^-  $_  =<  $
                |%
                  +-  $
                    ?:  _?
                      ~
                    [i=(snag 0 a) t=$]
                --
        a
      --
    ==
|=  *
|=  ~
^-  bowl
:_  ~  :_  ~
:-  %$
!>
=+  foo=[1 2 3 4 5 ~]
(homo foo)
