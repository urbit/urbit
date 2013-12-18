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
      --
    ==
|=  *
|=  ~
^-  bowl
:_  ~  :_  ~
:-  %$
!>
(spag 3 `(list ,@)`[1 2 3 4 5 ~])
