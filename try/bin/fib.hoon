!:
::  /=try=/bin/fib/hoon  
::
|=  *
|=  [num=@ud ~]
:_  ~  :_  ~
:-  %$
!>  %.  num
    |=(x=@ ?:((lth x 2) 1 (add $(x (dec x)) $(x (sub x 2)))))
