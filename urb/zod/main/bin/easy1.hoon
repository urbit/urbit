!:
::  /=main=/bin/solid/hoon
::
=>  .(- `[who=@p how=path]`-)
|=  [est=time eny=@uw]
|=  arg=~
=+  ^=  cor
    =<  |=  [x=@ y=@]
        (add x y)
    =>  %164
    ~%  %k164  ~  ~
    |%
    ++  add   
      ~/  %add
      |=  [a=@ b=@]
      ^-  @
      ?:  =(0 a)  b
      $(a (dec a), b +(b))
    ::
    ++  dec
      ~/  %dec
      |=  a=@
      ~|  %decrement-underflow
      ?<  =(0 a)
      =+  b=0
      |-  ^-  @
      ?:  =(a +(b))  b
      $(b +(b))
    --
:_  ~  :_  ~
[%xx %sage [%easy1 %pill ~] cor]
