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
    |%
    ++  add   
      |=  [a=@ b=@]
      ^-  @
      ?:  =(0 a)  b
      $(a (dec a), b +(b))
    ::
    ++  dec
      |=  a=@
      ?<  =(0 a)
      =+  b=0
      |-  ^-  @
      ?:  =(a +(b))  b
      $(b +(b))
    --
:_  ~  :_  ~
[%xx %sage [%easy0 %pill ~] cor]
