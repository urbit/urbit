!:     
=>  |%
    ++  foobug-state
      $%  [%0 bug=@ud]
      ==
    --
|=  *
|_  [hide foo=foobug-state]
++  prep
  |=  old=(unit foobug-state)
  ?~  old  +>
  +>(foo u.old)
::
++  save
  ^-  foobug-state
  foo(bug +(bug.foo))
::
++  peer
  |=  [ost=bone you=ship pax=path]
  ~&  [%peer [ost you pax]]
  :_  +>  :_  ~
  :+  ost  %give
  :-  %rust
  :-  %html
  %-  crip  
  %+  xmlt  |
  :_  ~
  ^-  manx
  ;html
    ;head
      ;title: Hi, everyone!
    ==
    ;body 
      ;p: Hello, world (instance {<bug.foo>})
    ==
  ==
--
