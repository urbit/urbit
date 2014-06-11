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
  foo
::
++  peer
  |=  [ost=bone you=ship pax=path]
  ~&  %peer
  ~&  [%peer `*`[ost you pax]]
  ~&  [%peer-foo `*`foo]
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
      ;title: Hi, Philip!
    ==
    ;body 
      ;p: Yo, world.
    ==
  ==
--
