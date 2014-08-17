!:
::  Define  ::
=>  |%
    ++  isin   |=  [a=tape b=tape]
      ?.  (gte (lent a) (lent b))  |
      ?:  =(b (scag (lent b) a))   &
      $(a +.a)
    ::
    ++  range  |=  [[a=tape b=tape] c=wain]
      ^-  wain
      ?~  c  ~
      ?.  (isin (trip i.c) a)  $(c t.c)
      :-  i.c  =>  .(c t.c)
      |-  ^-  wain
      ?~  c  ~
      ?:  (isin (trip i.c) b)  ~
      [i.c $(c t.c)]
    ::
    ++  vanes  
      %-  turn  :_  |=([a=@t ~] a)
      (~(tap by r:;;(arch .^(cy//=arvo=))))
    --
|=  ^
|=  [a=@tas ~]
=-  ~[te/-]~
%-  zing
%+  turn  vanes  |=  b=@t
=-  ?~  -  ~
    [(cat 3 '%' b) -]
%+  range  ["++  {(trip a)}" "++"]
=-  (lore ;;(,@ .^(cx//=arvo=/[b]/hoon)))
'''
++  by
my 
stuff
++ron

as
'''
