~%  %up-lib  ..part  ~
|%
++  zero                                              ::  zero
  ~/  %zero
  |=  [k=@ n=@]
  ^-  ?
  =(0 (dis k n))
::
++  gone                                              ::  nomatch
  ~/  %gone
  |=  [k=@ l=@ m=@]
  ^-  ?
  =/  n  (mask m)
  !=((dis k n) (dis l n))
::
++  mask                                              ::  maskw
  ~/  %mask
  |=  a=@
  ^-  @
  (mix (not 5 1 (dec a)) a)
::
++  pert                                              ::  branch mask
  ~/  %pert
  |=  [k=@ l=@]
  ^-  @
  (high (mix k l))
::
++  high                                              ::  highest bitmask
  ~/  %high
  |=  a=@
  ^-  @
  (rsh 0 (bex (xeb a)))
::
++  lex                                               ::  lexicographic order
  ~/  %lex                                            ::  [atom atom]
  |=  [[p=@ k=@] [q=@ l=@]]
  ^-  ?
  ?:  =(p q)
    (lth k l)
  (lth p q)
--

