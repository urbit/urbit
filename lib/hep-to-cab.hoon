::   rewrite query string keys
::
::::  /hoon/hep-to-cab/lib
  ::
/?    310
::
::::  ~fyr
  ::
=<  term
|%
++  gsub                                            ::  replace chars
  |=  {a/@t b/@t t/@t}
  ^-  @t
  ?:  =('' t)  t
  %+  mix  (lsh 3 1 $(t (rsh 3 1 t)))
  =+  c=(end 3 1 t)
  ?:(=(a c) b c)
::
++  term  |=(a/^term (gsub '-' '_' a))              ::  single atom
++  path  |=(a/^path (turn a term))                 ::  path elements
++  quay                                            ::  query string keys
  |=  a/quay:eyre  ^+  a
  %+  turn  a
  |=({p/@t q/@t} [(term p) q])
--
