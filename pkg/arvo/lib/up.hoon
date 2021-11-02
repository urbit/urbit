=*  k  @
=*  v  @
~%  %up-lib  ..part  ~
|%
+$  pro  (qeu (trel k @ v))
::
+$  buc  [=k =v t=pro]                                ::  bucket
::
+$  pri                                               ::  psq
  $@  ~
  $%  [%bin k=@ p=@ v=buc m=@ l=pri r=pri]
      [%tip k=@ p=@ v=buc]
  ==
::
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
::
++  fuse                                              ::  disjoint tree merge
  ~/  %fuse
  |=  [m=@ l=pri r=pri]
  ^-  pri
  ?~  l  r
  ?-    -.l
      %tip
    ?~  r  l
    :-  %bin
    ?-    -.r
        %tip
      ?:  (lex [p.l k.l] [p.r k.r])
        [k.l p.l v.l m ~ r]
      [k.r p.r v.r m l ~]
    ::
        %bin
      ?:  (lex [p.l k.l] [p.r k.r])
        [k.l p.l v.l m ~ r]
      [k.r p.r v.r m l $(m m.r, l l.r, r r.r)]
    ==
  ::
      %bin
    ?~  r  l
    :-  %bin
    ?-    -.r
        %tip
      ?:  (lex [p.l k.l] [p.r k.r])
        [k.l p.l v.l m $(m m.l, l l.l, r r.l) r]
      [k.r p.r v.r m l ~]
    ::
        %bin
      ?:  (lex [p.l k.l] [p.r k.r])
        [k.l p.l v.l m $(m m.l, l l.l, r r.l) r]
      [k.r p.r v.r m l $(m m.r, l l.r, r r.r)]
    ==
  ==
::
++  funk                                              ::  bin shrink-left
  ~/  %funk
  |=  [k=@ p=@ v=buc m=@ l=pri r=pri]
  ^-  pri
  ?~  l
    ?~  r  [%tip k p v]
    :-  %bin
    ?-  -.r
      %tip  [k p v m ~ r]
      %bin  [k p v m ~ r]
    ==
  [%bin k p v m l r]
::
++  wane                                              ::  bin shrink-right
  ~/  %wane
  |=  [k=@ p=@ v=buc m=@ l=pri r=pri]
  ^-  pri
  ?~  r
    ?~  l  [%tip k p v]
    :-  %bin
    ?-  -.l
      %tip  [k p v m l ~]
      %bin  [k p v m l ~]
    ==
  [%bin k p v m l r]
--
