::  test the behavior of the new mink (temporary file)
:-  %say
|=  [[now=@da * [our=@p *]] *]
:-  %noun
?>  ?=  [%2 *]
  (mino [0 11 [%slog 1 2 leaf+"CHECK FOR ME"] 0 0] |=(* ~))
=/  pass
  |=  [ref=* gof=*]
  =/  fol  [12 [1 ref] 1 gof]
  =/  pro  .*(~ fol)
  ``pro
=/  good
  |=  =path
  ^-  ?
  =/  raw=@t  .^(@t %cx path)
  =/  virt=tono
    (mino [. !=(.^(@t %cx path))] pass)
  ?.  ?=(%0 -.virt)  |
  =(product.virt raw)
?>  (good /(scot %p our)/home/(scot %da now)/gen/hello/hoon)
=/  scry
  |=  [a=* b=*]
  ^-  (unit (unit))
  =/  al=?  (? a)
  ::  not a (list @ta), so mush will give an empty rose
  =/  c=(pair path *)  ((pair path *) b)
  ?.  &(=(a al) =(b c))  [~ ~]
  ?.  al  ~
  ?~  p.c  [~ ~]
  ``q.c
?>  ?&
  =([%0 42] (mino [0 12 [1 0] [1 /foo] 1 42] scry))
  =([%1 /foo 42] (mino [0 12 [1 1] [1 /foo] 1 42] scry))
  =([%1 0 42] (mino [0 12 [1 1] [1 ~] 1 42] scry))
  ?=([%2 *] (mino [0 12 [1 0] [1 ~] 1 42] scry))
  .=  :-  2
      :~  [%spot 1]
          [%lose 2]
          [%mean 3]
      ::  [%hand 4]
          [%hunk 5]
      ==
  %+  mino
    :-  0
    :*  11  [%hunk 1 5]
    ::  11  [%hand 1 4]
        11  [%mean 1 3]
        11  [%lose 1 2]
        11  [%spot 1 1]
        0   0
     ==
  |=(* ~)
==
=/  tests=(list [subject=* formula=* product=(unit *)])
  :~  :+  0  [0 0]  ~
      :+  0  [0 1]  `0
      :+  0  [0 2]  ~
      :+  [0 1 2]  [0 2]  `0
      :+  [0 1 2]  [0 3]  `[1 2]
      :+  [0 1 2]  [0 4]  ~
      :+  [0 1 2]  [0 5]  ~
      :+  [0 1 2]  [0 6]  `1
      :+  [0 1 2]  [0 7]  `2
      :+  0  [1 42]  `42
      :+  0  [1 1 2 3 4]  `[1 2 3 4]
      :+  [[0 3] 1 2 3]  [2 [0 3] 0 2]  `[2 3]
      :+  42  [3 0 1]  `1
      :+  [0 42]  [3 0 1]  `0
      :+  41  [4 0 1]  `42
      :+  [0 41]  [4 0 1]  ~
      :+  [42 43]  [5 [0 2] [0 3]]  `1
      :+  [42 43]  [5 [4 0 2] [0 3]]  `0
      :+  [42 43]  [5 0 1]  ~
      :+  0  [6 [1 0] [1 42] 0]  `42
      :+  0  [6 [1 1] [1 42] 0]  ~
      :+  0  [6 [1 0] [1 42] 1 43]  `42
      :+  0  [6 [1 1] [1 42] 1 43]  `43
      :+  0  [6 [1 2] [1 42] 1 42]  ~
      :+  0  [7 [1 2 3] 4 4 4 0 2]  `5
      :+  0  [8 [1 2 3] 4 4 4 0 4]  `5
      :+  0  [8 [1 2 3] 0 3]  `0
      :+  0  [9 2 1 [0 6] 42 0]  `42
      :+  [0 0 42]  [7 [10 [6 0 7] 0 1] 0 6]  `42
      :+  [0 0]  [10 [1 1 42] 0 2]  `42
      :+  0  [10 [1 1 42] 0 2]  ~
      :+  0  [10 [2 1 42] 0 1]  ~
      :+  0  [10 [3 1 42] 0 1]  ~
      :+  [0 0]  [10 [3 1 42] 0 1]  `[0 42]
      :+  [0 0]  [10 [2 1 42] 0 1]  `[42 0]
      :+  [0 0 0]  [10 [6 1 42] 0 1]  `[0 42 0]
      :+  [0 0 0]  [10 [7 1 42] 0 1]  `[0 0 42]
      :+  0  [11 %hint 1 42]  `42
      :+  0  [11 [%hint 1 42] 1 42]  `42
      :+  0  [11 [%hint 0] 1 42]  ~
      :+  0  !=  =<  (fib 5)
             |%
             ++  dec
               |=  n=@
               ?:  =(0 n)  !!
               =|  i=@
               |-
               =/  ip  +(i)
               ?:  =(n ip)  i
               $(i ip)
             ++  add
               |=  [a=@ b=@]
               ?:  =(0 a)  b
               $(a (dec a), b +(b))
             ++  mul
               |=  [a=@ b=@]
               ?:  |(=(0 a) =(0 b))  0
               =/  r=@  a
               |-
               ?:  =(b 1)  r
               $(b (dec b), r (add a r))
             ++  fib
               |=  n=@
               ?:  =(0 n)  0
               =/  a=@  1
               |-
               ?:  =(1 n)  a
               $(n (dec n), a (mul a n))
             --
      [~ :(mul 5 4 3 2)]
  ==
=/  scry  |=(* ~)
|-  ^-  ?
?~  tests  &
=/  t  i.tests
=/  r  (mino [subject.t formula.t] scry)
?~  product.t
  ?.  ?=(%0 -.r)
    $(tests t.tests)
  ~&  [no-crash=t got=product.r]
  %.n
?:  &(?=(%0 -.r) =(product.r u.product.t))
  $(tests t.tests)
~&  [failed=t got=r]
%.n
