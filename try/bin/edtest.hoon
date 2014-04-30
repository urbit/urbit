!:
:: /=try=/bin/edtest/hoon
::
=>  %=    .
        +
    =>  +
    |%
    ++  swab  |=(a=@ (rep 3 (flop (rip 3 a))))
    ++  tase  ,[sk=@ux pk=@ux m=@ux sm=@ux]               ::  test-case
    ++  read
      |.
      =+  fil=((hard ,@) .^(%cx /===/doc/ed/test))
      (rash fil pars)
    ++  line
      %+  knee  *tase  |.  ~+
      ;~  plug
        ;~(sfix (bass 16 (star hit)) col)
        ;~(sfix (bass 16 (star hit)) col)
        ;~(sfix (bass 16 (star hit)) col)
        ;~(sfix (bass 16 (star hit)) col (just `@`10)) 
      ==
    ++  pars
      %+  knee  *(list tase)  |.  ~+
      (star line)
    ++  chek
      |=  a=tase
      =+  gsk=(swab (cut 3 [32 64] sk.a))                 ::  endian hell
      =+  gpk=(swab (cut 3 [0 32] sk.a))
      =+  sig=(sign:ed (swab m.a) gsk)
      =+  cpk=(puck:ed gsk)
      =+  tsg=(cut 3 [0 64] (swab sm.a))
      =+  ypk=(swab pk.a)
      =+  [a=(veri:ed tsg (swab m.a) ypk) b==(ypk cpk) c==(sig tsg)]
      ~&  [%chek a b c]
      ?&(a b c)
    ++  test
      |=  a=(list tase)
      ^-  (list ,*)
      ?~  a
        ~
      [(chek i.a) $(a t.a)]
    --
  ==
|=  [est=time eny=@uw]
|=  ~
^-  bowl
~&  (test (read))
[~ ~]
