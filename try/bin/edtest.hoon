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
      ~|  a
      =+  gsk=`@ux`(cut 3 [0 32] (swab sk.a))               ::  endian hell
      =+  cpk=`@ux`(puck:ed gsk)
      =+  ypk=`@ux`(swab pk.a)
      ~|  [%pk cpk ypk]
      ?>  =(cpk ypk)
      =+  sig=`@ux`(sign:ed (swab m.a) gsk)
      =+  tsg=`@ux`(cut 3 [0 64] (swab sm.a))
      ~|  [%sg sig tsg]
      ?>  =(sig tsg)
      ?>  (veri:ed tsg (swab m.a) ypk)
      ~&  [%a-ok a]
      &
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
