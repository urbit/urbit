/@  sandbox
/@  sandbox-diff
^-  firm:neo
|%
++  state  %sandbox
++  poke  (sy %sandbox %sandbox-diff ~)
++  kids  *kids:neo
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo sta=vase *]
  ++  init
    |=  new=(unit vase)
    ^-  (quip card:neo vase)
    :-  ~
    ?~  new
      !>(*sandbox)
    u.new
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?+    stud  !!
        %sandbox-diff
      =/  poke  !<(sandbox-diff vax)
      ?-    -.poke
          %make
        :_  sta
        :~  [(snoc here.bowl name.poke) %make stud.poke ~ ~]
        ==
      ::
          %tomb
        !!
      ==
    ==
  --
--
