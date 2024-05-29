/@  folder
/@  folder-diff
^-  kook:neo
|%
++  state  pro/%folder
++  poke  (sy %folder %folder-diff ~)
++  kids  *kids:neo
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  new=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    ?^  new  u.new
    folder/!>(*folder)
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  !!
        %folder-diff
      =/  poke  !<(folder-diff vax)
      =/  this  !<(folder q.pail)
      ?-    -.poke
          %make
        :_  folder/!>([name.poke this])
        :~  [(snoc here.bowl name.poke) %make stud.poke ~ ~]
        ==
      ::
          %tomb
        :-  ~
        =/  i  (need (find ~[name.poke] this))
        folder/!>((oust [i 1] this))
      ==
    ==
  --
--
