=>
|%
+$  card  card:neo
--
^-  kook:neo
|%
++  state  pro/%desk
++  poke   (sy %clay-res ~)
++  deps   *deps:neo
++  kids   *kids:neo
++  form
  |_  [=bowl:neo =aeon:neo s=stud:neo state-vase=vase]
  +*  sta  !<(@t state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%clay-res stud)
    =+  !<(=res:clay:neo vax)
    :_  desk/state-vase
    %+  murn  ~(tap axol-of:neo files.res)
    |=  [=path =cage]
    ^-  (unit card)
    ?.  =(%mime p.cage)
      ~
    `[(welp here.bowl (pave:neo path)) %make %mime `[%mime q.cage] ~]
  ++  init
    |=  old=(unit pail:neo)
    =+  !<(=desk q:(need old))
    :_  desk/!>(desk)
    [#/[p/our.bowl]/$/clay %poke %clay-req !>(`req:clay:neo`[%peer ~ desk /neo `%mime])]^~
  --
--
  
