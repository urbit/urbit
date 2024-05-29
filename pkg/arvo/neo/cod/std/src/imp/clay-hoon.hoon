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
  |_  [=bowl:neo =aeon:neo =pail:neo]
  +*  sta  !<(@t state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%clay-res stud)
    =+  !<(=res:clay:neo vax)
    :_  pail
    %+  murn  ~(tap of files.res)
    |=  [=path =cage]
    ^-  (unit card:neo)
    ?.  =(%hoon p.cage)
      ~
    =.  path  (snip path)
    ~&  making/path
    `[(welp here.bowl (pave:neo path)) %make %hoon `cage ~]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =+  !<(=desk q:(need old))
    :_  desk/!>(desk)
    [#/[p/our.bowl]/$/clay %poke %clay-req !>(`req:clay:neo`[%peer ~ desk /neo ~])]^~
  --
--
  
