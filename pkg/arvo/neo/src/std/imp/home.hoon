/@  home
^-  firm:neo
|%
++  state  %home
++  poke  (sy %home ~)
++  kids  *kids:neo
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo sta=vase *]
  ++  init
    |=  old=(unit vase)
    ^-  (quip card:neo vase)
    :_  !>(*home)
    :~  [#/[p/our.bowl]/home/diary %make %diary `!>('') ~]
        [#/[p/our.bowl]/home/tasks %make %task `!>(['' | ~]) ~]
        [#/[p/our.bowl]/home/sail %make %sail `!>(['' 'prose p3' ~]) ~]
        [#/[p/our.bowl]/home/accel %make %accel `!>(~) ~]
        [#/[p/our.bowl]/home/iframes/wiki %make %iframe `!>('https://en.wikipedia.org/wiki/Main_Page') ~]
        [#/[p/our.bowl]/home/circle %make %circle ~ ~]
    ==
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(stud %home)
    ~&  !<(home vax)
    [~ vax]
  --
--
