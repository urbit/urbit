::  usage:
::  /-  pill
::  =p .^(pill:pill %cx %/urbit/pill)
::  |start %here
::  :here &pill p
::  :here %init
::  :here [%dojo "+ls %"]
::  :here [%dojo "our"]
::
/-  pill
=,  pill
=>  $~  |%
    ++  move  (pair bone card)
    ++  card
      $%  [%turf wire ~]
          [%vein wire]
          [%look wire src=(each ship purl:eyre)]
          [%wind wire p=@ud]
          [%snap wire snap=snapshot:jael kick=?]
      ==
    ++  state
      $:  pil=pill
          roc=*
      ==
    --
=,  gall
|_  $:  hid/bowl
        state
    ==
++  poke-pill
  |=  p=pill
  ^-  (quip move _+>)
  =.  pil  p
  ~&  lent=(met 3 (jam boot-ova.pil))
  =/  res=toon :: (each * (list tank))
    (mock [boot-ova.pil [2 [0 3] [0 2]]] scry)
  ?-  -.res
      %0
    ~&  %suc
    =.  roc  +7.p.res
    `+>.$
    ::(u3v-plow userspace-ova.pil)
  ::
      %1
    ~&  [%vere-blocked p.res]
    `+>.$
  ::
      %2
    ~&  %vere-fail
    %-  (slog p.res)
    `+>.$
  ==
::
++  u3v-plow
  |=  ova=*
  ^-  (quip move _+>)
  =+  ova=((list ,*) ova)
  ?~  ova
    `+>.$
  =/  res  (mox +47.roc)
  ?>  ?=(%0 -.res)
  =+  poke=p.res
  =+  res=(slum poke now.hid i.ova)
  =+  effects=((list ovum) -.res)
  ::  ~&  effects
  =+  %+  turn  effects
      |=  ovo=ovum
      ~?  =(%blit p.q.ovo)
        :+  p.ovo  p.q.ovo
        =+  bs=((list blit:dill) q.q.ovo)
        %+  turn  bs
        |=  b=blit:dill
        ?:  ?=(%lin -.b)
          [%lin (tape p.b)]
        b
        ::  [p.ovo p.q.ovo %hrm ] ::((list blit:dill) q.q.ovo)]
      ~?  !=(%blit p.q.ovo)
        ovo
      ~
  =.  roc  +3.res
  $(ova t.ova)
::
++  poke-noun
  |=  val=*
  ^-  (quip move _+>)
  ~&  r=(met 3 (jam roc))
  ?+  val  ~|(%bad-noun-arg !!)
      %init
    =+  who=~bud
    %-  u3v-plow
    :~
        [/ %wack 0]  ::  eny
        [/ %whom who]  ::  eny
        [//newt/0v1n.2m9vh %barn ~]
        [//behn/0v1n.2m9vh %born ~]
        [//term/1 %boot %fake who]
        -.userspace-ova.pil
        [//http/0v1n.2m9vh %live 8.080 `8.445]
        [//term/1 %belt %ctl %x]
    ==
  ::
      [%dojo p=*]
    %-  u3v-plow
    :~  
        [//term/1 %belt %ctl %e]
        [//term/1 %belt %ctl %u]
        [//term/1 %belt %txt (tape p.val)]
        [//term/1 %belt %ret ~]
    ==
  ::
      [%peek p=*]
    =+  res=(mox +46.roc)
    ?>  ?=(%0 -.res)
    =+  peek=p.res
    ~&  (slum peek p.val)
    `+>.$
  ::
      [%wish p=@t]
    =+  res=(mox +22.roc)
    ?>  ?=(%0 -.res)
    =+  wish=p.res
    ~&  (slum wish p.val)
    `+>.$
  ==
::
++  mox  |=(* (mock [roc +<] scry))
::
++  scry  |=([* *] ~)
::
++  prep
  |=  old/(unit noun)
  ^-  [(list move) _+>.$]
  ?~  old
    `+>.$
  =+  new=((soft state) u.old)
  ?~  new
    `+>.$
  `+>.$(+<+ u.new)
--
