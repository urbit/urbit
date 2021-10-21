|%
+$  cask  (^cask *)
+$  task
  $%  [%watch ~]
      [%poke =cask]
      [%leave ~]
  ==
::
+$  sign
  $%  [%arvo p=sign-arvo]
      [%agent p=sign:agent:gall]
  ==
+$  card  card:agent:gall
::
+$  update
  [=path =cask]
::
++  agent
  $_  ^|
  |_  bowl:gall
  ++  init
    *(quip card _^|(..init))
  ::
  ++  stay
    *vase
  ::
  ++  load
    |~  old-state=vase
    *(quip card _^|(..init))
  ::
  ++  poke
    |~  [mark vase]
    *(quip card _^|(..init))
  ::
  ++  call
    |~  [path task]
    *(quip card _^|(..init))
  ::
  ++  take
    |~  [wire sign]
    *(quip card _^|(..init))
  ::
  ++  peek
    |~  path
    *(unit (unit cage))
  ::
  ++  goof
    |~  [term tang]
    *(quip card _^|(..init))
  --
++  adapt
  |=  a=agent
  ^-  agent:gall
  |_  =bowl:gall
  +*  this  .
      agent  ~(. a bowl)
  ++  on-init
    =^  cards  a
      init:agent
    [cards this]
  ++  on-save  stay:agent
  ++  on-load
    |=  =vase
    =^  cards  a
      (load:agent vase)
    [cards this]
  ++  on-poke
    |=  [=mark =vase]
    =^  cards  a
      ?.  =(%mall-update mark)
        (poke:agent mark vase)
      =+  !<([=path =cask] vase)
      (call:agent path %poke cask)
    [cards this]
  ::
  ++  on-watch
    |=  =path
    =^  cards  a
      (call:agent path %watch ~)
    [cards this]
  ::
  ++  on-leave
    |=  =path
    =^  cards  a
      (call:agent path %leave ~)
    [cards this]
  ::
  ++  on-peek  peek:agent
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    =^  cards  a
      (take:agent wire %agent sign)
    [cards this]
  ::
  ++  on-arvo
    |=  [=wire sign=sign-arvo]
    =^  cards  a
      (take:agent wire %arvo sign)
    [cards this]
  ++  on-fail
    |=  [=term =tang]
    =^  cards  a
      (goof:agent term tang)
    [cards this]
  --
--
