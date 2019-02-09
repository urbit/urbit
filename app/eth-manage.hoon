::  usage:
::    :eth-manage %look
::      kick polling from eth mainnet node
::    :eth-manage [%wind 1.000.000]
::      rewind to block 1.000.000
=>  $~  |%
    ++  move  (pair bone card)
    ++  card
      $%  [%turf wire ~]
          [%vein wire]
          [%look wire src=(each ship purl:eyre)]
          [%wind wire p=@ud]
          [%snap wire snapshot=snapshot:jael kick=?]
      ==
    ++  state
      $:  a/@
      ==
    --
=,  gall
|_  $:  hid/bowl
        state
    ==
++  poke
  |=  [mar=@tas val=*]
  ^-  (quip move _+>)
  :_  +>.$
  ?+  val  ~&(%oops ~)
      %turf         [ost.hid %turf /hi ~]~
      %vein         [ost.hid %vein /hi]~
      [%wind @ud]   [ost.hid %wind /hi +.val]~
      [%snap * ?]
    [ost.hid %snap /hi (snapshot:jael +<.val) +>.val]~
  ::
      %look-ethnode
    :_  ~
    =/  pul
      (need (de-purl:html 'http://eth-mainnet.urbit.org:8545'))
    [ost.hid %look /hi |+pul]
  ::
      [%look-kick who=@p]
    :_  ~
    [ost.hid %look /hi %& who.val]
  ==
::
++  vein
  |=  [wir/wire =life ven=(map life ring)]
  ^-  (quip move _+>)
  ~&  [%pierc life ven]
  `+>.$
::
++  turf
  |=  [wir/wire pax=(list path)]
  ^-  (quip move _+>)
  ~&  [%slurp pax]
  `+>.$
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
