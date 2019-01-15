=>  $~  |%
    ++  move  (pair bone card)
    ++  card
      $%  [%turf wire ~]
          [%vein wire]
          [%look wire src=(each ship purl:eyre)]
          [%wind wire p=@ud]
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
  ~&  %ouch
  :_  +>.$
  ?+  val
        ~&(%oops ~)
    %turf  [ost.hid %turf /hi ~]~                       ::  +
    %vein  [ost.hid %vein /hi]~                 ::  +
    %look  :_  ~                                        ::  +
           =/  pul  |+(need (de-purl:html 'http://eth-mainnet.urbit.org:8545'))
           [ost.hid %look /hi pul]
    %wind  :_  ~                                        ::  -
           :*  ost.hid  %wind  /hi
               ::  0
               4.280.000
           ==
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
