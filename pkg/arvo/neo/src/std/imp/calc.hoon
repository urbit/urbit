^-  plot:neo
=>
  |%
  ++  get-dep
    |=  =bowl:neo
    |=  sid=?(%a %b)
    ^-  @ud
    !<(@ud q.pail.q:(~(got by deps.bowl) sid))
  --
|%
++  state  %atom
++  kids   *kids:neo
++  deps   
  %-  ~(gas by *deps:neo)
  :~  [%a & [%atom %sig] ~]
      [%b & [%atom %sig] ~]
  ==
++  farm
  |=  =bowl:neo
  ^-  vase
  =/  get  (get-dep bowl)
  !>((add (get %a) (get %b)))
--
