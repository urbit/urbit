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
++  deps   *deps:neo
++  farm
  |=  =bowl:neo
  ^-  vase
  =/  get  (get-dep bowl)
  !>((add (get %a) (get %b)))
--
