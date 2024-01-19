/-  neo
|*  =mold
|=  start=mold
^-  firm:neo
|%
++  poke  mold
++  state  mold
++  deps   *deps:neo
++  form
  |_  [=bowl:neo raw-sta=* *]
  ++  call
    |=  ^  *(list card:neo)
  ++  reduce
    |=  raw-val=*
    =+  ;;(val=mold raw-val)
    val
  ++  take
    |=  =sign:neo
    *(list card:neo)
  ++  born  *(list card:neo)
  ++  init
    |=  old=(unit *)
    start
  ++  echo
    |=  [=pith val=*]
    *(list card)
  --
--

