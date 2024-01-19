/-  neo
|%
+$  poke   ?
+$  state  ?
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo raw-sta=* *]
  ++  call
    |=  ^  *(list card:neo)
  ++  reduce
    |=  raw-val=*
    =+  ;;(val=? raw-val)
    val
  ++  take
    |=  =sign:neo
    *(list card:neo)
  ++  born  *(list card:neo)
  ++  init
    |=  old=(unit *)
    &
  ++  echo
    |=  [=pith val=*]
    *(list card:neo)
  --
--
