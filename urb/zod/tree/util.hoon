!:
|_  gas=epic
++  get-path 
  ^-  path
  (tope bem.gas(s but.gas))
::
++  get-parent
  ^-  path
  (tope bem.gas(s (slag 1 but.gas)))
::
++  get-tree 
  ^-  arch
  ((hard arch) .^(%cy get-path))
++  kids-json
  |=  kids=(list ,@ta)
  :-  %a
    %+  turn  kids
  |=  kid=@t  [%s kid]
++  bread-json
  |=  kids=(list path)
  :-  %a
    %+  turn  kids
  |=  kid=path  [%s (spat (flop kid))]
++  parent-json  |=(pax=path [%s (spat (slag 3 pax))])
++  get-kids
  %-  sort  :_  aor
  ^-  (list span)
  %+  murn  (~(tap by r:get-tree))
  =+  pax=get-path
  |=  [kid=span ~]  ^-  (unit span)
  =+  ark=;;(arch .^(%cy (welp pax /[kid])))
  ?~  r.ark  ~
  (some kid)
::
++  get-bread
  =+  [pax=`path`/pub paf=(flop but.gas)]
  |-  ^-  (list path)
  ?~  paf  ~[pax]
  [pax $(paf t.paf, pax [i.paf pax])]
--
