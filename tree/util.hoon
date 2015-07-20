!:
|_  gas=epic
++  baff  |*([a=(unit) b=(trap)] ?^(a a *b))
++  find-in-tree
  |*  [paz=fist:jo fun=$+(* (unit))]
  |=  a=json  ^+  *fun
  %+  biff  (paz a)
  |*  [b=(list json) c=*]  ^+  *fun
  %+  baff  (fun c)
  |.
  ?~  b  ~
  (baff ^^$(a i.b) |.(^$(b t.b)))
::
++  json-front
  |=  a=json  ^-  json
  =-  (fall `(unit json)`- ~)
  %+  biff  ((ot body/some ~):jo a)
  %+  find-in-tree  (ot c/(ar some) gn/so ga/(om so) ~):jo
  |=  [nom=span atr=(map span cord)]  ^-  (unit json)
  ?.  (~(has by atr) 'urb:front')  ~
  ?>  ?=(%meta nom)
  (biff (~(get by atr) %value) poja)
::
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
