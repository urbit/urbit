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
  (tope bem.gas(s (welp but.gas /pub)))
::
++  get-parent
  ^-  path
  (tope bem.gas(s (welp (slag 1 but.gas) /pub)))
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
++  parent-json
  |=  pare=path  [%s (spat (slag 3 pare))]
++  get-kids
  %-  sort  :_  aor
  ^-  (list span)
  %+  murn  (~(tap by r:get-tree))
  =+  pax=get-path
  |=  [kid=span ~]  ^-  (unit span)
  =+  ark=;;(arch .^(%cy (welp pax /[kid])))
  ?~  r.ark  ~
  (some kid)
++  get-link
  |=  [pax=path mal=marl]  
  ^-  manx
  =+  rut=`path`[q.bem.gas (flop (slag 1 s.bem.gas))]
  ;a/"/gen{<rut>}{<(slag 4 pax)>}":"*{mal}"
++  get-down
  ^-  marl
  %+  turn  get-kids
  |=  a=span
  ;span:  +{(get-link (welp get-path /[a]) ;"{(trip a)}")}
++  render-bread
  |=  a=path
  ^-  manx
  ;span: /+{(get-link (tope bem.gas(s a)) ;"{(trip (snag 0 a))}")}
::
++  get-bread
  =+  [pax=`path`/pub paf=(flop but.gas)]
  |-  ^-  (list path)
  ?~  paf  ~[pax]
  [pax $(paf t.paf, pax [i.paf pax])]
--
