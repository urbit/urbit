/+  old-zuse
=,  old-zuse
|%
:: sort knots by date
:: TODO when we change the write path to have trailing sig, remove here before sort
++  dor
  |=  [a=knot b=knot]
  (gth (unt (slav %da a)) (unt (slav %da b)))
:: checks if authorized
++  authed
  |=  gas/epic
  ^-  ?
  %+  lien
    ~(tap in (~(get ju aut.ced.gas) %$)) 
  |=  b/knot 
  =((scot %p p.bem.gas) b)
++  no-title
  |=  wat/wain
  ^-  wain
  ?:  =((scag 2 (trip -:wat)) "# ")
    +:wat
  wat
++  esoo
  |=  d/@d
  ^-  tape
  =/  t  (yore d)
  ;:  welp
      (scag 1 (scow %ud y.t))
      (swag [2 3] (scow %ud y.t))
      "-"
      (double m.t)
      "-"
      (double d.t.t)
      "T"
      (double h.t.t)
      ":"
      (double m.t.t)
      ":"
      (double s.t.t)
      "Z"
  ==
:: ud to leading zero tape
++  double
  |=  a/@ud
  ^-  tape
  =/  x  (scow %ud a)
  ?:  (lth a 10)
    (welp "0" x)
  x
--
