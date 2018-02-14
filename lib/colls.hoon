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
--
