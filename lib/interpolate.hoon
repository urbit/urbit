::   /foo/:bar/baz interpolation syntax
::
::::  /hoon/interpolate/lib
  ::
/?    310                        
::
::::  ~fyr
  ::
=,  eyre
|%
++  parse-url
  |=  a/$@(cord:purl purl)  ^-  purl
  ?^  a  a
  ~|  bad-url+a
  (rash a auri:de-purl:html)
::
++  add-query
  |=  {a/$@(@t purl) b/quay}  ^-  purl
  ?@  a  $(a (parse-url a))  :: deal with cord
  a(r (weld r.a b))
::
++  into-url
  |=  {a/$@(cord purl) b/(unit hart) c/(list (pair term knot))}
  ^-  purl
  ?@  a  $(a (parse-url a))  :: deal with cord
  %_  a
    p    ?^(b u.b p.a)
    q.q  (into-path q.q.a c)
  ==
::
++  into-path    ::  [/a/:b/c [%b 'foo']~] -> /a/foo/c
  =+  replacable=|=(a/knot `(unit term)`(rush a ;~(pfix col sym)))
  |=  {a/path b/(list (pair term knot))}  ^-  path
  ?~  a  ?~(b ~ ~|(unused-values+b !!))
  =+  (replacable i.a)
  ?~  -  [i.a $(a t.a)]  ::  literal value
  ?~  b  ~|(no-value+u !!)
  ?.  =(u p.i.b)  ~|(mismatch+[u p.i.b] !!)
  [q.i.b $(a t.a, b t.b)]
::
++  into-path-partial  ::  [/a/:b/c [d+'bar' b+'foo']~] -> [/a/foo/c [d+'bar']~]
  |=  {pax/path quy/quay}  ^-  {path quay}
  =+  ^=  inline                                        ::  required names
      %-  ~(gas in *(set term))
      (murn pax replacable:into-path)
  =^  inter  quy
    (skid quy |=({a/knot @} (~(has in inline) a)))
  [(into-path pax inter) quy]
--
