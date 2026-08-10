|=  [now=@da q=(list quac:dill)]
^-  (list (trel (list @t) @da @ud))
=|  out=(list (trel (list @t) @da @ud))
|-  ^+  out
?~  q  (flop out)
%=    $
    q  t.q
    out
  =|  pax=(list @t)
  =|  top=?
  |-
  =?  out  top
    :_  out
    [(flop [name.i.q pax]) now size.i.q]
  ?~  quacs.i.q
    out
  %=  $
    i.q  i.q(quacs t.quacs.i.q)
    top  |
    out  $(i.q i.quacs.i.q, top &, pax [name.i.q pax])
  ==
==
