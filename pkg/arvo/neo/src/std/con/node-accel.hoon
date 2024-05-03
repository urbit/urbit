/@  node
/@  accel-diff
:-  [%node %accel-diff]
|=  nod=node
^-  accel-diff
=/  input-el  (snag 0 c.nod)
=/  dep-el  (snag 1 c:(snag 1 c:(snag 1 c.nod)))
=/  ref-path
  %+  rust  (~(gut by (malt a.g.dep-el)) %value "")
  stap
:*  %new
    (slav %ud (crip (~(got by (malt a.g.nod)) %row)))
    (slav %ud (crip (~(got by (malt a.g.nod)) %col)))
    (crip (~(got by (malt a.g.input-el)) %value))
    ?~  ref-path  ~
    `(pave:neo u.ref-path)
==
