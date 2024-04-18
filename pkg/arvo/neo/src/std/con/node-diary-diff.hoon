/@  node
/@  diary-diff
:-  [%node %diary-diff]
|=  nod=node
^-  diary-diff
=/  head  (@tas (crip (~(got by (malt a.g.nod)) %head)))
=/  id-el  (snag 0 c.nod)
=/  text-el  (snag 1 c.nod)
=/  id
  %+  slav  %da
  (crip (~(got by (malt a.g.id-el)) %value))
=/  text
  (crip (~(got by (malt a.g.text-el)) %value))
[%put-entry id text]
