/@  node
/@  sail
:-  [%node %sail]
|=  nod=node
|^
  ^-  sail
  =/  code-el  (snag 1 c.nod)
  =/  code
    =/  raw=tape  (~(got by (malt a.g.code-el)) %value)
    ?:  =((rear raw) '\0a')  (crip (snip raw))
    (crip raw)
  =/  class-el  (snag 0 c.nod)
  =/  class  (crip (~(got by (malt a.g.class-el)) %value))
  [code class `(render-udon code)]
++  render-udon
  |=  code=@t
  ^-  (each manx tang)
  =/  newline  (trip 10)
  =/  udon
    :: format as udon document
    %-  crip
    ;:  welp
      ";>"  newline  newline
      (trip code)  newline
    ==
  =/  mul
    %-  mule
    |.
    !<  manx
    %+  slap  !>(..zuse)
    (ream udon)
  ?-  -.mul
    %.y  [%.y (manx p.mul)]
    %.n  [%.n (tang p.mul)]
  ==
--
