/@  node
/@  sail
/-  _/manx-utils
:-  [%node %sail]
|=  nod=node
|^
  ^-  sail
  =/  code
    =/  raw=tape  (need (~(value manx-utils nod) "code"))
    ?:  =((rear raw) '\0a')  (crip (snip raw))
    (crip raw)
  =/  class  (crip (need (~(value manx-utils nod) "classes")))
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
    %+  slap
      ;:  slop
        !>(manx-utils=manx-utils)
        !>(..zuse)
      ==
    (ream udon)
  ?-  -.mul
    %.y  [%.y (manx p.mul)]
    %.n  [%.n (tang p.mul)]
  ==
--
