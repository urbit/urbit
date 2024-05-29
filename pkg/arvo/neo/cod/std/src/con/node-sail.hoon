/@  node
/@  sail
/-  manx-utils
:-  [%node %$ %sail]
|=  nod=node
|^
  ^-  sail
  =/  mu  ~(. manx-utils nod)
  =/  code
    =/  raw=tape  (need (val:mu "code"))
    ?:  =((rear raw) '\0a')  (crip (snip raw))
    (crip raw)
  =/  class  (vol:mu "classes")
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
        !>(..zuse)
        !>(manx-utils=manx-utils)
      ==
    (ream udon)
  ?-  -.mul
    %.y  [%.y (manx p.mul)]
    %.n  [%.n (tang p.mul)]
  ==
--
