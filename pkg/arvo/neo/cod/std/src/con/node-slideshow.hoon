/@  node
/@  slideshow
/-  manx-utils
/-  sh=slideshow
:-  [%node %$ %slideshow]
|=  nod=node
|^
  ^-  slideshow
  =/  mu  ~(. manx-utils nod)
  =/  code
    =/  raw=tape  (need (val:mu "code"))
    %-  crip
    ?:  =((lent raw) 0)  raw
    ?.  =((rear raw) '\0a')  raw
    (snip raw)
  =/  class  (vol:mu "classes")
  [code class `(render-slideshow:sh code) %both 0]
--
