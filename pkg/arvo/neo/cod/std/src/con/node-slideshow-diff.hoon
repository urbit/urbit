/@  node
/@  slideshow-diff
/-  manx-utils
:-  [%node %$ %slideshow-diff]
|=  nod=node
^-  slideshow-diff
=*  mu  ~(. manx-utils nod)
=/  head  (got:mu %head)
?+    head  !!
    %mode
  :-  %mode
  %+  snag  0
  %+  murn  c.nod
  |=  =manx
  =/  ribs  (malt a.g.manx)
  =/  cls  (fall (~(get by ribs) %class) "")
  ?~  x=(find "toggled" cls)  ~
  :-  ~
  %-  ?(%edit %both %preview %present)
  (crip (~(got by ribs) %mode))
::
    %slide
  [%slide (slav %ud (got:mu %current))]
==
