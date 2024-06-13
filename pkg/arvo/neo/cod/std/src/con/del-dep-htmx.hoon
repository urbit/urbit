/@  del-dep
:-  [%del-dep %$ %htmx]
|=  dep=del-dep
|=  =bowl:neo
;div
  =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}?no-save"
  =hx-trigger  "load"
  =hx-swap  "morph"
  =hx-target  "closest .accel-cell"
  =hx-select  ".accel-cell"
  ;
==
