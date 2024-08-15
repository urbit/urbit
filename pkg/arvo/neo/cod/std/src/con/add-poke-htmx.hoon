/@  add-poke
/-  feather-icons
:-  [%add-poke %$ %htmx]
|=  add=add-poke
|=  =bowl:neo
;div.loading
  =hx-get  "/hawk{(en-tape:pith:neo here.bowl)}?no-save"
  =hx-trigger  "load"
  =hx-swap  "morph"
  =hx-target  "closest .accel-cell"
  =hx-select  ".accel-cell"
  ;+  loading.feather-icons
==
