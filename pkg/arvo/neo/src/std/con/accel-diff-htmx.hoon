/@  accel-diff
:-  [%accel-diff %htmx]
|=  a=accel-diff
|=  =bowl:neo
;div.fr
;div.loading
  =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}"
  =hx-target  "table"
  =hx-select  "table"
  =hx-indicator  "closest .loader"
  =hx-swap  "morph"
  =hx-trigger  "load"
  ; +++
==
;div.loading
  =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}/{<row.a>}/{<column.a>}"
  =hx-target  "#config"
  =hx-select  "#config"
  =hx-indicator  "closest .loader"
  =hx-swap  "morph"
  =hx-trigger  "load"
  ; +++
==
==
