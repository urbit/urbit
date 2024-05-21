/@  sandbox-diff
:-  [%sandbox-diff %htmx]
|=  =sandbox-diff
|=  =bowl:neo
;div.loading
  =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}"
  =hx-target  "closest ha-wk"
  =hx-indicator  "closest .loader"
  =hx-swap  "innerHTML"
  =hx-trigger  "load"
  ; +++
==