/@  circle-diff
:-  [%circle-diff %htmx]
|=  circle-diff
|=  =bowl:neo
;div.hidden
  =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}"
  =hx-target  "closest ha-wk"
  =hx-swap  "innerHTML"
  =hx-trigger  "load"
  ; +++
==
