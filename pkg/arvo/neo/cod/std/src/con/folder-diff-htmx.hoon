/@  folder-diff
/-  feather-icons
:-  [%folder-diff %$ %htmx]
|=  =folder-diff
|=  =bowl:neo
;div.loading
  =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}"
  =hx-target  "closest .hawk"
  =hx-indicator  "closest .loader"
  =hx-swap  "innerHTML"
  =hx-trigger  "load"
  ;+  loading.feather-icons
==
