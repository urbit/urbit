/@  task-diff
/-  feather-icons
:-  [%task-diff %$ %htmx]
|=  t=task-diff
|=  =bowl:neo
;div.loading
  =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}"
  =hx-target  "closest .hawk"
  =hx-indicator  "closest .loader"
  =hx-swap  "innerHTML"
  =hx-trigger  "load"
  ;span.loading
  ;+  loading.feather-icons
  ==
==
