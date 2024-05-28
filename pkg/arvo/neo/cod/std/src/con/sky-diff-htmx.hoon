/@  sky-diff
/-  feather-icons
:-  [%sky-diff %$ %htmx]
|=  diff=sky-diff
|=  =bowl:neo
^-  manx
;div.loading
  =hx-get  "/neo/sky"
  =hx-params  "none"
  =hx-indicator  "closest .loader"
  =hx-target  "#air"
  =hx-select  "#air"
  =hx-swap  "outerHTML"
  =hx-trigger  "load once"
  ;+  loading.feather-icons
==
