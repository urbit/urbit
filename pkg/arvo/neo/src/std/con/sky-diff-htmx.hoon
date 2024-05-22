/@  sky-diff
/-  _/feather-icons
:-  [%sky-diff %htmx]
|=  diff=sky-diff
|=  =bowl:neo
^-  manx
;div.loading
  =hx-get  "/neo/sky"
  =hx-params  "none"
  =hx-indicator  "closest .loader"
  =hx-target  "#air"
  =hx-select  "#air"
  =hx-swap  "morph:outerHTML"
  =hx-trigger  "load"
  ;+  loading.feather-icons
==
