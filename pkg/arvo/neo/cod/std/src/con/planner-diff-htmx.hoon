/@  planner-diff
/-  feather-icons
:-  [%planner-diff %$ %htmx]
|=  =planner-diff
|=  =bowl:neo
;div.loading
  =hx-get  "/hawk{(en-tape:pith:neo here.bowl)}"
  =hx-target  "closest .planner"
  =hx-select  ".planner"
  =hx-indicator  "closest .loader"
  =hx-swap  "outerHTML"
  =hx-trigger  "load"
  ;+  loading.feather-icons
==
