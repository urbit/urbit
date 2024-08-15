/@  task-diff
/-  feather-icons
:-  [%task-diff %$ %htmx]
|=  t=task-diff
|=  =bowl:neo
^-  manx
;div.loading
  =hx-get        "/hawk{(en-tape:pith:neo here.bowl)}"
  =hx-target     "closest .hawk"
  =hx-indicator  "closest .loader"
  =hx-swap       "innerHTML"
  =hx-trigger    "load"
  ;span.loading
  ;+  loading.feather-icons
  ==
==
