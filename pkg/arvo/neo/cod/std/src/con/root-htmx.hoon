/-  feather-icons
:-  [%root %$ %htmx]
|=  ~
|=  =bowl:neo
^-  manx
;div.p-page
  ;div.fc.g2
    ;*
    %+  turn
      ^-  (list [tape pith])
      :~
        ["home" #/home]
        ["settings" #/sky/settings]
        ["code" #/cod]
      ==
    |=  [=tape =pith]
    ;a.p2.b2.br1.bd1.hover.loader
      =href  "/neo/hawk{(en-tape:pith:neo (welp here.bowl pith))}"
      ;span.loaded: {tape}
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
==
