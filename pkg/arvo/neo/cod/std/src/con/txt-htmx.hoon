/@  txt
/-  feather-icons
:-  [%txt %$ %htmx]
|=  =txt
|=  =bowl:neo
;form.fc.p2.wf.hf.relative
  =here  (en-tape:pith:neo here.bowl)
  =hx-post  "/hawk{(en-tape:pith:neo here.bowl)}?stud=txt&refresh"
  =hx-trigger  "input changed delay:0.4s from:find textarea"
  =hx-swap  "none"
  ;div.htmx-indicator.absolute
    =style  "top: 15px; right: 15px"
    ;+  loading.feather-icons
  ==
  ;textarea.wf.p2.bd1.br1.grow
    =autocomplete  "off"
    =rows  "4"
    =name  "text"
    =oninput  "this.setAttribute('value', this.value);"
    ; {(trip (@t txt))}
  ==
==
