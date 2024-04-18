/@  txt
:-  [%txt %htmx]
|=  =txt
|=  =bowl:neo
;form.fc.p2
  =here  (en-tape:pith:neo here.bowl)
  =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=txt"
  =hx-trigger  "input changed delay:0.4s from:find textarea"
  =hx-swap  "none"
  ;textarea.wf.p2.border.br1.ma
    =style  "max-width: 650px;"
    =autocomplete  "off"
    =rows  "4"
    =name  "text"
    =oninput  "this.setAttribute('value', this.value);"
    ::=is  "multiline-input"
    ; {(trip (@t txt))}
  ==
==
