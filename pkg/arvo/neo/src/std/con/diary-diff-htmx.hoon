/@  diary-diff
:-  [%diary-diff %htmx]
|=  =diary-diff
|=  =bowl:neo
?-  -.diary-diff
    %put-entry
  =/  tape  (trip txt.diary-diff)
  =/  subject-end  (fall (find [10]~ tape) 56)
  =/  subject  (scag subject-end tape)
  =/  id  (scow %da id.diary-diff)
  ;div.fr.g2
    ;a.p2.br1.grow.fc.g1.js.as.g2.b1.hover
      =href  "{(en-tape:pith:neo (weld /neo/hawk here.bowl))}/{id}"
      ;span: {id}
      ;span.bold: {subject}
    ==
    ;button.p2.br1.fr.g2.b1.hover.fc.ac.jc
      =onclick  "alert('not yet implemented. no tombstoning?')"
      ; X
    ==
  ==
    %del-entry
  ;div: deled
==
