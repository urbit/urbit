/@  diary
:-  [%diary %htmx]
|=  dia=diary
|=  =bowl:neo
^-  manx
|^  shell
++  form-put-entry
  ::
  ;form.fc.g2
    =hx-post  "{(en-tape:pith:neo :(weld /neo/hawk here.bowl))}?stud=diary-diff"
    =hx-on-submit  "this.reset()"
    =hx-target  "this"
    =hx-swap  "afterend"
    =head  "put-entry"
    ;date-now(name "id");
    ;textarea.p2.border.br1
      =name  "text"
      =placeholder  ". . . text"
      =oninput  "this.setAttribute('value', this.value)"
      =rows  "4"
      =required  ""
      =autocomplete  "off"
      ;
    ==
    ;button.p2.b1.br1.wfc.hover
      ; create
    ==
  ==
  ::
++  link-entry
  ::
  |=  [pax=pith =pail:neo]
  =/  tape  (trip !<(@t q.pail))
  =/  subject-end  (fall (find [10]~ tape) 56)
  =/  subject  (scag subject-end tape)
  =/  id  (trip (snag 0 (pout pax)))
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
  ::
++  shell
  ::
  ;div.p2
    =label  "Diary"
    ;div.ma.fc.g2
      =style  "max-width: 650px;"
      ;+  form-put-entry
      ;*
      %+  turn
        %+  sort  ~(tap by kids.bowl)
        |=  [a=[=pith *] b=[=pith *]]
        (gth ->.pith.a ->.pith.b)
      link-entry
    ==
  ==
--
