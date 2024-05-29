/@  diary
/-  feather-icons
:-  [%diary %$ %htmx]
|=  dia=diary
|=  =bowl:neo
^-  manx
|^
  ;div.p2
    =label  "Diary"
    ;div.ma.fc.g2
      =style  "max-width: 650px;"
      ;+  form-put-entry
      ;*
      %+  turn  ~
        ::  %+  sort  ~(tap of:neo kids.bowl)
        ::  |=  [a=[=pith *] b=[=pith *]]
        ::  (gth ->.pith.a ->.pith.b)
      link-entry
    ==
  ==
++  form-put-entry
  ::
  ;form.fc.g2
    =style  "margin-bottom: 30px;"
    =hx-post  "{(en-tape:pith:neo :(weld /neo/hawk here.bowl))}?stud=diary-diff"
    =hx-on-submit  "this.reset()"
    =hx-target  "this"
    =hx-swap  "afterend"
    =head  "put-entry"
    ;date-now(name "id");
    ;textarea.p2.bd1.br1
      =name  "text"
      =placeholder  "today, i ..."
      =oninput  "this.setAttribute('value', this.value)"
      =rows  "5"
      =required  ""
      =autocomplete  "off"
      ;
    ==
    ;button.p2.b1.br1.bd1.wfc.hover.loader
      ;span.loaded.s2: create
      ;span.loading
        ;+  loading.feather-icons
      ==
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
    ;a.p2.br1.grow.b1.hover.loader
      =href  "{(en-tape:pith:neo (weld /neo/hawk here.bowl))}/{id}"
      ;div.loaded.fc.g1.js.as.g2
        ;span.f3: {(pretty-date `@da`->:pax)}
        ;span.bold: {subject}
      ==
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
    ;button.p2.br1.fr.g2.b1.hover.fc.ac.jc.loader
      =onclick  "alert('not yet implemented. no tombstoning?')"
      ;span.loaded
        ;+  close.feather-icons
      ==
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
++  pretty-date
  |=  date=@da
  ^-  tape
  =/  d  (yore date)
  "{(y-co:co y:d)}-{(y-co:co m:d)}-{(y-co:co d:t:d)}"
--
