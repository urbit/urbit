/@  folder
/-  feather-icons
:-  [%folder %$ %htmx]
|=  =folder
|=  =bowl:neo
|^
  ;div.wf.hf.relative
    ;div.p3.fc.g5.ma.mw-page
      ;+  form
      ;+  shrubs
    ==
  ==
::
++  form
  ;form.fr.g1.af
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=folder-diff"
    =hx-swap  "outerHTML"
    =hx-target  "find .loading"
    =head  "make"
    ;input.p2.bd1.br2.grow.fr
      =style  "width: 0;"
      =oninput  "$(this).attr('value', this.value)"
      =autocomplete  "off"
      =name  "name"
      =type  "text"
      =required  ""
      =pattern  (trip '[a-z]{1}[a-z0-9\\-]+')
      =title  "lowercase and heps"
      =placeholder  "file-or-folder-name"
      ;
    ==
    ;select.f1.b0.bd1.br2.p1
      =oninput  "$(this).attr('value', this.value)"
      =autocomplete  "off"
      =value  (trip %accel)
      =required  ""
      =name  "stud"
      ;option.f3.o4(value ""): type
      ;*  %+  turn
            ^-  (list @tas)
            :~  %folder
                %accel
                %circle
                %diary
                %iframe
                %task
                %txt
                %sail
            ==
          |=  t=@tas
          ^-  manx
          ;option
            =value  (trip t)
            ; {(trip t)}
          ==
      ;
    ==
    ;button.loader.p2.p1.hover.bd1.br2.b1
      ;span.loaded: create
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
::
++  shrubs
  ;div.fc.g2
    ::;+  ?.  =(0 (lent (~(tap of kids.bowl) /)))  ;/("")
    ::;div.p5.f3.fc.jc.ac: none yet
    ;*
    %+  turn  folder
    |=  name=@tas
    =/  pith  ~[name]
    =/  =pail:neo  pail:(~(got of:neo kids.bowl) pith)
    =/  =stud:neo  p.pail
    =/  =path  (pout (welp here.bowl pith))
    =/  label
      ?@  stud
        (trip stud)
      (trip mark.stud)
    ;div.fr.g2
      ;a.b1.br1.hover.p3.s1.bd1.fr.ac.jb.loader.grow
        =hx-target  "closest .hawk"
        =href  (trip (spat ['neo' 'hawk' path]))
        ;span.loaded: {(trip (snag 0 (pout pith)))}
        ;span.loading
          ;+  loading.feather-icons
        ==
        ;span.s-1.f2.loaded: {label}
      ==
      ;button.b1.br1.hover.p3.s1.bd1.fr.ac.jb.loader
        =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=folder-diff"
        =hx-swap  "outerHTML"
        =hx-target  "find .loading"
        =head  "tomb"
        =smeg  (trip name)
        ;span.loaded
          ;+  close.feather-icons
        ==
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ==
--
