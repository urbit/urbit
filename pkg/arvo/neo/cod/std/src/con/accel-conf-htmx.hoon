/@  accel-conf
/@  htmx
:-  [%accel-conf %$ %htmx]
|=  conf=accel-conf
|=  =bowl:neo
|^  ^-  manx
::
;div.fc.trans-root.grow
  ;form.fc.grow
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=hoon"
    =hx-trigger  "input changed delay:0.4s from:[name='text'], input changed delay:0.4s from:[name='a']"
    =hx-swap  "none"
    =hx-target  "#code-spinner .loading"
    =hx-target-400  "#error-code-{id}"
    =hx-indicator  "#code-spinner"
    =row  (scow %ud +:x)
    =col  (scow %ud +:y)
    ;div.fc.border.grow.basis-half.wf
      ;+  code-input
      ;+  (spinner "code")
    ==
  ==
  ;div.fc.border
    ;+  conf-header
    ;div.fr
      ;div.fc.p2
        ;form.fr.js.hf
          =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=add-poke"
          =hx-swap  "none"
          =hx-indicator  "#code-spinner"
          =hx-target  "#code-spinner .loading"
          ;input
            =type  "text"
            =placeholder  "/{(scow %p our.bowl)}/shrub/to/poke"
            =name         "pith"
            =autocomplete  "off"
            =oninput      "this.setAttribute('value', this.value);"
            ;
          ==
          ;input
            =type  "text"
            =placeholder  "%some-type"
            =name         "stud"
            =autocomplete  "off"
            =oninput      "this.setAttribute('value', this.value);"
            ;
          ==
          ;button
            =type  "submit"
          ; Route poke
          ==
        ==
      ==
      ;div.fc.p2
        ::
        ;+  deps
        ;form.fr.js.hf
          =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=add-dep"
          =hx-swap  "none"
          =hx-target  "#conf-spinner .loading"
          =hx-target-400  "#error-add-{id}"
          =hx-indicator  "#conf-spinner"
          =row  (scow %ud +:x)
          =col  (scow %ud +:y)
          ;div.fc.grow.basis-half.wf
            ;div.fr
              ;input
                =type  "text"
                =placeholder  "name"
                =autocomplete  "off"
                =oninput  "this.setAttribute('value', this.value);"
                =name  "name"
                ;
              ==
              ;input
                =type  "text"
                =placeholder  "/{(scow %p our.bowl)}/demo/cell/5"
                =autocomplete  "off"
                =oninput  "this.setAttribute('value', this.value);"
                =name  "pith"
                ;
              ==
            ==
            ;button
              =type  "submit"
              ;  Add dep
            ==
            ;+  (spinner "conf")
          ==
        ==
      ==
    ==
  ==
==
++  id
  ^-  tape
  %-  zing
  %+  turn  (pout (tail here.bowl))
  |=  smeg=@ta
  %+  weld  "--"
  (trip smeg)
  ::
++  x  (rear (snip (snip here.bowl)))
++  y  (rear (snip here.bowl))
++  spinner
  |=  =tape
  ;div.b1.loader.p1.s-2.f2
    =id  (welp tape "-spinner")
    ;span.loaded: saved
    ;span.loading: ---
  ==
++  conf-header
  =/  pit=tape  (en-tape:pith:neo (snoc (snip here.bowl) %out))
  ;div.b1.border.fr.jb
    ;span.p1.mono.s-1: {pit}
    ;button.br1.border.b1.hover
      =style  "padding: 4px 8px;"
      =type  "button"
      =pith  pit
      =onclick  "navigator.clipboard.writeText(this.getAttribute('pith'));"
      ; copy path
    ==
  ==
++  code-input
  ;textarea#input.wf.p2.pre.mono.grow
    =name  "text"
    =placeholder  "code"
    =spellcheck  "false"
    =value  (trip hoon.conf)
    =oninput  "this.setAttribute('value', this.value);"
    ; {(trip hoon.conf)}
  ==
++  deps
  ^-  manx
  ?:  =(~ crew.conf)
    ;div.fr: No dependencies
  ;div.fc
    ;div.s1.p1: Dependencies
    ;*
    %+  turn  ~(tap by crew.conf)
    |=  [=term =pith:neo]
    =/  tap  (trip term)
    ;label.fr.p1
      ;div.border.wf
        =name  "name"
        ; {tap}
      ==
      ;label.border.wf: {(en-tape:pith:neo pith)}
      ;button.border
        =hx-post  "/neo/sky{(en-tape:pith:neo here.bowl)}?stud=del-dep"
        ; Delete
        ;label.hidden
          =name  "name"
          =value  tap
          ;
        ==
      ==
    ==
  ==
++  error
  |=  =tang
  ;div.pre.mono.p2
    ;*
    %+  turn  (scag 25 tang)
    |=  =tank
    ;span: {(of-wall:format (~(win re tank) 0 80))}
  ==
--
