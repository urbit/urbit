/@  accel-conf
/@  htmx
:-  [%accel-conf %$ %htmx]
|=  conf=accel-conf
|=  =bowl:neo
|^
::
;div.fr
  ;form.fr.af.js.hf
    =hx-post  "/neo/hawk/{(en-tape:pith:neo here.bowl)}?stud=hoon"
    =hx-trigger  "input changed delay:0.4s from:find textarea, input changed delay:0.4s from:[name='a']"
    =hx-swap  "outerHTML"
    =hx-target  "#spinner .loading"
    =hx-target-400  "#error-{id}"
    =hx-indicator  "#spinner"
    =row  (scow %ud +:x)
    =col  (scow %ud +:y)
    ;div.fc.border.grow.basis-half.wf
      ;+  conf-header
      ;+  input
    ==
  ==
  ;div.fc
    ;h3: Add 
    ;form.fr.af.js.hf
      =hx-post  "/neo/hawk/{(en-tape:pith:neo here.bowl)}?stud=deps"
      =hx-trigger  "input changed delay:0.4s from:find textarea, input changed delay:0.4s from:[name='a']"
      =hx-swap  "outerHTML"
      =hx-target  "#spinner .loading"
      =hx-target-400  "#error-{id}"
      =hx-indicator  "#spinner"
      =row  (scow %ud +:x)
      =col  (scow %ud +:y)
      ;div.fc.border.grow.basis-half.wf
        ;+  conf-header
        ;+  input
      ==
    ==

    ;h3: Dependencies 
    ;div#config.grow.border.basis-half.fc.scroll-x.scroll-y
      ;+  spinner
      ;*  
      %+  turn  ~(tap by crew.conf)
      |=  [=term =pith:neo]
      (deps (trip term) pith)
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
  ;div#spinner.b1.loader.p1.s-2.f2
    ;span.loaded: saved
    ;span.loading: ---
  ==
++  conf-header
  ;div.b1.border.fr.ac.jb.p1
    ;span.p1.mono.s-1: /{(scow %ud +:x)}/{(scow %ud +:y)}
    ;button.br1.border.b1.hover
      =style  "padding: 4px 8px;"
      =type  "button"
      =pith  "/{(scow %p our.bowl)}{(en-tape:pith:neo here.bowl)}"
      =onclick  "navigator.clipboard.writeText(this.getAttribute('pith'));"
      ; copy path
    ==
  ==
++  input
  ;textarea#input.wf.p2.pre.mono.grow
    =name  "code"
    =placeholder  "code"
    =spellcheck  "false"
    =value  (trip hoon.conf)
    =oninput  "this.setAttribute('value', this.value);"
    ; {(trip hoon.conf)}
  ==
++  deps
  |=  [term=tape =pith:neo]
  ;label.fc.p1
    ;input.border.wf
      =value  term
      =placeholder  "var-name"
      =autocomplete  "off"
      =oninput  "this.setAttribute('value', this.value);"
      =name  term
      ;
    ==
    ;input.border.wf
      =placeholder  "/{(scow %p our.bowl)}/path/to/dep"
      =value  (en-tape:pith:neo pith)
      =autocomplete  "off"
      =oninput  "this.setAttribute('value', this.value);"
      =name  :(welp term "-" "value")
      ; {(en-tape:pith:neo pith)}
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
