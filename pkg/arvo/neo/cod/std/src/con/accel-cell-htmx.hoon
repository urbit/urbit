/@  accel-cell
:-  [%accel-cell %$ %htmx]
|=  =accel-cell
|=  =bowl:neo
|^
  ;form.fr.af.js.hf
    =hx-post  "/neo/hawk{(en-tape:pith:neo (snip (snip here.bowl)))}?stud=accel-diff"
    =hx-trigger  "input changed delay:0.4s from:find textarea, input changed delay:0.4s from:[name='a']"
    =hx-swap  "outerHTML"
    =hx-target  "#spinner .loading"
    =hx-target-400  "#error-{id}"
    =hx-indicator  "#spinner"
    =row  (scow %ud +:x)
    =col  (scow %ud +:y)
    ;div.fc.border.grow.basis-half.wf
      ;+  cell-header
      ;+  input
    ==
    ;div#config.grow.border.basis-half.fc.scroll-x.scroll-y
      ;+  spinner
      ;+  depsa
      ;+  depsb
      ;+  output
    ==
  ==
++  id
  ^-  tape
  %-  zing
  %+  turn  (pout here.bowl)
  |=  smeg=@ta
  %+  weld  "--"
  (trip smeg)
  ::
++  x  (rear (snip here.bowl))
++  y  (rear here.bowl)
++  spinner
  ;div#spinner.b1.loader.p1.s-2.f2
    ;span.loaded: saved
    ;span.loading: ---
  ==
++  cell-header
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
    =value  (trip code.accel-cell)
    =oninput  "this.setAttribute('value', this.value);"
    ; {(trip code.accel-cell)}
  ==
++  depsa
  ;label.fc.p1
    ;span.f3.s-2.p1: a
    ;input.border.wf
      =placeholder  "/{(scow %p our.bowl)}/path/to/dep"
      =value  ?~(refa.accel-cell "" (en-tape:pith:neo u.refa.accel-cell))
      =autocomplete  "off"
      =oninput  "this.setAttribute('value', this.value);"
      =name  "a"
      ;
    ==
  ==
++  depsb
  ;label.fc.p1
    ;span.f3.s-2.p1: b
    ;input.border.wf
      =placeholder  "/{(scow %p our.bowl)}/path/to/dep"
      =value  ?~(refb.accel-cell "" (en-tape:pith:neo u.refb.accel-cell))
      =autocomplete  "off"
      =oninput  "this.setAttribute('value', this.value);"
      =name  "b"
      ;
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
++  output
  ;div#output.grow.scroll-x.scroll-y.p1
    ;div.f2.s-2: result
    ;div(id "error-{id}");
    ;+
    ?~  result.accel-cell
      ;div.prose.p3
        ;h1: nothing yet
      ==
    =/  res  (need result.accel-cell)
    ?-  -.res
      %.n  (error +.res)
        %.y
      ;div.pre.mono.p2
        ;+
        ;/  (of-wall:format (~(win re (sell +.res)) 0 80))
      ==
    ==
  ==
--
