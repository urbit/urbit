/@  accel-cell
:-  [%accel-cell %htmx]
|=  =accel-cell
|=  =bowl:neo
|^
  ;form.fr.af.js.hf
    =hx-post  "/neo/hawk{(en-tape:pith:neo (snip (snip here.bowl)))}?stud=accel-diff"
    =hx-trigger  "input changed delay:0.4s from:find textarea"
    =hx-swap  "outerHTML"
    =hx-target  "#spinner .loading"
    =hx-indicator  "#spinner"
    =row  (scow %ud +:x)
    =col  (scow %ud +:y)
    ;+  input
    ;div#config.grow.border.basis-half.fc.scroll-x
      ;+  spinner
      ;+  depsa
      ;+  depsb
      ;+  output
    ==
  ==
++  x  (rear (snip here.bowl))
++  y  (rear here.bowl)
++  spinner
  ;div#spinner.b1.loader.p1.s-2.f2
    ;span.loaded: saved
    ;span.loading: ---
  ==
++  input
  ;textarea#input.border.grow.basis-half.wf.p2.pre.mono
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
      =placeholder  "/path/to/dep"
      =value  ?~(refa.accel-cell "" (en-tape:pith:neo u.refa.accel-cell))
      =oninput  "this.setAttribute('value', this.value);"
      =name  "a"
      ;
    ==
  ==
++  depsb
  ;label.fc.p1
    ;span.f3.s-2.p1: b
    ;input.border.wf
      =placeholder  "/path/to/dep"
      =value  ?~(refb.accel-cell "" (en-tape:pith:neo u.refb.accel-cell))
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
