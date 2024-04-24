/@  sail
:-  [%sail %htmx]
|=  =sail
|=  =bowl:neo
|^
  ;div.fc.relative.hf
    ;+  controls
    ;div.frw.js.as.scroll-hidden.hf
      ;+  editor
      ;+  viewer
    ==
  ==
++  id
  ^-  tape
  %-  zing
  %+  turn  (pout here.bowl)
  |=  smeg=@ta
  %+  weld  "--"
  (trip smeg)
++  controls
  ;div.b1.p2.fr.g3.sticky.wf
    =style  "top:0; left: 0;"
    ;label.fr.g1
      ;input
        =name  "view"
        =type  "radio"
        =oninput  "$('#editor-{id}').removeClass('hidden');$('#viewer-{id}').addClass('hidden');"
        ;
      ==
      ;span: edit
    ==
    ;label.fr.g1
      ;input
        =name  "view"
        =type  "radio"
        =oninput  "$('#editor-{id}').parent().children().removeClass('hidden');"
        ;
      ==
      ;span: both
    ==
    ;label.fr.g1
      ;input
        =name  "view"
        =type  "radio"
        =checked  ""
        =oninput  "$('#editor-{id}').addClass('hidden');$('#viewer-{id}').removeClass('hidden');"
        ;
      ==
      ;span: view
    ==
    ;div.grow;
    ;div.loader
      =id  "ind-{id}"
      ;span.loaded.f3: saved
      ;span.loading: ---
    ==
  ==
++  editor
  ;form.fc.hidden.grow.basis-half.border.scroll-y
    =id  "editor-{id}"
    =style  "min-width: 300px;"
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=sail"
    =hx-swap  "innerHTML"
    =hx-select  "main > div"
    =hx-target  "#viewer-{id}"
    =hx-trigger  "input changed delay:0.4s from:find textarea, input changed delay:0.4s from:find input"
    =hx-indicator  "#ind-{id}"
    ;input.p2.mono
      =style  "border-bottom: 1px solid var(--f3);"
      =name  "class"
      =placeholder  "prose p3"
      =type  "text"
      =autocomplete  "off"
      =value  (trip class.sail)
      =oninput  "$(this).attr('value', this.value);"
      ;
    ==
    ;textarea.p2.pre.mono.scroll-x
      =name  "code"
      =oninput  "this.setAttribute('value', this.value); this.rows = this.value.split('\\n').length + 0;"
      =rows  "{<(add (lent (fand ~[10] (trip code.sail))) 0)>}"
      =spellcheck  "false"
      =placeholder  "# new document"
      ; {(trip code.sail)}
    ==
  ==
++  error
  |=  =tang
  ;div.fc.g3.p3
    ;div.pre.mono
      ;*
      %+  turn  (scag 25 tang)
      |=  =tank
      ;span: {(of-wall:format (~(win re tank) 0 80))}
    ==
  ==
++  viewer
  ;main.grow.basis-half.scroll-x.scroll-y
    =id  "viewer-{id}"
    =style  "min-width: 300px; min-height: 100%;"
    ;+
    ?~  result.sail
      ;div.prose.p3
        ;h1: nothing rendered
        ;p: edit the sail to begin rendering
      ==
    =/  res  (need result.sail)
    ?-  -.res
      %.n  (error +.res)
        %.y
      =-  -(a.g [[%class (trip class.sail)] a.g.-])
      ^-  manx
      +.res
    ==
  ==
--
