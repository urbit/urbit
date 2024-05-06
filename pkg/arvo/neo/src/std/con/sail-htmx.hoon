/@  sail
:-  [%sail %htmx]
|=  =sail
|=  =bowl:neo
|^
  ;div.fc.relative.hf.scroll-hidden
    ;+  controls
    ;div.frw.js.as.scroll-y.hf
      =id  "tabs-{id}"
      ;+  editor
      ;+  viewer
      ;+  docs
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
  ;div.b1.p2.frw.g3.sticky.wf
    =style  "top:0; left: 0;"
    ;label.fr.g1
      ;input
        =name  "view"
        =type  "radio"
        =oninput  "$('#tabs-{id}').children().addClass('hidden');$('#editor-{id}').removeClass('hidden');"
        ;
      ==
      ;span: edit
    ==
    ;label.fr.g1
      ;input
        =name  "view"
        =type  "radio"
        =oninput  "$('#tabs-{id}').children().addClass('hidden');$('#viewer-{id}').removeClass('hidden');$('#editor-{id}').removeClass('hidden');"
        ;
      ==
      ;span: both
    ==
    ;label.fr.g1
      ;input
        =name  "view"
        =type  "radio"
        =checked  ""
        =oninput  "$('#tabs-{id}').children().addClass('hidden');$('#viewer-{id}').removeClass('hidden');"
        ;
      ==
      ;span: view
    ==
    ;label.fr.je.g1.grow
      ;input
        =name  "view"
        =type  "radio"
        =oninput  "$('#tabs-{id}').children().addClass('hidden');$('#docs-{id}').removeClass('hidden');"
        ;
      ==
      ;span: docs
    ==
    ;div.loader
      =id  "ind-{id}"
      ;span.loaded.f3: saved
      ;span.loading: ---
    ==
  ==
++  editor
  ;form.fc.hidden.grow.basis-half.border.scroll-y
    =id  "editor-{id}"
    =style  "min-width: 300px; height: 100%;"
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=sail"
    =hx-swap  "innerHTML"
    =hx-select  "main > div"
    =hx-target  "#viewer-{id}"
    =hx-trigger  "input changed delay:0.4s from:find textarea, input changed delay:0.4s from:find input"
    =hx-indicator  "#ind-{id}"
    ;input.p2.mono
      =style  "border-bottom: 1px solid var(--f3);"
      =name  "classes"
      =placeholder  "prose p3"
      =type  "text"
      =autocomplete  "off"
      =value  (trip class.sail)
      =oninput  "$(this).attr('value', this.value);"
      ;
    ==
    ;textarea.p2.pre.mono.scroll-x.grow
      =name  "code"
      =oninput  "this.setAttribute('value', this.value);"
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
    ;div.pre.numbered.mono
      ;span: ;>
      ;span;
      ;*
      %+  turn  (to-wain:format code.sail)
      |=  t=@t
      ;span: {(trip t)}
    ==
  ==
++  viewer
  ;main.grow.border.basis-half.scroll-x.scroll-y
    =id  "viewer-{id}"
    =style  "min-width: 300px; height: 100%;"
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
::  XX import from source file instead of copying
++  docs
;div.grow.border.basis-half.scroll-x.scroll-y.hidden.prose.p3
=id  "docs-{id}"
=style  "min-width: 300px; height: 100%;"

# CSS Utility Classes

```

.hidden, .folded {
  display: none !important;
}

.wfc {
  width: fit-content;
}
.wf {
  width: 100%;
}
.mw-page {
  max-width: 650px;
}
.hf {
  height: 100%;
}
.hfc {
  height: fit-content;
}

.mono {
  font-variation-settings: "xtab" 500;
  font-size: 0.8em;
}
.bold {
  font-weight: bold;
}
.strike {
  text-decoration: line-through;
}
.pre {
  white-space: pre;
}
.pre-line {
  white-space: pre-line;
}

.tl {
  text-align: left;
}
.tc {
  text-align: center;
}
.tr {
  text-align: right;
}


.fc {
  display: flex;
  flex-direction: column;
}
.fcr {
  display: flex;
  flex-direction: column-reverse;
}
.fr {
  display: flex;
  flex-direction: row;
}
.frw {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
}

.jc {
  justify-content: center;
}
.jb {
  justify-content: space-between;
}
.ja {
  justify-content: space-around;
}
.js {
  justify-content: start;
}
.je {
  justify-content: end;
}
.js {
  justify-content: start;
}
.as {
  align-items: start;
}
.af {
  align-items: stretch;
}
.ae {
  align-items: end;
}
.ac {
  align-items: center;
}

.basis-full {
  flex-basis: 100%;
}
.basis-half {
  flex-basis: 50%;
  flex-shrink: 0;
}
.basis-none {
  flex-basis: 0%;
  flex-shrink: 1;
}
.shrink-0 {
  flex-shrink: 0;
}
.grow {
  flex-grow: 1;
}

.g1 {
  gap: 4px;
}
.g2 {
  gap: 8px;
}
.g3 {
  gap: 12px;
}
.g4 {
  gap: 16px;
}
.g5 {
  gap: 20px;
}
.g6 {
  gap: 24px;
}
.g7 {
  gap: 28px;
}

.relative {
  position: relative;
}
.absolute {
  position: absolute;
}
.fixed {
  position: fixed;
}
.sticky {
  position: sticky;
}
.z-2 {
  z-index: -20;
}
.z-1 {
  z-index: -10;
}
.z0 {
  z-index: 0;
}
.z1 {
  z-index: 10;
}
.z2 {
  z-index: 20;
}

.block {
  display: block;
}

.p-1 {
  padding: 0px 4px;
}
.p0 {
  padding: 0;
}
.p1 {
  padding: 4px;
}
.p2 {
  padding: 8px;
}
.p3 {
  padding: 12px;
}
.p4 {
  padding: 16px;
}
.p-page {
  padding-top: 30px;
  padding-bottom: 200px;
  padding-left: 12px;
  padding-right: 12px;
}
.ma {
  margin: auto;
}

.scroll-y {
  overflow-y: auto;
}
.scroll-x {
  overflow-x: auto;
}
.scroll-hidden {
  overflow: hidden;
}

.break {
  word-break: break-word;
}
.action {
  touch-action: manipulation;
}

.f1 {
  color: var(--f1);
}
.f2 {
  color: var(--f2);
}
.f3 {
  color: var(--f3);
}
.f-success {
  color: var(--f-success);
}
.f-error {
  color: var(--f-error);
}
.b0 {
  background-color: var(--b0);
}
.b1 {
  background-color: var(--b1);
}
.b2 {
  background-color: var(--b2);
}
.b3 {
  background-color: var(--b3);
}
.b-success {
  background-color: var(--b-success);
}
.b-error {
  background-color: var(--b-error);
}

.s-2 {
  font-size: 0.7em;
  }
.s-1 {
  font-size: 0.85em;
}
.s0 {
  font-size: 1em;
}
.s1 {
  font-size: 1.15em;
}
.s2 {
  font-size: 1.3em;
}
.s3 {
  font-size: 1.45em;
}
.s4 {
  font-size: 1.6em;
}

.border {
  border: 1.2px solid var(--f3);
}
.border-2 {
  border: 1px solid var(--f4);
}
.br1 {
  border-radius: 3px;
}
.br2 {
  border-radius: 6px;
}

.hover:hover {
  filter: invert(20%);
}

.toggled {
  filter: invert(100%);
}
.hover.toggled:hover {
  filter: invert(100%);
}

.active,
.selected {
  filter: invert(10%);
}
.hover.active:hover,
.hover.selected:hover {
  filter: invert(25%);
}

```

# The .prose Class

```

.prose h1 {
  font-size: 1.45rem;
  margin: 1rem 0;
}
.prose h2 {
  font-size: 1.3rem;
  margin: 1rem 0;
}
.prose h3 {
  font-size: 1.15rem;
  margin: 1rem 0;
}
.prose h1, .prose h2, .prose h3 {
  font-weight: bold;
}
.prose p {
  margin-bottom: 1rem;
  line-height: 1.3;
}
.prose img {
  max-width: 100%;
  display: block;
  max-height: 350px;
}
.prose details {
  margin-bottom: 1rem;
}
.prose a {
  text-decoration: underline;
}
.prose mono {
  font-size: 1em;
}
.prose pre {
  font-variation-settings: "xtab" 500;
  font-size: 0.8em;
  overflow-x: auto;
  width: 100%;
  display: block;
  padding: 8px;
}
.prose code {
  white-space: pre;
  overflow-x: auto;
  width: 100%;
  display: block;
  padding: 8px;
}
.prose ul,
.prose ol {
  padding-left: 19px;
  line-height: 1.2;
  margin-bottom: 1rem;
}
.prose ul p,
.prose ol p {
  margin-bottom: 0;
  line-height: 1.4;
}
.prose ul ul,
.prose ol ul,
.prose ul ol,
.prose ol ol {
  margin-bottom: 0;
}
```

==
--
