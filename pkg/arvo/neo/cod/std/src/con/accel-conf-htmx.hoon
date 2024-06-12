/@  accel-conf
/@  htmx
:-  [%accel-conf %$ %htmx]
|=  conf=accel-conf
|=  =bowl:neo
|^
  ;div.accel-cell.wf.hf.fc
    ;+  controls
    ;div.tabs.fc.grow
      ;div.tab.in.p2.fc.grow
        =morph-retain  "class"
        ;+  in
      ==
      ;div.tab.poke.p2.hidden.hf
        =morph-retain  "class"
        =ok-ok  "poke"
        ;+  poke
      ==
      ;div.tab.ref.p2.hidden.hf
        =morph-retain  "class"
        =ok-ok  "ref"
        ;+  ref
      ==
    ==
    ;+  script
  ==
::
++  out-path  (snoc (snip here.bowl) %out)
++  print-iota
  |=  =iota
  ?@(iota (trip iota) (scow iota))
++  controls
  ;div.p-1.fr.g2.ac.js
    ;button.p-1.br1.bd1.b1.hover.toggled
      =onclick  "accelSwitchTab(this, 'in');"
      =morph-retain  "class"
      ; in
    ==
    ;button.p-1.br1.bd1.b1.hover
      =onclick  "accelSwitchTab(this, 'poke');"
      =morph-retain  "class"
      ; poke
    ==
    ;button.p-1.br1.bd1.b1.hover
      =onclick  "accelSwitchTab(this, 'ref');"
      =morph-retain  "class"
      ; ref
    ==
  ==
++  in
  ;form.fc.grow
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=hoon"
    =hx-trigger  "input changed delay:0.4s from:find textarea"
    =hx-swap  "none"
    ;textarea
      =class  "p2 bd1 br1 scroll-x pre mono wf grow"
      =name  "text"
      =placeholder  "code"
      =spellcheck  "false"
      =autocomplete  "off"
      =value  (trip hoon.conf)
      =oninput  "this.setAttribute('value', this.value);"
      ; {(trip hoon.conf)}
    ==
  ==
++  poke
  ;div
    ; XX poke
  ==
++  ref
  ;div
    =morph-no-swap  ""
    ;+  add-dep
    ;+  deps
  ==
++  old
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
++  x  (rear `pith`(snip (snip here.bowl)))
++  y  (rear `pith`(snip here.bowl))
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
    ;div.fr: no deps
  ;div.fc.g2.p2
    ;*
    %+  turn  ~(tap by crew.conf)
    |=  [=term =pith:neo]
    =/  tap  (trip term)
    ;div.fr.g3.ac
      ;button.bd1.br1.p-1.b1.hover
        =hx-post  "/neo/sky{(en-tape:pith:neo here.bowl)}?stud=del-dep"
        ; delete
        ;label.hidden
          =name  "name"
          =value  tap
          ;
        ==
      ==
      ;div.f3
        =name  "name"
        ; {tap}
      ==
      ;div.mono: {(en-tape:pith:neo pith)}
    ==
  ==
++  add-dep
  ;form.fr.js.hf
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=add-dep"
    =hx-swap  "none"
    =row  (scow %ud +:x)
    =col  (scow %ud +:y)
    ;div.fr.g1
      ;input.p-1.br1.bd1
        =type  "text"
        =placeholder  "name"
        =autocomplete  "off"
        =oninput  "this.setAttribute('value', this.value);"
        =name  "name"
        =required  ""
        ;
      ==
      ;input.p-1.br1.bd1.grow
        =type  "text"
        =placeholder  "/{(scow %p our.bowl)}/demo/cell/5"
        =autocomplete  "off"
        =oninput  "this.setAttribute('value', this.value);"
        =required  ""
        =name  "pith"
        ;
      ==
    ==
    ;button.br1.bd1.b1.hover.p-1
      =type  "submit"
      ;  add dep
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
++  script
  ;script
    ;+  ;/  %-  trip
    '''
    function accelSwitchTab(el, name) {
      $(el).siblings().removeClass('toggled');
      $(el).addClass('toggled');
      let tabs = $(el).closest('.accel-cell').find('.tab');
      tabs.addClass('hidden');
      tabs.filter('.'+name).removeClass('hidden');
    }
    '''
  ==
--
