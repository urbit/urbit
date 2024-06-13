/@  accel-conf
/@  htmx
/-  feather-icons
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
    =hx-on-htmx-after-request  "$(this).closest('.top').find('.refresher').emit('accel-refresh');"
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
    ;+  add-dep
    ;+  deps
  ==
++  x  (rear `pith`(snip (snip here.bowl)))
++  y  (rear `pith`(snip here.bowl))
++  deps
  ^-  manx
  ?:  =(~ crew.conf)
    ;div.fr.p2.f3: no deps
  ;div.fc.g2.p2
    ;*
    %+  turn  ~(tap by crew.conf)
    |=  [=term =pith:neo]
    =/  tap  (trip term)
    ;div.fr.g3.ac
      ;button.bd1.br1.p-1.b1.hover.loader
        =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=del-dep"
        =hx-swap  "outerHTML"
        =hx-target  "find .loading"
        =tap  tap
        ;span.loaded: delete
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
      ;div.f3
        ; {tap}
      ==
      ;div.mono: {(en-tape:pith:neo pith)}
    ==
  ==
++  add-dep
  ;form.fr.js.hf.g2
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=add-dep"
    =hx-swap  "outerHTML"
    =hx-target  "find .loading"
    =morph-no-swap  ""
    =row  (scow %ud +:x)
    =col  (scow %ud +:y)
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
    ;button.br1.bd1.b1.hover.p-1.loader
      =type  "submit"
      ;span.loaded: add dep
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
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
