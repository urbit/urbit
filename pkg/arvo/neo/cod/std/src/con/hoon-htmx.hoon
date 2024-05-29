/@  htmx
:-  [%hoon %$ %htmx]
|=  hon=@t
|=  =bowl:neo
=/  =name:neo
  [our here]:bowl
=/  =file:ford:neo
  (scan (trip hon) (rein:ford:neo name))
=/  src=wain
  (to-wain:format hon)
^-  manx
=<  apex
|%
::  XX: non-std
++  post-href
  |=  =post:neo
  ^-  path
  ?>  ?=(@ q.post)
  =/  dsk
    /neo/hawk/src/std
  %+  welp  dsk
  /[p.post]/[q.post]
::
++  apex
  ^-  manx
  ;div.wf.hf.p3.hoon.fc.g2
    ;+  imports
    ;+  contents
    ;+  style
  ==
++  imports
  ;div.frw.g2
    ;*
    %+  turn  pro.file
    |=  =pro:ford:neo
    ^-  manx
    ;a.p2.br1.b1.hover.s-1
      =href  (spud (post-href %pro stud.pro))
      =hx-target  "closest .hawk"
      =hx-swap  "innerHTML"
      ; {<stud.pro>}
    ==
  ==
++  error
  ;details.error-parent.wf.br1.bd1(open "")
    =style  "max-height: 220px;"
    ;summary.p2.br1.b-3.f-3: error
    ;div.p3
      ;div.error.empty;
    ==
  ==
++  contents
  ;form.fc.g2.wf.relative.grow.scroll-y
    =hx-put  "{(en-tape:pith:neo (welp /neo/hawk here.bowl))}?stud=hoon"
    =hx-trigger  "click from:find button, keydown[metaKey&&key=='Enter']"
    =hx-target  "closest .hoon"
    =hx-target-error  "find .error"
    =hx-swap  "morph"
    ;div.relative.grow.fc
      ;div.wfc.z1.absolute
        =style  "top: 15px; right: 15px;"
        ;button.p2.br2.b1.bd1.hover.loader
          ;span.loaded.fr.ac.g1
            ;span.bold: save
            ;span.f2.s-2: cmd+enter
          ==
          ;span.loading: ...
        ==
      ==
      ;textarea.p2.bd1.br1.scroll-x.pre.mono.wf.grow
        =style  "outline:none;"
        =autocomplete  "off"
        =rows  "1"
        =spellcheck  "false"
        =name  "text"
        =oninput  "this.setAttribute('value', this.value);"
        =value  (trip hon)
        ;*
        %+  turn  src
        |=  lin=@t
        ;/  "{(trip lin)}\0a"
      ==
    ==
    ;+  error
  ==
++  style
  ;style
    ;+  ;/  %-  trip
    '''
    details.error-parent:has(.error:empty) {
      display: none;
    }
    '''
  ==
--
