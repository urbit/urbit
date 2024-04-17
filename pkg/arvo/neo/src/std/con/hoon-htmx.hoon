/@  htmx
:-  [%hoon %htmx]
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
  ;div.scroll-x.p3
    ;+  imports
    ;+  contents
  ==
++  imports
  ;div.p2
    ;div.p2.border.br1.fc.g2
      ;h4.bold: Protocols
      ;div.frw.g2
        ;*
        %+  turn  pro.file
        |=  =pro:ford:neo
        ^-  manx
        ;a.p2.br1.b1.hover
          =hx-get  (spud (post-href %pro stud.pro))
          =hx-target  "closest ha-wk"
          =hx-swap  "innerHTML"
          ; {<stud.pro>}
        ==
      ==
    ==
  ==
++  contents
  ;form.fc.g2.wf.relative.p2
    =hx-put  "{(en-tape:pith:neo (welp /neo/hawk here.bowl))}?stud=hoon"
    ;div.p2.wf.z1
      =style  "position: sticky; top: 0; right: 0;"
      ;button.p2.br1.b1.hover.loader.wf
        ;span.loaded: save
        ;span.loading: ...
      ==
    ==
    ;textarea.p2.border.br1.ma.scroll-x.pre.mono.wf
      =style  "max-width: 650px;"
      =autocomplete  "off"
      =rows  "4"
      =name  "text"
      =oninput  "this.setAttribute('value', this.value); this.rows = this.value.split('\\n')"
      ;*
      %+  turn  src
      |=  lin=@t
      ;/  "{(trip lin)}\0a"
    ==
  ==
--
