/@  sandbox
:-  [%sandbox %htmx]
|=  =sandbox
|=  =bowl:neo
|^
  ;div.wf.hf.relative
    ;div.p3.fc.g5.ma.mw-page
      ;+  form
      ;+  shrubs
    ==
  ==
::
++  form
  ;form.fc.g2
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=sandbox-diff"
    =hx-swap  "outerHTML"
    =hx-target  "find .loading"
    =head  "make"
    ;input.p2.br2.border
      =oninput  "$(this).attr('value', this.value)"
      =autocomplete  "off"
      =name  "name"
      =type  "text"
      =placeholder  "name"
      ;
    ==
    ;select.f1.b0
      =oninput  "$(this).attr('value', this.value)"
      =autocomplete  "off"
      =value  (trip %accel)
      =name  "stud"
      ;*  %+  turn
            ^-  (list @tas)
            :~  %accel 
                %circle 
                %diary
                %iframe
                %task
                %txt
                %sail
                %sandbox
            ==
          |=  t=@tas
          ^-  manx
          ;option  
            =value  (trip t)
            ; {(trip t)}
          ==
      ;
    ==
    ;button.loader.p2.p1.hover.br1.b1
      ;span.loaded: Create
      ;span.loading: ...
    ==
  ==
::
++  shrubs
  ;div.fc.g2
    ;*
    %+  turn
      ~(tap by kids.bowl)
    |=  [=pith =stud:neo =vase]
    =/  =path  (pout (welp here.bowl pith))
    =/  label
      ?@  stud
        (trip stud)
      (trip mark.stud)
    ;a.b1.br2.hover.p3.s1.border-2.fr.ac.jb.loader
      =hx-swap  "closest ha-wk"
      =href  (trip (spat ['neo' 'hawk' path]))
      ;span.loaded: {(trip (snag 0 (pout pith)))}
      ;span.loading: ...
      ;span.s-1.f2.loaded: {(weld "%" label)}
    ==
  ==
--

