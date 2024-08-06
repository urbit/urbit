/@  home
/-  feather-icons
:-  [%home %$ %htmx]
|=  =home
|=  =bowl:neo
|^
  ;div.wf.hf.relative
    ;div.fc.g5.ma.mw-page.p-page
      :: ;+  menu
      ;+  apps
    ==
  ==
++  id
  ^-  tape
  %-  zing
  %+  turn  (pout (slag 1 here.bowl))
  |=  smeg=@ta
  %+  weld  "--"
  (trip smeg)
++  menu
  ;div.p4.sticky.z2
    =style  "top: 20px; left: 0;"
    ;div.fr.g2.relative
      ;input.p2.grow.br2.bd1.b0
        =onchange  "alert('not yet implemented')"
        =type  "text"
        =placeholder  "Search..."
        ;
      ==
      ;a.p2.b1.hover.loader.br2.bd1.fr.jc.ac.f3
        =href  "/neo/hawk/{<our.bowl>}/sky/settings"
        ;span.loaded
          ;+  settings.feather-icons
        ==
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
      ;div.absolute.hidden
        =style  "top: 0px; right: 0px;"
        ;div.loader.wfc.fc.p2.f2.s-2.mono.br2
          =id  "indicator-{id}"
          ;span.loaded(style "opacity: 0"): ---
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
      ==
    ==
  ==
++  apps
  ;form.frw.g3.ac.jc.wfc.ma
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=home"
    =hx-swap  "none"
    =hx-indicator  "#indicator-{id}"
    ;*
    =/  apps
      %+  welp
        apps.home
      %-  skip
      :_  |=  =pith
          (gth (lent pith) 1)
      %~  tap  in
      %-  %~  dif  in
          %~  key  by
          %~  tar  of:neo
          kids.bowl
      `(set pith)`(silt apps.home)
    %+  murn  apps
    |=  =pith
    ^-  (unit manx)
    ?~  pith  ~
    =/  =path  (pout (welp here.bowl pith))
    :-  ~
    ;div.relative.br2
      =pith  (en-tape:pith:neo pith)
      ;a.b1.br2.block.fc.as.js.hover.p3.s1.border-2.loader
        =style  "width: 160px; height: 160px;"
        =hx-target  "closest .hawk"
        =hx-swap  "outerHTML"
        =hx-indicator  "this"
        =href  (trip (spat ['neo' 'hawk' path]))
        ;span.loaded: {(trip (snag 0 (pout:neo pith)))}
        ;span.loading.s2
          ;+  loading.feather-icons
        ==
      ==
      ;div.br2.b2.z1.f3.fc.wf.border-2
        =style  "position: absolute; bottom: 0; right: 0;"
        ;button.basis-full.tc.s-2.p1.hover.br2
          =type  "button"
          =onclick  "$(this).next().toggleClass('hidden')"
          ; • • •
        ==
        ;div.fr.hidden.g1.p1
          ;button.grow.fr.jc.b0.br2.p1
            =hx-trigger  "click"
            =onclick  "let t = $(this).closest('.relative'); t.parent().prepend(t);"
            ;+  chevron-first.feather-icons
          ==
          ;button.grow.fr.jc.b0.br2.p1
            =onclick  "let t = $(this).closest('.relative'); t.insertBefore(t.prev());"
            =hx-trigger  "click"
            ;+  chevron-left.feather-icons
          ==
          ;button.grow.fr.jc.b0.br2.p1
            =hx-trigger  "click"
            =onclick  "let t = $(this).closest('.relative'); t.insertAfter(t.next());"
            ;+  chevron-right.feather-icons
          ==
          ;button.grow.fr.jc.b0.br2.p1
            =onclick  "let t = $(this).closest('.relative'); t.parent().append(t);"
            ;+  chevron-last.feather-icons
          ==
        ==
      ==
    ==
  ==
::
--
