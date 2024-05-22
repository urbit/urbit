/@  sky
/@  sky-settings
/-  _/feather-icons
:-  [%sky %htmx]
|=  =sky
|=  =bowl:neo
^-  manx
|^
  shell
::
++  map-to-css-tape
  |=  m=(map @t @t)
  ^-  tape
  %-  zing
  %+  turn  ~(tap by m)
  |=  [key=@t val=@t]
  """
  --{(trip key)}: {(trip val)};
  """
++  shell
  =/  settings
    ^-  (unit sky-settings)
    =/  s  (~(get by kids.bowl) /settings)
    ?~  s  ~
    :-  ~
    !<  sky-settings
    q.u.s
  ;a-i-r.wf.hf.b1.relative
    =id  "air"
    =hawks  "{<slots.sky>}"
    ;button.hover.f2.b1.fc.ac.jc.air-btn
      =slot  "button"
      =onclick  "$(this).closest('a-i-r').attr('closed', !$(this).parent().attr('closed'))"
      ;div.fc.ac.jc.bold.s2: ~
    ==
    ;style
      ;+  ;/  %-  trip
      '''
      .air-btn {
        position: relative;
        padding: 8px;
        margin: 8px;
        border-radius: 4px;
      }
      @media(max-width: 900px) {
        .air-btn {
          position: absolute;
          bottom: 25px;
          right: 25px;
          padding: 30px;
          width: 70px;
          height: 70px;
          z-index: 10;
          border-radius: 50px;
          border: 1px solid var(--f3);
        }
      }
      '''
    ==
    ;nav.wf.hf.p2.fc.g2
      =slot  "nav"
      ;hr.m0.wf.bd0.b3.o4(style "height: 1.5px;");
      ;+  new-tab
      ;*
        =<  p
        %^  spin  hawks.sky
              1
            |=  [[id=@da =pith] a=@]
          :_  +(a)
        =/  color  (trip ?:((lte a slots.sky) 'b2' 'b1'))
        ;div
          =class  "fr ac br1 {color}"
          ;button
            =class  "loader p2 tl br1 hover grow {color}"
            =hx-post  "/neo/hawk/sky?stud=sky-diff"
            =hx-target  "find .loading"
            =hx-swap  "outerHTML"
            =head  "maximize"
            =hawk-slot  "{<a>}"
            ;span.loaded: {(en-tape:pith:neo pith)}
            ;span.loading
              ;+  loading.feather-icons
            ==
          ==
          ;button
            =class  "loader p2 tl br1 hover {color}"
            =hx-post  "/neo/hawk/sky?stud=sky-diff"
            =hx-target  "find .loading"
            =hx-swap  "outerHTML"
            =head  "close"
            =hawk-slot  "{<a>}"
            ;span.loaded.f3
              ;+  close.feather-icons
            ==
            ;span.loading
              ;+  loading.feather-icons
            ==
          ==
        ==
    ==
    ;*
      =<  p
      %^  spin  (scag slots.sky hawks.sky)
            1
          |=  [[id=@da =pith] a=@]
        :_  +(a)
      =/  ext
        ?:  =(pith /)  ""
        (en-tape:pith:neo pith)
      ;div.wf.hf.br1
        =slot  "s{<a>}"
        =id  "hawk-part-{<id>}"
        ;div.wf.hf.fc.jc.ac.f2.s3
          =id  "hawk-slot-{<id>}"
          =morph-no-swap  ""
          =hx-get  "/neo/hawk{ext}?slot={<a>}&id={<id>}&no-save"
          =hx-trigger  "load once"
          =hx-target  "this"
          =hx-swap  "outerHTML"
          ;+  loading.feather-icons
        ==
      ==
    ;style
      ;+  ;/
      ?~  settings
        ""
      """
      html \{
        {(map-to-css-tape u.settings)}
      }
      """
    ==
  ==
++  new-tab
  ;button.loader.b2.p2.tc.br1.hover.o6.wfc.s-1
    =hx-post  "/neo/hawk/sky?stud=sky-diff"
    =hx-target  "find .loading"
    =hx-swap  "outerHTML"
    =head  "new-tab"
    ;span.loaded.fr.ac.js.g2
      ;+  add.feather-icons
      ;span: New tab
    ==
    ;span.loading
      ;+  loading:feather-icons
    ==
  ==
++  shell2
  =/  settings
    ^-  (unit sky-settings)
    =/  s  (~(get by kids.bowl) /settings)
    ?~  s  ~
    :-  ~
    !<  sky-settings
    q.u.s
  ;s-k-y.wf.hf(open "", hawks "{<slots.sky>}")
    ;*
    =<  p
    %^  spin  hawks.sky
          1
        |=  [[id=@da =pith] a=@]
      :_  +(a)
    ;ha-wk
      =slot  "s{<a>}"
      =here  (en-tape:pith:neo pith)
      ;div
        =hx-get  "/neo/hawk{(en-tape:pith:neo pith)}"
        =hx-trigger  "load"
        =hx-target  "this"
        =hx-swap  "outerHTML"
        ;div.wf.hf.fc.jc.ac.f2.s3
          ;+  loading.feather-icons
        ==
      ==
    ==
    ;style
      ;+  ;/
      ?~  settings
        ""
      """
      html \{
        {(map-to-css-tape u.settings)}
      }
      """
    ==
  ==
--
