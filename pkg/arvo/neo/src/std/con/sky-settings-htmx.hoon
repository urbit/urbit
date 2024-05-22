/@  sky-settings
:-  [%sky-settings %htmx]
|=  settings=sky-settings
|=  =bowl:neo
^-  manx
|^
  ;div.wf.p-page
    =label  "Settings"
    ;form.fc.g5.mw-page.ma
      =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=sky-settings"
      =hx-swap  "none"
      ;+  script
      ;+  typography
      ;+  (colors "light")
      ;+  (colors "dark")
      ;+  style
      ;div.frw.g2
        ;button.p3.br1.b1.hover
          =onclick  "document.body.removeAttribute('style')"
          =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}"
          =hx-target  "closest .hawk"
          =hx-select  ".hawk"
          ;span: Reset
        ==
        ;button.p3.br1.b1.hover.loader
          ;span.loaded: Save
          ;span.loading: ---
        ==
      ==
    ==
  ==
++  typography
  ;div.fc.js.g2.ja.p2
    ;label.fc.g1.mt1
      ;span.s-1: Main font
      ;select.br1.bd1.p2
        =oninput  "setCss('font', this.value); $(this).attr('value', this.value);"
        =value  (trip (~(gut by settings) 'font' ''))
        =var  "font"
        ;*
        %+  turn
        :~
          "Urbit Sans"
          "Montserrat"
          "Verdana"
          "Gill Sans"
          "Helvetica Neue"
          "Open Sans"
          "Roboto"
          "Arial"
          "Futura"
          "PT Sans"
        ==
        |=  =tape
        ;option(value tape): {tape}
      ==
    ==
    ;label.fc.g1
      ;span.s-1: Base size
      ;input.br1.border
        =oninput  "setCss('1in', this.value+'px'); $(this).attr('value', this.value);"
        =value  (snip (snip (trip (~(gut by settings) '1in' ''))))
        =var  "1in"
        =type  "range"
        =min  "2"
        =max  "10"
        =step  "0.1"
        ;
      ==
    ==
    ;label.fc.g1
      ;span.s-1: Monospace font
      ;select.br1.bd1.p2
        =oninput  "setCss('font-mono', `'$\{this.value}'`); $(this).attr('value', this.value);"
        =value  (trip (~(gut by settings) 'font-mono' ''))
        =var  "font-mono"
        ;*
        %+  turn
        :~
          "Andale Mono"
          "Urbit Mono"
          "Courier New"
          "Monaco"
          "Spot Mono"
        ==
        |=  =tape
        ;option(value tape): {tape}
      ==
    ==
    ;label.fc.g1
      ;span.s-1: Mono scale
      ;input.br1.border
        =oninput  "setCss('mono-scale', this.value); $(this).attr('value', this.value);"
        =value  (trip (~(gut by settings) 'mono-scale' ''))
        =var  "mono-scale"
        =type  "range"
        =min  "0.5"
        =max  "1.2"
        =step  "0.05"
        ;
      ==
    ==
    ;label.fc.g1
      ;span.s-1: Letter spacing
      ;input.br1.border
        =oninput  "setCss('letter-spacing', this.value+'px'); $(this).attr('value', this.value);"
        =value  (snip (snip (trip (~(gut by settings) 'letter-spacing' ''))))
        =var  "letter-spacing"
        =type  "range"
        =min  "-1"
        =max  "3"
        =step  "0.001"
        ;
      ==
    ==
    ;label.fc.g1
      ;span.s-1: Line Height
      ;input.br1.border
        =oninput  "setCss('line-height', this.value); $(this).attr('value', this.value);"
        =value  (trip (~(gut by settings) 'line-height' ''))
        =var  "line-height"
        =type  "range"
        =min  "1"
        =max  "2"
        =step  "0.1"
        ;
      ==
    ==
  ==
++  color
  |=  [mode=tape var=tape]
  =/  val  (~(gut by settings) (crip "{mode}-{var}") '')
  ;label.fr.af.js.g1.mono
    ;span.fr.ac.je(style "width:5ch;"): {var}
    ;input.grow.fc.ac.jc
      =oninput  "setCss('{mode}-{var}', this.value); $(this).attr('value', this.value);"
      =var  "{mode}-{var}"
      =type  "color"
      =value  (trip val)
      ;
    ==
  ==
++  colors
  |=  [mode=tape]
  ;div
    =class  "sky-settings-{mode} frw g2 js p2"
    ;div.fc.g1.ac.jc
      ;*
      %+  turn
      :~
        "f-3"
        "f-2"
        "f-1"
        "f0"
        "f1"
        "f2"
        "f3"
        "f4"
      ==
      |=  =tape
      (color mode tape)
    ==
    ;div.fc.g1.ac.jc
      ;*
      %+  turn
      :~
        "b-3"
        "b-2"
        "b-1"
        "b0"
        "b1"
        "b2"
        "b3"
        "b4"
      ==
      |=  =tape
      (color mode tape)
    ==
  ==
++  script
  ;script
    ;+  ;/  %-  trip
    '''
    function setCss(name, val) {
      document.documentElement.style
        .setProperty('--'+name, val, 'important');
    }
    '''
  ==
++  style
  ;style
    ;+  ;/  %-  trip
    '''
    .sky-settings-dark {
      display: none !important;
    }
    .sky-settings-light {
      display: flex !important;
    }
    @media (prefers-color-scheme: dark) {
      .sky-settings-dark {
        display: flex !important;
      }
      .sky-settings-light {
        display: none !important;
      }
    }
    '''
  ==
--
