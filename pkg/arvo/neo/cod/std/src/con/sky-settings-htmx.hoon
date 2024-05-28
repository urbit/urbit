/@  sky-settings
/-  feather-icons
:-  [%sky-settings %$ %htmx]
|=  settings=sky-settings
|=  =bowl:neo
^-  manx
|^
  ;div.wf.hf.relative
    ;form.fc.g5.mw-page.ma.p-page
      =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=sky-settings"
      =hx-swap  "outerHTML"
      =hx-target  "this"
      =hx-select  "form"
      ;+  header
      ;+  script
      ;+  background-image
      ;+  spacing
      ;+  typography
      ;+  (colors "light")
      ;+  (colors "dark")
      ;+  style
    ==
  ==
++  curr
  |=  var=cord
  (trip (~(gut by settings) var ''))
++  carr
  |=  [var=cord head=@ud tail=@ud]
  =/  val  (trip (~(gut by settings) var ''))
  =/  wit  (add head tail)
  ?.  (gth (lent val) wit)  val
  (swag [head (sub (lent val) wit)] val)
++  selected
  |=  [f=? =manx]
  %=  manx
    a.g
      ?.  f  a.g.manx
      [[%selected ""] a.g.manx]
  ==
++  home-button
  ;a.p2.br1.bd1.b1.hover.loader.wfc.block
    =href  "/neo/hawk/home"
    =hx-swap  "innerHTML"
    =hx-target  "closest .hawk"
    =hx-select  ".hawk"
    ;span.loaded: home
    ;span.loading
      ;+  loading.feather-icons
    ==
  ==
++  header
  ;header.sticky.z1.b0.fc.g1
    =style  "top:0px; left: 0;"
    ;h1.bold.s2.p1: settings
    ;div.frw.g2.ac
      ;+  home-button
      ;div.grow;
      ;button.p2.bd1.br1.b1.hover
        =type  "button"
        =onclick  "document.documentElement.removeAttribute('style')"
        =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}"
        =hx-target  "closest .hawk"
        =hx-select  ".hawk"
        ;span: reset
      ==
      ;button.p2.bd1.br1.b1.hover.loader
        ;span.loaded: save
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ==
++  background-image
  ;div.fc.js.g2.ja.prose
    ;h2: background
    ;label.fc.g1
      ;span.s-1: image
      ;input.hidden
        =var  "sky-bg-url"
        =value  (curr 'sky-bg-url')
        ;
      ==
      ;input.bd1.border.p2.br1
        =oninput  "setCss('sky-bg-url', encase(this.value, 'url(', ')')); $(this).prev().attr('value', encase(this.value, 'url(', ')'));"
        =autocomplete  "off"
        =placeholder  "https://images.website/waterfall.jpg"
        =value  (carr 'sky-bg-url' 4 1)
        =type  "text"
        ;
      ==
    ==
    ;label.fr.g2.ac
      ;span.s-1: position
      ;select.br1.bd1.p2
        =oninput  "setCss('sky-bg-size', this.value); $(this).attr('value', this.value);"
        =value  (curr 'sky-bg-size')
        =var  "sky-bg-size"
        ;*
        %+  turn
        :~
          "initial"
          "contain"
          "cover"
        ==
        |=  =tape
        %+  selected  =(tape (curr 'sky-bg-size'))
        ;option(value tape): {tape}
      ==
    ==
    ;label.fc.g1
      ;span.s-1: opacity
      ;input.bd1.border.p2.br1
        =oninput  "setCss('sky-opacity', this.value); $(this).attr('value', this.value);"
        =value  (trip (~(gut by settings) 'sky-opacity' '1'))
        =var  "sky-opacity"
        =type  "range"
        =min  "0.6"
        =max  "1.0"
        =step  "0.01"
        ;
      ==
    ==
  ==
++  spacing
  ;div.fc.g2.prose
    ;h2: spacing
    ;label.fc.g1
      ;span.s-1: outer gap
      ;input.bd1.border.p2.br1
        =oninput  "setCss('sky-outer-gap', `$\{this.value}px`); $(this).attr('value', this.value);"
        =value  (curr 'sky-outer-gap')
        =var  "sky-outer-gap"
        =type  "range"
        =min  "0"
        =max  "30"
        =step  "1"
        ;
      ==
    ==
    ;label.fc.g1
      ;span.s-1: inner gap
      ;input.bd1.border.p2.br1
        =oninput  "setCss('sky-inner-gap', `$\{this.value}px`); $(this).attr('value', this.value);"
        =value  (curr 'sky-inner-gap')
        =var  "sky-inner-gap"
        =type  "range"
        =min  "0"
        =max  "30"
        =step  "1"
        ;
      ==
    ==
  ==
++  typography
  ;div.fc.js.g2.ja.prose
    ;h2: typography
    ;label.fc.g1.mt1
      ;span.s-1: main font
      ;select.br1.bd1.p2
        =oninput  "setCss('font', this.value); $(this).attr('value', this.value);"
        =value  (curr 'font')
        =var  "font"
        ;*
        %+  turn
        :~
          "Urbit Sans"
          "DM Sans"
          "Lato"
          "Georgia"
          "Optima"
          "DejaVu Sans"
          "Palatino"
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
        %+  selected  =(tape (curr 'font'))
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
        =value  (curr 'font-mono')
        =var  "font-mono"
        ;*
        %+  turn
        :~
          "Andale Mono"
          "Urbit Mono"
          "Courier New"
          "Courier"
          "Monaco"
          "Spot Mono"
        ==
        |=  =tape
        %+  selected  =(tape (curr 'font-mono'))
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
    =class  "sky-settings-{mode} frw g2 js prose"
    ;h2.basis-full: colors
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
    function encase(val, head, tail) {
      if (!val.length) {
        return '';
      }
      return `${head}${val}${tail}`;
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
