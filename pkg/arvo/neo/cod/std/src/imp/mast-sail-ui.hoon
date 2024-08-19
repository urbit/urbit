/@  ui-event
/@  sail
/-  lib-sail=sail
/-  feather-icons
/*  feather
^-  kook:neo
=<
|%
++  state  pro/%manx
++  poke   (sy %ui-event %rely ~)
++  kids
  *kids:neo
++  deps
  ^-  deps:neo
  %-  my
  :~  :-  %src
      :-  req=&
      :-  [pro/%sail (sy %sail ~)]
      ~
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    manx/!>((render (get-sail bowl) p:(~(got by deps.bowl) %src)))
  ::
  ++  poke
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  ~|(bad-stud/stud !!)
    ::
        %ui-event
      =/  event  !<(ui-event vase)
      =/  here=pith:neo  p:(~(got by deps.bowl) %src)
      :_  pail
      :~  
        :-  here
        :+  %poke 
        %sail
        !>  
        =+  sail=(get-sail bowl)
        ?+  path.event  ~|(missing-event-handler-for/path.event !!)
            [%input %new-classes ~]
          =.  class.sail  (~(got by data.event) '/target/value')
          sail
            [%input %code ~]
          =.  code.sail  (~(got by data.event) '/target/value')
          =.  result.sail  `(render-udon:lib-sail (~(got by data.event) '/target/value'))
          sail
        ==
      ==
        %rely
      :-  ~
      manx/!>((render (get-sail bowl) p:(~(got by deps.bowl) %src)))
    ==
  --
--
::
|%
::
++  render
  |_  [=sail =pith:neo]
  ++  $
    ^-  manx
    ;html
      ;head
        ;meta(charset "utf-8");
        ;style: {(trip feather)}
      ==
      ;body
        ;div.fc.relative.hf.scroll-hidden
          ;+  controls
          ;div.fr.js.as.scroll-y.hf
            =id  "tabs-{id}"
            ;+  editor
            ;+  viewer
          ==
        ==
      ==
    ==
  ++  id
  ^-  tape
  %-  zing
  %+  turn  (pout (slag 1 pith))
  |=  smeg=@ta
  %+  weld  "--"
  (trip smeg)
  ::
  ++  controls
  ^-  manx
  ;div.p2.fr.jc.ac.g3.sticky.wf
  =style  "top:0; left: 0;"
    ;button.p-1.br1.b1.hover
      =type  "button"
      =onclick
        """
        document.getElementById('tabs-{id}').children[0].classList.add('hidden');
        document.getElementById('tabs-{id}').children[1].classList.add('hidden');
        document.getElementById('editor-{id}').classList.remove('hidden');
        this.nextSibling.classList.remove('toggled');
        this.nextSibling.nextSibling.classList.remove('toggled');
        this.classList.add('toggled');
        """
      ; edit
    ==
    ;button.p-1.br1.b1.hover
      =type  "button"
      =onclick
        """
        document.getElementById('tabs-{id}').children[0].classList.add('hidden');
        document.getElementById('tabs-{id}').children[1].classList.add('hidden');
        document.getElementById('viewer-{id}').classList.remove('hidden');
        document.getElementById('editor-{id}').classList.remove('hidden');
        this.previousSibling.classList.remove('toggled');
        this.nextSibling.classList.remove('toggled');
        this.classList.add('toggled');
        """
      ; both
    ==
    ;button.p-1.br1.b1.hover.toggled
      =type  "button"
      =onclick
        """
        document.getElementById('tabs-{id}').children[0].classList.add('hidden');
        document.getElementById('tabs-{id}').children[0].classList.add('hidden');
        document.getElementById('viewer-{id}').classList.remove('hidden');
        this.previousSibling.classList.remove('toggled');
        this.previousSibling.previousSibling.classList.remove('toggled');
        this.classList.add('toggled');
        """
      ; view
    ==
  ==
  ::
  ++  editor
  =/  key  (en-tape:pith:neo pith)
  ^-  manx
  ;div.fc.p1.g1.hidden.grow.basis-half.scroll-y.relative
    =id  "editor-{id}"
    =style  "min-width: 300px; height: 100%;"
    ;input.p2.mono.bd1.br1.b0
      =placeholder   "prose p3"
      =type          "text"
      =autocomplete  "off"
      =value         (trip class.sail)
      =data-key      key
      =return        "/target/data-key /target/value"
      =event         "/input/new-classes"
      ;
    ==
    ;textarea.p2.pre.mono.scroll-x.grow.bd1.m0.br1.b0
      =placeholder  "# new document"
      =oninput  "this.setAttribute('value', this.value);"
      =spellcheck   "false"
      =value        ""
      =data-key     key
      =return       "/target/data-key /target/value"
      =event        "/input/code"
      ; {(trip code.sail)}
    ==
    ;div.absolute
      =style  "top: 14px; right: 14px;"
      ;div.loader
        ;span.loaded(style "opacity: 0;"): ---
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ==
::
  ++  error
  |=  =tang
  ;div.fc.g3.p3.s0
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
::
  ++  viewer
    ;main.grow.basis-half.scroll-x.scroll-y.br1
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
--
::
++  get-sail
  |=  =bowl:neo
  ^-  sail
  =/  =lore:neo  q:(~(got by deps.bowl) %src)
  !<(sail q.pail:(need fil.lore))
::
--