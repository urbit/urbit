/@  sail
/-  feather-icons
:-  [%sail %$ %htmx]
|=  =sail
|=  =bowl:neo
|^
  ;div.fc.relative.hf.scroll-hidden
    ;+  controls
    ;div.frw.js.as.scroll-y.hf
      =id  "tabs-{id}"
      ;+  editor
      ;+  viewer
    ==
  ==
++  id
  ^-  tape
  %-  zing
  %+  turn  (pout (slag 1 here.bowl))
  |=  smeg=@ta
  %+  weld  "--"
  (trip smeg)
++  controls
  ;div.p2.frw.jc.ac.g3.sticky.wf
    =style  "top:0; left: 0;"
    ;button.p-1.br1.b1.hover
      =type  "button"
      =onclick
        """
        $('#tabs-{id}').children().addClass('hidden');
        $('#editor-{id}').removeClass('hidden');
        $(this).siblings().removeClass('toggled');
        $(this).addClass('toggled');
        """
      ; edit
    ==
    ;button.p-1.br1.b1.hover
      =type  "button"
      =onclick
        """
        $('#tabs-{id}').children().addClass('hidden');
        $('#viewer-{id}').removeClass('hidden');
        $('#editor-{id}').removeClass('hidden');
        $(this).siblings().removeClass('toggled');
        $(this).addClass('toggled');
        """
      ; both
    ==
    ;button.p-1.br1.b1.hover.toggled
      =type  "button"
      =onclick
        """
        $('#tabs-{id}').children().addClass('hidden');
        $('#viewer-{id}').removeClass('hidden');
        $(this).siblings().removeClass('toggled');
        $(this).addClass('toggled');
        """
        ;
      ; view
    ==
  ==
++  editor
  ;form.fc.p1.g1.hidden.grow.basis-half.scroll-y.relative
    =id  "editor-{id}"
    =style  "min-width: 300px; height: 100%;"
    =hx-post  "/hawk{(en-tape:pith:neo here.bowl)}?stud=sail"
    =hx-swap  "innerHTML"
    =hx-select  "main > div"
    =hx-target  "#viewer-{id}"
    =hx-trigger  "input changed delay:0.4s from:find textarea, input changed delay:0.4s from:find input"
    ;input.p2.mono.bd1.br1
      =name  "classes"
      =placeholder  "prose p3"
      =type  "text"
      =autocomplete  "off"
      =value  (trip class.sail)
      =oninput  "$(this).attr('value', this.value);"
      ;
    ==
    ;textarea.p2.pre.mono.scroll-x.grow.bd1.m0.br1
      =name  "code"
      =oninput  "this.setAttribute('value', this.value);"
      =spellcheck  "false"
      =value  ""
      =placeholder  "# new document"
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
